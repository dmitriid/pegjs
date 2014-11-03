%%%-----------------------------------------------------------------------------
%%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%%% @doc 
%%%
%%% Generates bytecode.
%%%
%%% Instructions
%%% ============
%%%
%%% Stack Manipulation
%%% ------------------
%%%
%%%  [0] PUSH c
%%%
%%%        stack.push(consts[c]);
%%%
%%% [26] PUSH_UNDEFINED
%%%
%%%        stack.push(undefined);
%%%
%%% [27] PUSH_NULL
%%%
%%%        stack.push(null);
%%%
%%% [28] PUSH_FAILED
%%%
%%%        stack.push(FAILED);
%%%
%%% [29] PUSH_EMPTY_ARRAY
%%%
%%%        stack.push([]);
%%%
%%%  [1] PUSH_CURR_POS
%%%
%%%        stack.push(currPos);
%%%
%%%  [2] POP
%%%
%%%        stack.pop();
%%%
%%%  [3] POP_CURR_POS
%%%
%%%        currPos = stack.pop();
%%%
%%%  [4] POP_N n
%%%
%%%        stack.pop(n);
%%%
%%%  [5] NIP
%%%
%%%        value = stack.pop();
%%%        stack.pop();
%%%        stack.push(value);
%%%
%%%  [6] APPEND
%%%
%%%        value = stack.pop();
%%%        array = stack.pop();
%%%        array.push(value);
%%%        stack.push(array);
%%%
%%%  [7] WRAP n
%%%
%%%        stack.push(stack.pop(n));
%%%
%%%  [8] TEXT
%%%
%%%        stack.push(input.substring(stack.pop(), currPos));
%%%
%%% Conditions and Loops
%%% --------------------
%%%
%%%  [9] IF t, f
%%%
%%%        if (stack.top()) {
%%%          interpret(ip + 3, ip + 3 + t);
%%%        } else {
%%%          interpret(ip + 3 + t, ip + 3 + t + f);
%%%        }
%%%
%%% [10] IF_ERROR t, f
%%%
%%%        if (stack.top() === FAILED) {
%%%          interpret(ip + 3, ip + 3 + t);
%%%        } else {
%%%          interpret(ip + 3 + t, ip + 3 + t + f);
%%%        }
%%%
%%% [11] IF_NOT_ERROR t, f
%%%
%%%        if (stack.top() !== FAILED) {
%%%          interpret(ip + 3, ip + 3 + t);
%%%        } else {
%%%          interpret(ip + 3 + t, ip + 3 + t + f);
%%%        }
%%%
%%% [12] WHILE_NOT_ERROR b
%%%
%%%        while(stack.top() !== FAILED) {
%%%          interpret(ip + 2, ip + 2 + b);
%%%        }
%%%
%%% Matching
%%% --------
%%%
%%% [13] MATCH_ANY a, f, ...
%%%
%%%        if (input.length > currPos) {
%%%          interpret(ip + 3, ip + 3 + a);
%%%        } else {
%%%          interpret(ip + 3 + a, ip + 3 + a + f);
%%%        }
%%%
%%% [14] MATCH_STRING s, a, f, ...
%%%
%%%        if (input.substr(currPos, consts[s].length) === consts[s]) {
%%%          interpret(ip + 4, ip + 4 + a);
%%%        } else {
%%%          interpret(ip + 4 + a, ip + 4 + a + f);
%%%        }
%%%
%%% [15] MATCH_STRING_IC s, a, f, ...
%%%
%%%        if (input.substr(currPos, consts[s].length).toLowerCase() === consts[s]) {
%%%          interpret(ip + 4, ip + 4 + a);
%%%        } else {
%%%          interpret(ip + 4 + a, ip + 4 + a + f);
%%%        }
%%%
%%% [16] MATCH_REGEXP r, a, f, ...
%%%
%%%        if (consts[r].test(input.charAt(currPos))) {
%%%          interpret(ip + 4, ip + 4 + a);
%%%        } else {
%%%          interpret(ip + 4 + a, ip + 4 + a + f);
%%%        }
%%%
%%% [17] ACCEPT_N n
%%%
%%%        stack.push(input.substring(currPos, n));
%%%        currPos += n;
%%%
%%% [18] ACCEPT_STRING s
%%%
%%%        stack.push(consts[s]);
%%%        currPos += consts[s].length;
%%%
%%% [19] FAIL e
%%%
%%%        stack.push(FAILED);
%%%        fail(consts[e]);
%%%
%%% Calls
%%% -----
%%%
%%% [20] REPORT_SAVED_POS p
%%%
%%%        reportedPos = stack[p];
%%%
%%% [21] REPORT_CURR_POS
%%%
%%%        reportedPos = currPos;
%%%
%%% [22] CALL f, n, pc, p1, p2, ..., pN
%%%
%%%        value = consts[f](stack[p1], ..., stack[pN]);
%%%        stack.pop(n);
%%%        stack.push(value);
%%%
%%% Rules
%%% -----
%%%
%%% [23] RULE r
%%%
%%%        stack.push(parseRule(r));
%%%
%%% Failure Reporting
%%% -----------------
%%%
%%% [24] SILENT_FAILS_ON
%%%
%%%        silentFails++;
%%%
%%% [25] SILENT_FAILS_OFF
%%%
%%%        silentFails--;
%%%

%%%-----------------------------------------------------------------------------
-module(pegjs2_bytecode).

%%_* Exports ===================================================================
-export([ generate/1
        ]).

%%_* Includes ==================================================================
-include("pegjs2.hrl").
-include("pegjs2_opcodes.hrl").

-record(context, { sp     = -1 %% stack pointer
                 , env    = [] %% mapping of label names to stack positions
                 , action = none %% action nodes pass themselves to children here
                 }).

-define(CONSTS, pegjs2_consts).
-define(COUNTER, '__pegjs$counter__').
-define(CONTEXT, pegjs2_context).

%%_* API =======================================================================
-spec generate(#analysis{}) -> any().
generate(#analysis{grammar = Grammar}) ->
  init_global_tables(),
  Bytecode = lists:flatten(generate(Grammar, #context{})),
  Consts = lists:sort( fun({_, I1}, {_, I2}) -> I1 < I2 end
                     , ets:tab2list(?CONSTS)),
  teardown_global_tables(),
  {Bytecode, Consts, Grammar}.


%%_* Internal ==================================================================
-spec generate(#entry{}, #context{}) -> any().
generate(#entry{type = <<"grammar">>, rules = Rules}, Context) ->
  Bytecode = generate(Rules, Context),
  lists:reverse(Bytecode);
generate(List, Context) when is_list(List) ->
  lists:foldl( fun(E, Bytecode) ->
                 [generate(E, Context) | Bytecode]
               end
             , []
             , List);
generate(#entry{ type       = <<"rule">>
               , expression = Expression
               }, Context) ->
  generate(Expression, Context);
generate(#entry{ type       = <<"named">>
               , expression = Expression
               , name       = Name
               }, Context) ->
  NameIndex = add_const(#entry{ type = <<"other">>
                              , description = Name
                              }),
  [ ?SILENT_FAILS_ON
  , generate(Expression, Context)
  , ?SILENT_FAILS_OFF
  , build_condition(?IF_ERROR, [?FAIL, NameIndex], [])
  ];
generate(#entry{ type         = <<"choice">>
               , alternatives = Alternatives
               }, Context) ->
  build_alternatives(Alternatives, Context);
generate(#entry{ type        = <<"action">>
               , expression  = Expression0
               , code        = Code
               } = Entry
        , #context{sp = Sp}) ->
  Expression = case Expression0 of
                 [E] -> E;
                 _   -> Expression0
               end,
  EmitCall = case Expression of
               #entry{type = <<"sequence">>} -> false;
               #entry{elements = Elements} ->
                 (is_list(Elements) andalso length(Elements) == 0) orelse
                 Elements == undefined;
               _ -> false
             end,
  ets:update_counter(?CONTEXT, ?COUNTER, 1),
  ExpressionCode = generate( Expression
                           , #context{ sp = case EmitCall of
                                              true -> Sp + 1;
                                              false -> Sp
                                            end
                                     , env = []
                                     , action = Entry
                                     }),
  FunctionIndex = add_function_const(Code),
  case EmitCall of
    true ->
      [ ?PUSH_CURR_POS
      , ExpressionCode
      , build_condition( ?IF_NOT_ERROR
                       , [ [?REPORT_SAVED_POS, 1]
                         , build_call(FunctionIndex, 1, Sp + 2)
                         ]
                       , []
                       )
      , ?NIP
      ];
    false ->
      ExpressionCode
  end;
generate(#entry{ type     = <<"sequence">>
               , elements = Elements
               } = Entry
        , #context{sp = Sp} = Context) ->
  [ ?PUSH_CURR_POS
  , build_elements_code(Entry, Elements, Context#context{sp = Sp + 1})
  ];
generate(#entry{ type       = <<"labeled">>
               , label      = Label
               , expression = Expression
               }
        , #context{sp = Sp}) ->
  [{_, Counter}] = ets:lookup(?CONTEXT, ?COUNTER),
  ets:insert(?CONTEXT, {{Label, Counter}, Sp + 1}),
  generate(Expression, #context{sp = Sp, env = [], action = none});
generate(#entry{ type     = <<"text">>
               , expression = Expression
               }
        , #context{sp = Sp}) ->
  [ ?PUSH_CURR_POS
  , generate(Expression, #context{sp = Sp + 1, env = [], action = none})
  , build_condition(?IF_NOT_ERROR, [?POP, ?TEXT], ?NIP)
  ];
generate( #entry{ type = <<"simple_and">>
                , expression = Expression
                }
        , Context) ->
  build_simple_predicate(Expression, false, Context);
generate( #entry{ type = <<"simple_not">>
                , expression = Expression
                }
        , Context) ->
  build_simple_predicate(Expression, true, Context);
generate( #entry{ type = <<"optional">>
                , expression = Expression
                }
        , #context{sp = Sp}) ->
  [ generate(Expression, #context{sp = Sp})
  , build_condition( ?IF_ERROR
                   , [?POP, ?PUSH_NULL]
                   , [])
  ];
generate( #entry{ type = <<"zero_or_more">>
                , expression = Expression
                }
        , #context{sp = Sp}) ->
  ExpressionCode = generate(Expression, #context{sp = Sp + 1}),
  [ ?PUSH_EMPTY_ARRAY
  , ExpressionCode
  , build_append_loop(ExpressionCode)
  , ?POP
  ];
generate( #entry{ type = <<"one_or_more">>
                , expression = Expression
                }
        , #context{sp = Sp}) ->
  ExpressionCode = generate(Expression, #context{sp = Sp + 1}),
  [ ?PUSH_EMPTY_ARRAY
  , ExpressionCode
  , build_condition( ?IF_NOT_ERROR
                   , [ build_append_loop(ExpressionCode)
                     , ?POP
                     ]
                   , [?POP, ?POP, ?PUSH_FAILED]
                   )
  ];
generate( #entry{ type = <<"semantic_and">>
                , code = Code
                }
        , Context) ->
  build_semantic_predicate(Code, false, Context);
generate( #entry{ type = <<"semantic_not">>
                , code = Code
                }
        , Context) ->
  build_semantic_predicate(Code, true, Context);
generate( #entry{ type = <<"rule_ref">>
                , name = Name
                }
        , _Context) ->
  [?RULE, index_of_rule(Name)];
generate( #entry{ type = <<"literal">>
                , ignore_case = IgnoreCase
                , value = Value
                }
        , _Context) ->
  case Value of
    <<>> ->
      StringIndex = add_const(<<>>),
      [?PUSH, StringIndex];
    _ ->
      %% TODO: escape string?
      StringIndex = add_const(Value),
      ExpectedStringIndex = add_const(#entry{ type        = <<"literal">>
                                            , value       = Value
                                            , description = << "\\\""
                                                             , Value/binary
                                                             , "\\\"">>
                                            }),
      %% For case-sensitive strings the value must match the beginning of the
      %% remaining input exactly. As a result, we can use |ACCEPT_STRING| and
      %% save one |substr| call that would be needed if we used |ACCEPT_N|.
      build_condition( case IgnoreCase of
                         true -> [?MATCH_STRING_IC, StringIndex];
                         false -> [?MATCH_STRING, StringIndex]
                       end
                     , case IgnoreCase of
                         true -> [?ACCEPT_N, size(Value)];
                         false -> [?ACCEPT_STRING, StringIndex]
                       end
                     , [?FAIL, ExpectedStringIndex]
                     )
  end;
generate( #entry{ type = <<"class">>
                , parts = Parts
                , inverted = Inverted
                , raw_text = RawText
                }
        , _Context) ->
  Regexp = case {Parts, Inverted} of
             {[], true}  -> <<"^[\\S\\s]">>;
             {[], false} -> <<"^(?!)">>;
             {_, _} ->
               Ps = lists:map(fun([B]) when is_binary(B) ->
                                  escape_for_regexp_class(B);
                                 ([[B1, B2]]) ->
                                  << (escape_for_regexp_class(B1))/binary
                                   , "-"
                                   , (escape_for_regexp_class(B2))/binary
                                  >>
                              end, Parts),
               iolist_to_binary([ <<"^[">>
                                , case Inverted of true -> <<"^">>; false -> <<>> end
                                , Ps
                                , <<"]">>
                                ])
           end,
  RegexpIndex = add_const(Regexp),
  ExpectedIndex = add_const(#entry{ type        = <<"class">>
                                  , value       = RawText
                                  , description = RawText
                                  }),
  build_condition( [?MATCH_REGEXP, RegexpIndex]
                 , [?ACCEPT_N, 1]
                 , [?FAIL, ExpectedIndex]
                 );
generate(#entry{type = <<"any">>}, _Context) ->
  ExpectedIndex = add_const(#entry{ type        = <<"any">>
                                  , description = <<"any character">>
                                  }),

  build_condition( ?MATCH_ANY
                 , [?ACCEPT_N, 1]
                 , [?FAIL, ExpectedIndex
                   ]).

-spec add_const(term()) -> integer().
add_const(Value) ->
  case ets:lookup(?CONSTS, Value) of
    [{_, I}] -> I;
    []       ->
      I = ets:update_counter(?CONSTS, ?COUNTER, 1),
      ets:insert(?CONSTS, {Value, I}),
      I
  end.

-spec add_function_const(term()) -> integer().
add_function_const(Code) ->
  ParamNames = lookup_param_names(),
  add_const({function, ParamNames, Code}).

-spec lookup_param_names() -> list().
lookup_param_names() ->
  Params = lookup_params(),
  [K || {K, _} <- Params].

-spec lookup_params() -> list().
lookup_params() ->
  [{_, Counter}] = ets:lookup(?CONTEXT, ?COUNTER),
  Matcher = [{{{'$1', Counter}, '$2'}, [], [{{'$1', '$2'}}]}],
  Params = ets:select(?CONTEXT, Matcher),
  lists:keysort(2, Params).

-spec build_condition( CondCode::term()
                     , ThenCode::term()
                     , ElseCode::term()) -> list().
build_condition(CondCode, ThenCode, ElseCode) ->
  [ CondCode
  , length(flatten(ThenCode))
  , length(flatten(ElseCode))
  , ThenCode
  , ElseCode
  ].

-spec build_alternatives(list(), #context{}) -> list().
build_alternatives([H|T], #context{sp = Sp} = Context) ->
  [ generate(H, #context{sp = Sp})
  | case T of
      [] -> [];
      _ ->
        build_condition( ?IF_ERROR
                       , [ ?POP
                         , build_alternatives(T, Context)
                         ]
                       , []
                       )
    end
  ].

-spec build_call(integer(), integer(), integer()) -> list().
build_call(FunctionIndex, Delta, Sp) ->
  Env = lookup_params(),
  Params = lists:map(fun(P) -> Sp - P end, [V || {_, V} <- Env]),
  [ ?CALL
  , FunctionIndex
  , Delta
  , length(Params)
  ] ++ Params.


-spec build_elements_code(#entry{}, list(), #context{}) -> list().
build_elements_code( #entry{elements = Elements}
                   , []
                   , #context{action = none}) ->
  [ [?WRAP, length(Elements)]
  , ?NIP
  ];
build_elements_code( #entry{elements = Elements}
                   , []
                   , #context{action = Action, sp = Sp}) ->
  FunctionIndex = add_function_const(Action#entry.code),
  [ [?REPORT_SAVED_POS, length(Elements)]
  , build_call( FunctionIndex
              , length(Elements)
              , Sp)
  , ?NIP
  ];
build_elements_code( #entry{elements = NodeElements} = Entry
                   , [H|T]
                   , #context{sp = Sp} = Context) ->
  ProcessedCount = length(NodeElements) - length(T),
  [ generate(H, Context#context{action = none})
  , build_condition( ?IF_NOT_ERROR
                   , build_elements_code(Entry, T, Context#context{sp = Sp + 1})
                   , [ case ProcessedCount > 1 of
                         true -> [?POP_N, ProcessedCount];
                         false -> ?POP
                       end
                     , ?POP_CURR_POS
                     , ?PUSH_FAILED
                     ]
                   )
  ].

-spec build_simple_predicate(term(), boolean(), #context{}) -> list().
build_simple_predicate(Expression, Negative, #context{sp = Sp}) ->
  [ ?PUSH_CURR_POS
  , ?SILENT_FAILS_ON
  , generate(Expression, #context{sp = Sp + 1, env = [], action = none})
  , ?SILENT_FAILS_OFF
  , build_condition( [case Negative of true -> ?IF_ERROR; false -> ?IF_NOT_ERROR end]
                   , [ ?POP
                     , case Negative of true -> ?POP; false -> ?POP_CURR_POS end
                     , ?PUSH_UNDEFINED
                     ]
                   , [ ?POP
                     , case Negative of true -> ?POP_CURR_POS; false -> ?POP end
                     , ?PUSH_FAILED
                     ]
                   )
  ].

-spec build_semantic_predicate(list(), boolean(), #context{}) -> list().
build_semantic_predicate(Code, Negative, #context{sp = Sp}) ->
  FunctionIndex = add_function_const(Code),
  [ ?REPORT_CURR_POS
  , build_call(FunctionIndex, 0, Sp)
  , build_condition( ?IF
                   , [ ?POP
                     , case Negative of true -> ?PUSH_FAILED; false -> ?PUSH_UNDEFINED end
                     ]
                   , [ ?POP
                     , case Negative of true -> ?PUSH_UNDEFINED; false -> ?PUSH_FAILED end
                     ]
                   )
  ].

-spec build_append_loop(list()) -> list().
build_append_loop(ExpressionCode) ->
  build_loop(?WHILE_NOT_ERROR, [?APPEND, ExpressionCode]).

-spec build_loop(list(), list()) -> list().
build_loop(CondCode, BodyCode) ->
  [ CondCode
  , [length(flatten(BodyCode))]
  , BodyCode
  ].

%% TODO!! FIXME!!
-spec index_of_rule(binary()) -> integer().
index_of_rule(_Name) ->
  0.

%% TODO!! FIXME!!
-spec escape_for_regexp_class(binary()) -> binary().
escape_for_regexp_class(Binary) ->
  Binary.

-spec flatten(term()) -> term().
flatten(L) when is_list(L) ->
  lists:flatten(L);
flatten(Any) ->
  [Any].

-spec init_global_tables() -> ok.
init_global_tables() ->
  try ets:new(?CONSTS, [set, named_table])
  catch _:_ -> ets:delete_all_objects(?CONSTS)
  end,
  try ets:new(?CONTEXT, [set, named_table])
  catch _:_ -> ets:delete_all_objects(?CONTEXT)
  end,
  ets:insert(?CONSTS, {?COUNTER, -1}),
  ets:insert(?CONTEXT, {?COUNTER, -1}).

-spec teardown_global_tables() -> ok.
teardown_global_tables() ->
  ets:delete(?CONSTS),
  ets:delete(?CONTEXT).
