%%%-----------------------------------------------------------------------------
%%% @doc Analyze output of peg.js parser
%%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%%%
%%%-----------------------------------------------------------------------------
-module(pegjs_analyze).

%%_* Exports ===================================================================
-export([ analyze/1
        , file/1
        ]).

%%_* Includes ==================================================================
-include("pegjs.hrl").

%%_* API =======================================================================
-spec file(string()) -> {#grammar{}, #analysis{}} | {error, term()}.
file(FileName) ->
  case pegjs_parse:file(FileName) of
    #grammar{} = G -> analyze(G);
    E              -> E
  end.

-spec analyze(#grammar{}) -> list().
analyze(#grammar{} = Grammar) ->
  Analysis0 = analyze(Grammar, #analysis{}),
  Analysis = #analysis{errors = Errors} = verify(Analysis0),
  case Errors of
    []   -> {Grammar, Analysis};
    List -> {error, List}
  end.


%%_* Internal ==================================================================
-spec analyze(list(), #analysis{}) -> list().
analyze([], #analysis{} = A) ->
  A;
analyze([Rule | Tail], Analysis0) ->
  Analysis = analyze(Rule, Analysis0),
  analyze(Tail, Analysis);
analyze( #grammar{rules = Rules, initializer = Initializer}
       , Analysis) ->
  analyze(Rules, add_code(Initializer, Analysis));
analyze( #rule{expression = Expression, index = Index, name = Name}
       , Analysis) ->
  analyze(Expression, add_rule(Name, Analysis, Index));
analyze( #choice{alternatives = Alternatives, index = Index}, Analysis) ->
  analyze(Alternatives, add_combinator(choice, Analysis, Index));
analyze( #sequence{elements = Elements, code = Code, index = Index}
       , Analysis) ->
  analyze(Elements, add_code( Code
                            , add_combinator(sequence, Analysis, Index)
                            ));
analyze( #text{expression = Expression, index = Index}, Analysis) ->
  analyze(Expression, add_combinator(choice, Analysis, Index));
analyze( #labeled{expression = Expression, index = Index}, Analysis) ->
  analyze(Expression, add_combinator(labeled, Analysis, Index));
analyze( #prefixed{expression = Expression, code = Code, index = Index}
       , Analysis) ->
  analyze(Expression, add_code( Code
                              , add_combinator(prefixed, Analysis, Index)
                              ));
analyze( #suffixed{expression = Expression, index = Index}, Analysis) ->
  analyze(Expression, add_combinator(suffixed, Analysis, Index));
analyze( #rule_ref{name = Name, index = Index}, Analysis) ->
  add_rule_ref(Name, Analysis, Index);
analyze( #anything{index = Index}, Analysis) ->
  add_combinator(anything, Analysis, Index);
analyze( #character_range{index = Index}, Analysis) ->
  add_combinator(character_range, Analysis, Index);
analyze( #charclass{index = Index}, Analysis) ->
  add_combinator(charclass, Analysis, Index);
analyze( #regexp{index = Index}, Analysis) ->
  add_combinator(regexp, Analysis, Index);
analyze( #code{index = Index}, Analysis) ->
  add_combinator(code, Analysis, Index);
analyze( #literal{index = Index}, Analysis) ->
  add_combinator(literal, Analysis, Index).

-spec add_combinator(atom(), #analysis{}, index()) -> #analysis{}.
add_combinator( Name
              , #analysis{combinators = Combinators0} = Analysis
              , Index
              ) ->
  Combinators = case orddict:is_key(Name, Combinators0) of
                  true  ->
                    Indices = orddict:fetch(Name, Combinators0),
                    orddict:store(Name, [Index | Indices], Combinators0);
                  false ->
                    orddict:store(Name, [Index], Combinators0)
                end,
  Analysis#analysis{combinators = Combinators}.

-spec add_rule(atom(), #analysis{}, index()) -> #analysis{}.
add_rule( Name
        , #analysis{errors = Errors0, unique_rules = UniqueRules0} = Analysis
        , Index
        ) ->
  case orddict:is_key(Name, UniqueRules0) of
    true  ->
      OriginalIndex = orddict:fetch(Name, UniqueRules0),
      Errors = ordsets:add_element( { duplicate_rule
                                    , {Name, Index, OriginalIndex}
                                    }
                                  , Errors0
                                  ),
      Analysis#analysis{errors = Errors};
    false ->
      UniqueRules = orddict:store(Name, Index, UniqueRules0),
      Analysis#analysis{unique_rules = UniqueRules}
  end.

-spec add_rule_ref(atom(), #analysis{}, index()) -> #analysis{}.
add_rule_ref( Name
            , #analysis{required_rules = RequiredRules0} = Analysis0
            , Index
            ) ->
  RequiredRules = case orddict:is_key(Name, RequiredRules0) of
                    true  ->
                      Indices = orddict:fetch(Name, RequiredRules0),
                      orddict:store(Name, [Index | Indices], RequiredRules0);
                    false ->
                      orddict:store(Name, [Index], RequiredRules0)
                  end,
  Analysis0#analysis{required_rules = RequiredRules}.

-spec add_code(binary() | list(), #analysis{}) -> #analysis{}.
add_code( #code{code = Source, index = Index}
        , #analysis{code = Code0} = Analysis0) ->
  Code = case Source of
           [] -> Code0;
           <<>> -> Code0;
           _ -> orddict:store(Index, Source, Code0)
         end,
  Analysis0#analysis{code = Code};
add_code([], Analysis) ->
  Analysis.


-spec verify(#analysis{}) -> ok | {error, term()}.
verify(Analysis) ->
%%   Analysis1 = verify_required_rules(Analysis0),
%%   verify_extra_rules(Analysis1).
  chain( [ fun verify_required_rules/1
         , fun verify_multiple_roots/1
         , fun verify_code/1
         ]
       , Analysis
       ).

-spec verify_required_rules(#analysis{}) -> #analysis{}.
verify_required_rules(#analysis{ errors = Errors0
                               , required_rules = Required
                               , unique_rules = Unique
                               } = Analysis0) ->
  RequiredKeys = orddict:fetch_keys(Required),
  UniqueKeys = orddict:fetch_keys(Unique),
  case lists:subtract(RequiredKeys, UniqueKeys) of
    []   -> Analysis0;
    KeyList0 ->
      KeyList = [  {Name, lists:usort(orddict:fetch(Name, Required))}
                || Name <- KeyList0],
      Errors = ordsets:add_element( {required_rules_missing, KeyList}
                                  , Errors0
                                  ),
      Analysis0#analysis{errors = Errors}
  end.

-spec verify_multiple_roots(#analysis{}) -> #analysis{}.
verify_multiple_roots(#analysis{ errors = Errors0
                               , required_rules = Required
                               , unique_rules = Unique
                               } = Analysis0) ->
  RequiredKeys = orddict:fetch_keys(Required),
  UniqueKeys = orddict:fetch_keys(Unique),
  case lists:subtract(UniqueKeys, RequiredKeys) of
    []   -> Analysis0;
    List when length(List) =:= 1 -> Analysis0;
    KeyList0 ->
      KeyList = [  {Name, orddict:fetch(Name, Unique)}
                || Name <- KeyList0],
      Errors = ordsets:add_element( {multiple_roots, lists:usort(KeyList)}
                                  , Errors0
                                  ),
      Analysis0#analysis{errors = Errors}
  end.

-spec verify_code(#analysis{}) -> #analysis{}.
verify_code(#analysis{ code   = Code0
                     , errors = Errors0
                     } = Analysis) ->
  {CodeErrors, Code} = verify_code(Code0, [], []),
  Errors = case CodeErrors of
             [] -> Errors0;
             _ -> ordsets:add_element({invalid_code, CodeErrors}, Errors0)
           end,
  Analysis#analysis{code = Code, errors = Errors}.

-spec verify_code(list(), list(), list()) -> {list(), list()}.
verify_code([], Errors, Accum) ->
  {Errors, lists:reverse(Accum)};
verify_code([{Index, Code} | T], Errors, Accum) ->
  case verify_code_block(Code) of
    {error, Reason} ->
      verify_code(T, [{Reason, Index} | Errors], Accum);
    {ok, Vars} ->
      verify_code(T, Errors, [{Code, Vars, Index} | Accum])
  end.

-spec verify_code_block(binary()) -> {ok, list()} | {error, term()}.
verify_code_block(_Code) ->
  {ok, []}.

%%_* Helpers ===================================================================
-spec chain([chain_func()], #analysis{}) -> ok | {error, term()}.
-type chain_func() :: fun((#analysis{}) -> {ok, #analysis{}} | {error, term()}).
chain([], Analysis) -> Analysis;
chain([F | T], Analysis) ->
  chain(T, F(Analysis)).
