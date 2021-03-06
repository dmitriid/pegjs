%%%-----------------------------------------------------------------------------
%%% @doc Analyze output of peg.js parser
%%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%%%
%%%-----------------------------------------------------------------------------
-module(pegjs_analyze).

%%_* Exports ===================================================================
-export([ analyze/1
        , perform_analysis/2
        , file/1
        , file/2
        ]).

%%_* Includes ==================================================================
-include("pegjs.hrl").

%%_* Types =====================================================================
-type option()  :: {ignore_unused, boolean()}        %% ignore unused rules. Default: true
                 | {ignore_duplicates, boolean()}    %% ignore duplicate rules. Default: false
                 | {ignore_unparsed, boolean()}      %% ignore incomplete parses. Default: false
                 | {ignore_missing_rules, boolean()} %% Default: false
                 | {ignore_invalid_code, boolean()}  %% Default: false
                 | {parser, atom()}                  %% use a different module to parse grammars. Default: pegjs_parse
                 | {root, Dir::string() | binary()}. %% root directory for @append instructions. Default: undefined
-type options() :: [option()].

-export_type([option/0, options/0]).

%%_* API =======================================================================
-spec file(string()) -> {#grammar{}, #analysis{}} | {error, term()}.
file(FileName) ->
  file(FileName, []).

-spec file(string(), options()) -> {#grammar{}, #analysis{}} | {error, term()}.
file(FileName, Options0) ->
  Options = case proplists:get_value(root, Options0) of
              undefined ->
                Root = filename:dirname(FileName),
                [{root, Root} | Options0];
              _       -> Options0
            end,
  { AppendedGrammar
  , AppendedAnalysis } =  analyze_appended_files(FileName, Options),
  Parser = proplists:get_value(parser, Options, pegjs_parse),
  case Parser:file(FileName) of
    #grammar{} = G -> analyze( merge_grammar(G, AppendedGrammar)
                             , Options
                             , AppendedAnalysis);
    {#grammar{} = G, Unparsed, Index} ->
      case proplists:get_value(ignore_unparsed, Options, false) of
        true -> analyze( merge_grammar(G, AppendedGrammar)
                       , Options
                       , AppendedAnalysis);
        false -> {error, {could_not_parse, Unparsed, Index}}
      end;
    E              -> E
  end.

-spec analyze(#grammar{}) -> {#grammar{}, #analysis{}}.
analyze(Grammar) ->
  analyze(Grammar, []).

-spec analyze(#grammar{}, options()) -> {#grammar{}, #analysis{}}.
analyze(#grammar{} = Grammar, Options) ->
  analyze(Grammar, Options, #analysis{options = Options}).

-spec analyze(#grammar{}, options(), #analysis{}) -> {#grammar{}, #analysis{}}.
analyze(#grammar{} = Grammar, Options0, Analysis0) ->
  Options = fill_options(Options0),
  Analysis1 = perform_analysis(Grammar, Analysis0#analysis{options = Options}),
  Analysis = #analysis{errors = Errors} = verify(Analysis1),
  case Errors of
    []   -> {Grammar, Analysis};
    List -> {error, List}
  end.


%%_* Internal ==================================================================

-spec analyze_appended_files(string(), proplists:proplist()) -> [#grammar{}].
analyze_appended_files(File, Options0) ->
  Appends = pegjs_append:file(File),
  Root    = proplists:get_value(root, Options0),
  Parser  = proplists:get_value(parser, Options0, pegjs_parse),
  IgnoreUnparsed  = proplists:get_value(ignore_unparsed, Options0, false),
  Options = fill_options([ {root, Root}
                         , {parser, Parser}
                         , {ignore_unparsed, IgnoreUnparsed}
                         ]),
  F = fun(AppendFile, {Grammar, Analysis}) ->
        Path = filename:join([Root, AppendFile]),
        case filelib:is_file(Path) of
          true ->
            case ?MODULE:file(Path, Options) of
              {error, _} = E ->
                Errs = ordsets:add_element(E, Analysis#analysis.errors),
                {Grammar, Analysis#analysis{errors = Errs}};
              {G, _A} -> { merge_grammar(G, Grammar)
                         , Analysis }
            end;
          false ->
            E = {error, {not_found, Path}},
            Errs = ordsets:add_element(E, Analysis#analysis.errors),
            {Grammar, Analysis#analysis{errors = Errs}}
        end
      end,
  lists:foldl(F, {#grammar{}, #analysis{}}, Appends).

-spec perform_analysis(list(), #analysis{}) -> #analysis{}.
perform_analysis([], #analysis{} = A) ->
  A;
perform_analysis([Rule | Tail], Analysis0) ->
  Analysis = perform_analysis(Rule, Analysis0),
  perform_analysis(Tail, Analysis);
perform_analysis( #grammar{rules = Rules, initializer = Initializer}
                , Analysis) ->
  perform_analysis(Rules, add_initializer(Initializer, Analysis));
perform_analysis( #rule{expression = Expression, index = Index, name = Name}
                , Analysis) ->
  perform_analysis(Expression, add_rule(Name, Analysis, Index));
perform_analysis( #choice{alternatives = Alternatives, index = Index}
                , Analysis) ->
  perform_analysis(Alternatives, add_combinator(choice, Analysis, Index));
perform_analysis( #action{expression = Expression, code = Code, index = Index}
                , Analysis) ->
  perform_analysis(Expression, add_code(Code
                                       , add_combinator(action, Analysis, Index)
                                       ));
perform_analysis( #sequence{elements = Elements, code = Code, index = Index}
                , Analysis) ->
  perform_analysis(Elements, add_code( Code
                                     , add_combinator(sequence, Analysis, Index)
                                     ));
perform_analysis( #text{expression = Expression, index = Index}, Analysis) ->
  perform_analysis(Expression, add_combinator(choice, Analysis, Index));
perform_analysis( #labeled{expression = Expression, index = Index}, Analysis) ->
  perform_analysis(Expression, add_combinator(labeled, Analysis, Index));
perform_analysis( #prefixed{expression = Expression, code = Code, index = Index}
                , Analysis) ->
  case Expression of
   undefined ->
      add_code(Code
        , add_combinator(prefixed, Analysis, Index)
      );
    _ ->
      perform_analysis(Expression, add_combinator(prefixed, Analysis, Index))
  end;
perform_analysis( #suffixed{expression = Expression, index = Index}, Analysis) ->
  perform_analysis(Expression, add_combinator(suffixed, Analysis, Index));
perform_analysis( #rule_ref{name = Name, index = Index}, Analysis) ->
  add_rule_ref(Name, Analysis, Index);
perform_analysis( #anything{index = Index}, Analysis) ->
  add_combinator(anything, Analysis, Index);
perform_analysis( #character_range{index = Index}, Analysis) ->
  add_combinator(character_range, Analysis, Index);
perform_analysis( #charclass{index = Index}, Analysis) ->
  add_combinator(charclass, Analysis, Index);
perform_analysis( #regexp{index = Index}, Analysis) ->
  add_combinator(regexp, Analysis, Index);
perform_analysis( #code{index = Index}, Analysis) ->
  add_combinator(code, Analysis, Index);
perform_analysis( #literal{index = Index}, Analysis) ->
  add_combinator(literal, Analysis, Index);
perform_analysis( _, Analysis) ->
  Analysis.

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
        , #analysis{ errors       = Errors0
                   , unique_rules = UniqueRules0
                   , options      = Options} = Analysis
        , Index
        ) ->
  case { orddict:is_key(Name, UniqueRules0)
       , proplists:get_value(ignore_duplicates, Options)} of
    {true, false}  ->
      OriginalIndex = orddict:fetch(Name, UniqueRules0),
      Errors = ordsets:add_element( { duplicate_rule
                                    , {Name, Index, OriginalIndex}
                                    }
                                  , Errors0
                                  ),
      Analysis#analysis{errors = Errors};
    _ ->
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
add_code(_, Analysis) ->
  Analysis.

-spec add_initializer(binary() | list(), #analysis{}) -> #analysis{}.
add_initializer( #code{code = Source}, Analysis0) ->
  Analysis0#analysis{initializer = Source};
add_initializer(_, Analysis) ->
  Analysis.


-spec verify(#analysis{}) -> ok | {error, term()}.
verify(Analysis) ->
%%   Analysis1 = verify_required_rules(Analysis0),
%%   verify_extra_rules(Analysis1).
  chain( [ fun verify_required_rules/1
         , fun verify_multiple_roots/1
         , fun verify_initializer/1
         , fun verify_code/1
         ]
       , Analysis
       ).

-spec verify_required_rules(#analysis{}) -> #analysis{}.
verify_required_rules(#analysis{ errors         = Errors0
                               , required_rules = Required
                               , unique_rules   = Unique
                               , options        = Options
                               } = Analysis0) ->
  RequiredKeys = orddict:fetch_keys(Required),
  UniqueKeys   = orddict:fetch_keys(Unique),
  case lists:subtract(RequiredKeys, UniqueKeys) of
    []   -> Analysis0;
    KeyList0 ->
      case proplists:get_value(ignore_missing_rules, Options) of
        true -> Analysis0;
        false ->
          KeyList = [  {Name, lists:usort(orddict:fetch(Name, Required))}
                       || Name <- KeyList0],
          Errors = ordsets:add_element( {required_rules_missing, KeyList}
                                      , Errors0
                                      ),
          Analysis0#analysis{errors = Errors}
      end
  end.

-spec verify_multiple_roots(#analysis{}) -> #analysis{}.
verify_multiple_roots(#analysis{ errors         = Errors0
                               , required_rules = Required
                               , unique_rules   = Unique
                               , options        = Options
                               } = Analysis0) ->
  case proplists:get_value(ignore_unused, Options) of
    true -> Analysis0;
    false ->
      RequiredKeys = orddict:fetch_keys(Required),
      UniqueKeys   = orddict:fetch_keys(Unique),
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
      end
  end.

-spec verify_initializer(#analysis{}) -> #analysis{}.
verify_initializer(#analysis{ initializer = _Initializer
                            , errors = _Errors0
                            } = Analysis) ->
  %% Don't know how to properly verify this yet
  Analysis.

-spec verify_code(#analysis{}) -> #analysis{}.
verify_code(#analysis{ code    = Code0
                     , errors  = Errors0
                     , options = Options
                     } = Analysis) ->
  {CodeErrors, Code} = verify_code(Code0, [], orddict:new(), Options),
  Errors = case CodeErrors of
             [] -> Errors0;
             _ -> ordsets:add_element({invalid_code, CodeErrors}, Errors0)
           end,
  Analysis#analysis{code = Code, errors = Errors}.

-spec verify_code(list(), list(), list(), list()) -> {list(), list()}.
verify_code([], Errors, Accum, _Options) ->
  {Errors, Accum};
verify_code([{Index, Code} | T], Errors, Accum, Options) ->
  case verify_code_block(Code) of
    {error, Reason} ->
      case proplists:get_value(ignore_invalid_code, Options, false) of
        true ->
          verify_code(T, Errors, orddict:store(Index, {Code, []}, Accum), Options);
        false ->
          verify_code(T, [{Reason, Index} | Errors], Accum, Options)
      end;
    {ok, Vars} ->
      verify_code(T, Errors, orddict:store(Index, {Code, Vars}, Accum), Options)
  end.

-spec verify_code_block(binary()) -> {ok, list()} | {error, term()}.
verify_code_block(Source0) ->
  Source = binary_to_list(Source0),
  case erl_scan:string(Source) of
    {error, Info, Location} ->
      {error, {Info, Location}};
    {ok, Tokens, EndLocation} ->
      %% We add the dot token so that it makes a complete
      %% expression list.
      case erl_parse:parse_exprs(Tokens ++ [{dot, EndLocation}]) of
        {ok, _ExprList} ->
          %% Find which arguments are used so they can be
          %% applied to the generated function.
          Vars = used_transform_variables(Tokens),
          {ok, Vars};
        {error, {_, _, Info}} ->
          {error, iolist_to_binary(Info)}
      end
  end.

used_transform_variables(Tokens) ->
    ordsets:to_list(lists:foldl(fun used_transform_variables/2,
                                ordsets:new(), Tokens)).

used_transform_variables({var, _, 'Node'}, Acc) ->
  ordsets:add_element(<<"Node">>, Acc);
used_transform_variables({var, _, 'Idx'}, Acc) ->
  ordsets:add_element(<<"Idx">>, Acc);
used_transform_variables(_, Acc) -> Acc.

%%_* Helpers ===================================================================
-spec chain([chain_func()], #analysis{}) -> ok | {error, term()}.
-type chain_func() :: fun((#analysis{}) -> {ok, #analysis{}} | {error, term()}).
chain([], Analysis) -> Analysis;
chain([F | T], Analysis) ->
  chain(T, F(Analysis)).

-spec fill_options(proplists:proplist()) -> options().
fill_options(UserOptions) ->
  Options = [ {ignore_unused, true}
            , {ignore_duplicates, false}
            , {ignore_missing_rules, false}
            , {ignore_invalid_code, false}
            , {root, undefined}],

  lists:map( fun({Key, Val}) ->
               {Key, proplists:get_value(Key, UserOptions, Val)}
             end
           , Options).

-spec merge_grammar(#grammar{}, #grammar{}) -> #grammar{}.
merge_grammar( #grammar{ initializer = #code{ code = Initializer1
                                            , index = Index }
                       , rules       = Rules1 }
             , #grammar{ initializer = #code{code = Initializer2}
                       , rules       = Rules2 }) ->
  Initializer = <<Initializer1/binary, Initializer2/binary>>,
  #grammar{ initializer = #code{ code = Initializer
                               , index = Index}
          , rules       = Rules1 ++ Rules2
          }.
