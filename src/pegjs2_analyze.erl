%%%-----------------------------------------------------------------------------
%%% @doc Analyze output of peg.js parser
%%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%%%
%%%-----------------------------------------------------------------------------
-module(pegjs2_analyze).

%%_* Exports ===================================================================
-export([ analyze/1
        , report_missing_rules/1
        , report_left_recursion/1
        , remove_proxy_rules/1
%%         , perform_analysis/2
%%         , file/1
%%         , file/2
        ]).

%%_* Includes ==================================================================
-include("pegjs2.hrl").

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

-spec analyze(options()) -> any().
analyze(Options0) ->
  {Fun, Input, Options} = case proplists:get_value(input, Options0) of
                            undefined ->
                              File = proplists:get_value(input_file, Options0),
                              case proplists:get_value(root, Options0) of
                                undefined ->
                                  Root = filename:dirname(File),
                                  {file, File, [{root, Root} | Options0]};
                                _ -> {file, File, Options0}
                              end;
                            I ->
                              {parse, I, Options0}
                          end,
  Parser = proplists:get_value(parser, Options, pegjs2_parse),
  case Parser:Fun(Input) of
    #entry{} = G -> analyze(G, Options);
    {_G, Unparsed, Index} ->
      {error, {could_not_parse, Unparsed, Input, Index}};
    Other -> {error, {could_not_parse, Other}}
  end.

-spec analyze([#entry{}], options()) -> any().
analyze(Grammar, Options) ->
  case perform_analysis(Grammar, #analysis{options = Options}) of
    {error, _} = Error -> Error;
    Analysis -> Analysis#analysis{ grammar = Grammar
                                 , combinators = retrieve_combinators(Grammar)
                                 }
  end.

-spec perform_analysis(#entry{}, #analysis{}) -> any().
perform_analysis([], Analysis) ->
  Analysis;
perform_analysis([H|T], Analysis0) ->
  case perform_analysis(H, Analysis0) of
    {error, _} = E -> E;
    Analysis       -> perform_analysis(T, Analysis)
  end;
perform_analysis(#entry{ type = <<"grammar">>
                       , initializer = Initializer
                       , rules = Rules
                       }, Analysis) ->
  perform_analysis(Rules, Analysis#analysis{initializer = Initializer});
perform_analysis(#entry{ type = <<"rule">>
                       , name = Name
                       , display_name = _DisplayName
                       , expression = Expression
                       , index = Idx
                       }, #analysis{ unique_rules = Rules0
                                   } = Analysis) ->
  Rules = dict:store(Name, Idx, Rules0),
  perform_analysis(Expression, Analysis#analysis{ unique_rules = Rules});
perform_analysis(#entry{ type = <<"named">>
                       , expression = Expression
                       }, Analysis) ->
  perform_analysis(Expression, Analysis);
perform_analysis(#entry{ type = <<"choice">>
                       , alternatives = Alternatives
                       }, Analysis) ->
  perform_analysis(Alternatives, Analysis);
perform_analysis(#entry{ type = <<"action">>
                       , expression = Alternatives
                       , code = Code0
                       , index = Idx
                       }, #analysis{ code   = Codes0 } = Analysis) ->
    case perform_code_analysis(Code0, Idx) of
      {error, _} = E -> E;
      {ok, F} ->
        Codes = dict:store(Idx, F, Codes0),
        perform_analysis(Alternatives, Analysis#analysis{code = Codes})
    end;
perform_analysis(#entry{ type = <<"sequence">>
                       , elements = Elements
                       }, Analysis) ->
  perform_analysis(Elements, Analysis);
perform_analysis(#entry{ type = <<"labeled">>
                       , expression = Expression
                       }, Analysis) ->
  perform_analysis(Expression, Analysis);
perform_analysis(#entry{ type = Type
                       , expression = Expression
                       }, Analysis) when Type =:= <<"text">>;
                                         Type =:= <<"simple_and">>;
                                         Type =:= <<"simple_not">>  ->
  perform_analysis(Expression, Analysis);
perform_analysis(#entry{ type = Type
                       , expression = Expression
                       }, Analysis) when Type =:= <<"optional">>;
                                         Type =:= <<"zero_or_more">>;
                                         Type =:= <<"one_or_more">>  ->
  perform_analysis(Expression, Analysis);
perform_analysis(#entry{ type = <<"rule_ref">>
                       , name = Name
                       , index = Idx
                       }, #analysis{required_rules = Required0} = Analysis) ->
  Required = case dict:find(Name, Required0) of
               error         -> dict:store(Name, [Idx], Required0);
               {ok, Indices} -> dict:store(Name, [Idx | Indices], Required0)
             end,
  Analysis#analysis{required_rules = Required};
perform_analysis(#entry{ type = Type
                       , code = Code0
                       , index = Idx
                       }, #analysis{code = Codes0} = Analysis) when Type =:= <<"semantic_and">>;
                                                                    Type =:= <<"semantic_not">>  ->
    case perform_code_analysis(Code0, Idx) of
      {error, _} = E -> E;
      {ok, F} ->
        Codes = dict:store(Idx, F, Codes0),
        Analysis#analysis{code = Codes}
    end;
perform_analysis(#entry{ type = <<"literal">>
                       }, Analysis) ->
  Analysis;
perform_analysis(#entry{ type = <<"class">>
                       }, Analysis) ->
  Analysis;
perform_analysis(#entry{ type = <<"any">>
                       }, Analysis) ->
  Analysis.

-spec retrieve_combinators(#entry{} | [#entry{}]) -> dict:dict().
retrieve_combinators(#entry{type = <<"grammar">>
                           , rules = Rules
                           , index = Index
                           }) ->
  retrieve_combinators(Rules, dict:new(), Index).

-spec retrieve_combinators( #entry{} | [#entry{}]
                          , dict:dict()
                          , index()
                          ) -> dict:dict().
retrieve_combinators([], Combinators, _) ->
  Combinators;
retrieve_combinators(undefined, Combinators, _) ->
  Combinators;
retrieve_combinators([H|T], Combinators0, Idx) ->
  retrieve_combinators(T, retrieve_combinators(H, Combinators0, Idx), Idx);
retrieve_combinators(#entry{ type = Type
                           , expression = Expression
                           , alternatives = Alternatives
                           , elements = Elements
                           , index = EntryIdx
                           }, Combinators0, Idx) ->
  Cs0 = retrieve_combinators(Expression, Combinators0, EntryIdx),
  Cs1 = retrieve_combinators(Alternatives, Cs0, EntryIdx),
  Cs  = retrieve_combinators(Elements, Cs1, EntryIdx),
  case is_combinator(Type) of
    false -> Cs;
    true  ->
      case dict:find(Type, Cs) of
        'error'    ->
          dict:store(Type, [Idx], Cs);
        {ok, Idxs} ->
          dict:store(Type, [Idx | Idxs], Cs)
      end
  end.

-spec is_combinator(binary()) -> boolean().
is_combinator(Type) ->
  lists:member(Type, [ <<"any">>
                     , <<"class">>
                     , <<"literal">>
                     , <<"rule_ref">>
                     , <<"one_or_more">>
                     , <<"zero_or_more">>
                     , <<"optional">>
                     , <<"simple_not">>
                     , <<"simple_and">>
                     , <<"semantic_not">>
                     , <<"semantic_and">>
                     , <<"text">>
                     , <<"labeled">>
                     , <<"sequence">>
                     , <<"action">>
                     , <<"choice">>
                     , <<"named">>]).

-spec perform_code_analysis(binary(), index()) ->
  {ok, #function{}} | {error, term()}.
perform_code_analysis(<<>>, Index) ->
  {error, {missing_action_code, Index}};
perform_code_analysis(Code0, Index) ->
  Code = case Code0 of
           [C] -> C;
           C -> C
         end,
  case validate_code(Code) of
    {error, Reason} -> {error, {Reason, Index}};
    {ok, Args}      -> {ok, #function{ arg   = Args
                                     , code  = Code
                                     , index = Index
                                     }
                       }
  end.

-spec validate_code(binary()) -> {ok, list()} | {error, term()}.
validate_code(Code) ->
  Source = binary_to_list(Code),
  case erl_scan:string(Source) of
    {error, Info, Location} ->
      {error, {Info, Location}};
    {ok, Tokens, EndLocation} ->
      %% We add the dot token so that it makes a complete
      %% expression list.
      case erl_parse:parse_exprs(Tokens ++ [{dot, EndLocation}]) of
        {ok, _ExprList} ->
          case code_uses_node_var(Tokens) of
            true  -> {ok, <<"Node">>};
            false -> {ok, <<"_Node">>}
          end;
        {error, {_, _, Info}} ->
          {error, iolist_to_binary(Info)}
      end
  end.

-spec code_uses_node_var(list()) -> boolean().
code_uses_node_var(Tokens) ->
  [] /= [node || {var, _, 'Node'} <- Tokens].

-spec report_missing_rules(#analysis{}) -> any().
report_missing_rules(#analysis{ unique_rules = Rules
                              , required_rules = Required} = Analysis) ->
  Missing = dict:fold( fun(Key, Value, Acc) ->
                         case dict:find(Key, Rules) of
                           error -> [{Key, Value} | Acc];
                           _     -> Acc
                         end
                       end
                     , []
                     , Required),
  case Missing of
    [] -> Analysis;
    _  -> {error, {missing_rules, Missing}}
  end.

-spec report_left_recursion(#analysis{}) -> any().
report_left_recursion(#analysis{grammar = #entry{rules = Rules}} = Analysis) ->
  case lists:foldl(fun(E, Acc) ->
                     case check_left_recursion(E, Rules, []) of
                       {ok, _} -> Acc;
                       Other   -> [Other, Acc]
                     end
                   end, [], Rules) of
    [] -> Analysis;
    RecursiveRules -> {error, {left_recursion, RecursiveRules}}
  end.

-spec check_left_recursion(#entry{}, [#entry{}], [binary()]) ->
                                                      {ok, term()} | [binary()].
check_left_recursion(#entry{ type = <<"rule">>
                           , name = Name
                           , expression = Expression0
                           }, Rules, AppliedRules) ->
  Expression = case Expression0 of
                 [E] -> E;
                 #entry{} -> Expression0
               end,
  check_left_recursion(Expression, Rules, [Name | AppliedRules]);
check_left_recursion(#entry{ type = <<"sequence">>
                           , elements = [H | _]
                           }, Rules, AppliedRules) ->
  check_left_recursion(H, Rules, AppliedRules);
check_left_recursion(#entry{ type = <<"rule_ref">>
                           , name = Name
                           , index = Idx
                           } = Ref, Rules, AppliedRules) ->
  case lists:member(Name, AppliedRules) of
    true  -> {Name, Idx};
    false ->
      [Entry] = [E || E <- Rules, E#entry.name =:= Name, E /= Ref],
      check_left_recursion(Entry, Rules, AppliedRules)
  end;
check_left_recursion(_, _, AppliedRules) ->
  {ok, AppliedRules}.


%%
%% @doc Removes proxy rules -- that is, rules that only delegate to other rule.
%%
-spec remove_proxy_rules(#analysis{}) -> any().
remove_proxy_rules(#analysis{ combinators    = Combinators
                            , grammar        = #entry{rules = [H | Rules0]}
                                             = Grammar0
                            , required_rules = RequiredRules
                            , unique_rules   = UniqueRules
                            } = Analysis) ->
  ProxyRules = detect_proxy_rules(Grammar0),

  RemoveRules = [From || {From, _} <- ProxyRules, From /= H#entry.name],

  Rules = [R || R <- Rules0, not lists:member(R#entry.name, RemoveRules)],
  Grammar = Grammar0#entry{rules = [H | Rules]},
  Analysis#analysis{ combinators    = erase_from_dict(RemoveRules, Combinators)
                   , grammar        = replace_proxy_rules(ProxyRules, Grammar)
                   , required_rules = erase_from_dict(RemoveRules, RequiredRules)
                   , unique_rules   = erase_from_dict(RemoveRules, UniqueRules)
                   }.

-spec erase_from_dict(list(), dict:dict()) -> dict:dict().
erase_from_dict(List, Dict) ->
  lists:foldl(fun(E, D) -> dict:erase(E, D) end, Dict, List).

-spec detect_proxy_rules(#entry{}) -> list().
detect_proxy_rules(#entry{ type = <<"grammar">>
                         , rules = Rules
                         }) ->
  lists:foldl(fun detect_proxy_rule/2, [], Rules).

-spec detect_proxy_rule(#entry{}, list()) -> list().
detect_proxy_rule(#entry{ name = ParentName
                        , expression = #entry{} = Expression
                        }, Acc) ->
  case Expression of
    #entry{type = <<"rule_ref">>, name = Name} ->
      [{ParentName, Name} | Acc];
    _ ->
      Acc
  end;
detect_proxy_rule(_, Acc) ->
  Acc.

-spec replace_proxy_rules(list(), #entry{}) -> #entry{}.
replace_proxy_rules(Rules, Grammar) ->
  lists:foldl(fun replace_proxy_rule/2, Grammar, Rules).

-spec replace_proxy_rule(#entry{} | list(), binary()) -> #entry{}.
replace_proxy_rule({From, To}, #entry{ type = <<"rule_ref">>
                                     , name = From
                                     } = Entry) ->
  Entry#entry{name = To};
replace_proxy_rule(Proxy, #entry{ alternatives = Alternatives
                                , elements     = Elements
                                , expression   = Expression
                                , rules        = Rules
                                } = Entry) ->
  Entry#entry{ alternatives = replace_proxy_rule(Proxy, Alternatives)
             , elements     = replace_proxy_rule(Proxy, Elements)
             , expression   = replace_proxy_rule(Proxy, Expression)
             , rules        = replace_proxy_rule(Proxy, Rules)
             };
replace_proxy_rule(Proxy, L) when is_list(L) ->
  [replace_proxy_rule(Proxy, E) || E <- L];
replace_proxy_rule(_, Other) ->
  Other.
