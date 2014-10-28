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
                              File = proplists:get_value(root, Options0),
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
    Analysis -> Analysis#analysis{grammar = Grammar}
  end.

-spec perform_analysis(#entry{}, #analysis{}) -> any().
perform_analysis([], Analysis) ->
  Analysis;
perform_analysis([H|T], Analysis0) ->
  Analysis = perform_analysis(H, Analysis0),
  perform_analysis(T, Analysis);
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
                       } = Entry, #analysis{ combinators = Combinators0
                                           , unique_rules = Rules0
                                           } = Analysis) ->
  Combinators = dict:store(Name, Entry, Combinators0),
  Rules = dict:store(Name, Idx, Rules0),
  perform_analysis(Expression, Analysis#analysis{ combinators = Combinators
                                                , unique_rules = Rules});
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
                       , code = Code
                       , index = Idx
                       }, #analysis{code = Codes0} = Analysis) ->
  Codes = dict:store(Idx, Code, Codes0),
  perform_analysis(Alternatives, Analysis#analysis{code = Codes});
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
                       , code = Code
                       , index = Idx
                       }, #analysis{code = Codes0} = Analysis) when Type =:= <<"semantic_and">>;
                                                                    Type =:= <<"semantic_not">>  ->
  Codes = dict:store(Idx, Code, Codes0),
  Analysis#analysis{code = Codes};
perform_analysis(#entry{ type = <<"literal">>
                       }, Analysis) ->
  Analysis;
perform_analysis(#entry{ type = <<"class">>
                       }, Analysis) ->
  Analysis;
perform_analysis(#entry{ type = <<"any">>
                       }, Analysis) ->
  Analysis.


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
report_left_recursion(#analysis{combinators = Combinators} = Analysis) ->
  case dict:fold(fun(_, Value, Acc) ->
                   case check_left_recursion(Value, Analysis, dict:new()) of
                     {ok, _} -> Acc;
                     Other   -> [Other | Acc]
                   end
                 end, [], Combinators) of
    []    -> Analysis;
    Rules -> {error, {left_recursion, Rules}}
  end.

check_left_recursion([], _, AppliedRules) ->
  {ok, AppliedRules};
check_left_recursion([H|T], Analysis, AppliedRules) ->
  case check_left_recursion(H, Analysis, AppliedRules) of
    {ok, Rules} -> check_left_recursion(T, Analysis, [Rules | AppliedRules]);
    Other       -> Other
  end;
check_left_recursion( #entry{ type = <<"rule">>
                            , name = Name
                            , expression = Expression}
                    , Analysis
                    , AppliedRules) ->
  check_left_recursion(Expression, Analysis, dict:store(Name, Name, AppliedRules));
check_left_recursion(#entry{ type = <<"sequence">>
                           , elements = [H|_]}, Analysis, AppliedRules) ->
  check_left_recursion(H, Analysis, AppliedRules);
check_left_recursion(#entry{ type = <<"rule_ref">>
                           , name = Name
                           , index = Idx
                           }, #analysis{combinators = Rules} = Analysis, AppliedRules) ->
  case dict:find(Name, AppliedRules) of
    {ok, _} -> {Name, Idx};
    error   ->
      {ok, Entry} = dict:find(Name, Rules),
      check_left_recursion(Entry, Analysis, AppliedRules)
  end;
check_left_recursion(_, _, AppliedRules) ->
  {ok, AppliedRules}.



-spec remove_proxy_rules(#analysis{}) -> any().
remove_proxy_rules(#analysis{combinators = _Combinators} = Analysis) ->
  %% TODO!!
  Analysis.
