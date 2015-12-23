%%%-----------------------------------------------------------------------------
%%% @doc Generate functional parser. Inspired by pegjs-fn and Neotoma
%%%
%%%      pegjs-fn
%%%      See https://github.com/shamansir/pegjs-fn/tree/actual
%%%      and especially http://shamansir.github.io/blog/articles/generating-functional-parsers/
%%%
%%%      Neotoma
%%%      https://github.com/seancribbs/neotoma
%%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%%%
%%%-----------------------------------------------------------------------------
-module(pegjs2_generate_functional).

%%_* Exports ===================================================================
-export([ generate/1
        ]).

%%_* Includes ==================================================================
-include("pegjs2.hrl").

%%_* Types =====================================================================

%%_* API =======================================================================

%%
%% @doc fn-genrator ignores any generated bytecode and uses only the grammar
%%
-spec generate(#analysis{}) -> any().
generate(#analysis{ grammar = Grammar
                  %, combinators = Combinators
                  , options = Options
                  , combinators = Combinators
                  , code = Code
                  }) ->
  FileName = proplists:get_value(output, Options),
  file:delete(FileName),
  {ok, F} = file:open(FileName, [append]),
  write( F
       , [ {fun generate_prelude/1, [Options]}
         , {fun generate_exports/1, [Options]}
         , {fun generate_template/1, [Options]}
         , {fun generate_initializer/1, [Grammar]}
         , {fun generate_file_parse/2, [Grammar, Options]}
         , {fun generate_rules/1, [Grammar]}
         , {fun generate_combinators/1, [Combinators]}
         , {fun generate_functions/1, [Code]}
         ]),
  file:close(F), ok.

%%_* Internal ==================================================================

generate_rules(#entry{type = <<"grammar">>
                     , rules = Rules
                     }) ->
  {ok, generate_rules(Rules)};
generate_rules([H|[]]) ->
  <<(generate_rule(H))/binary, ".\n\n">>;
generate_rules([H|T]) ->
  <<(generate_rule(H))/binary, ";\n", (generate_rules(T))/binary>>;
generate_rules([]) ->
  <<>>.

generate_rule(#entry{ name = Name
                    , expression = Expression}) ->
  <<"pegjs_rule('", Name/binary, "', Node) -> \n"
  , "  pegjs( Node\n"
  , "       , '", Name/binary, "'\n"
  , "       , fun(N) -> \n"
  , "           (", (generate_combinator_call(Expression))/binary, ")(N)\n"
  , "         end\n"
  , "       )"
  >>.

generate_combinator_call(#entry{type = <<"any">>}) ->
  <<"pegjs_combinator_any()">>;
generate_combinator_call(#entry{ type = <<"choice">>
                               , alternatives = Elements}) ->
  <<"pegjs_combinator_choice(["
  , (generate_combinator_list(Elements))/binary
  , "])">>;
generate_combinator_call(#entry{ type = <<"sequence">>
                               , elements = Elements}) ->
  <<"pegjs_combinator_sequence(["
  , (generate_combinator_list(Elements))/binary
  , "])">>;
generate_combinator_call(#entry{ type = <<"literal">>
                               , value = Text
                               , ignore_case = IgnoreCase}) ->
  Chars0 = iolist_to_binary(Text),
  Chars = list_of_ints_to_binary(Chars0),
  <<"pegjs_combinator_literal("
  , "", Chars/binary, ", "
  , (atom_to_binary(IgnoreCase, latin1))/binary
  , ")">>;
generate_combinator_call(#entry{ type = <<"class">>
                               , parts = Parts
                               , ignore_case = IgnoreCase
                               , inverted = Inverted}) ->
  Ps0 = lists:map(fun(B) when is_binary(B) ->
                      pegjs_util:escape_for_regex(B);
                    ([B1, B2]) ->
                      << (pegjs_util:escape_for_regex(B1))/binary
                      , "-"
                      , (pegjs_util:escape_for_regex(B2))/binary
                      >>
                  end, Parts),
  Ps1 = << <<Part/binary>> || Part <- Ps0>>,
  Ps = <<"^["
       , (case Inverted of true -> <<"^">>; false -> <<"">> end)/binary
       , Ps1/binary
       , "]">>,
  Chars = list_of_ints_to_binary(Ps),
  <<"pegjs_combinator_regexp("
  , "", (iolist_to_binary(Chars))/binary, ", "
  , (atom_to_binary(IgnoreCase, latin1))/binary
  , ")">>;
generate_combinator_call(#entry{ type = <<"rule_ref">>
                               , name = Name}) ->
  <<"pegjs_combinator_rule_ref('", Name/binary, "')">>;
generate_combinator_call(#entry{ type = Type
                               , expression = Expression})
  when Type =:= <<"zero_or_more">>
     ; Type =:= <<"one_or_more">>
     ; Type =:= <<"optional">>   ->
  <<"pegjs_combinator_suffixed('", Type/binary, "', "
  , (generate_combinator_call(Expression))/binary
  ,")">>;
generate_combinator_call(#entry{ type = Type
                               , expression = Expression})
  when Type =:= <<"simple_and">>
     ; Type =:= <<"simple_not">> ->
  <<"pegjs_combinator_prefixed('", Type/binary, "', "
  , (generate_combinator_call(Expression))/binary
  ,")">>;
generate_combinator_call(#entry{ type = <<"text">>
                               , expression = Expression}) ->
  <<"pegjs_combinator_text("
  , (generate_combinator_call(Expression))/binary
  ,")">>;
generate_combinator_call(#entry{ type = <<"labeled">>
                               , label = Label
                               , expression = Expression}) ->
  <<"pegjs_combinator_labeled(<<\"", Label/binary, "\">>, "
  , (generate_combinator_call(Expression))/binary
  ,")">>;
generate_combinator_call(#entry{ type = <<"action">>
                               , expression = Expression
                               , index = Index}) ->
  <<"pegjs_combinator_action(", (generate_function_call(Index))/binary, ", "
  , (generate_combinator_call(Expression))/binary
  ,")">>;
generate_combinator_call(#entry{ type = Type
                               , index = Index})
  when Type =:= <<"semantic_and">>
     ; Type =:= <<"semantic_not">> ->
  <<"pegjs_combinator_prefixed('", Type/binary, "', "
  , (generate_function_call(Index))/binary
  ,")">>;
generate_combinator_call(#entry{ type = <<"named">>
                               , expression = Expression}) ->
  <<"pegjs_combinator_named("
  , (generate_combinator_call(Expression))/binary
  ,")">>;
generate_combinator_call([Entry|[]]) ->
  generate_combinator_call(Entry).


generate_combinator_list([]) ->
  <<>>;
generate_combinator_list([H|[]]) ->
  <<(generate_combinator_call(H))/binary>>;
generate_combinator_list([H | T]) ->
  <<(generate_combinator_call(H))/binary
  , ", "
  , (generate_combinator_list(T))/binary>>.

generate_function_call(Index) ->
  <<"fun ", (generate_function_name(Index))/binary
  , "/1">>.

%%_* Helpers ===================================================================
write(File, FunList) ->
  lists:foldl(fun({F, A}, _) ->
                case apply(F, A) of
                  {error, _} = E -> error(E);
                  {ok, V}        -> file:write(File, V)
                end
              end, ok, FunList).

generate_prelude(Options) ->
  Module = atom_to_binary(proplists:get_value(module, Options), latin1),
  {ok, <<"-module('", Module/binary, "').\n\n">>}.

generate_exports(_Options) ->
  {ok,
  <<"%%_* API "
  , "===================================================================\n"
  , "-export([ file/1\n"
  , "        , file/2\n"
  , "        , parse/1\n"
  , "        , parse/2\n"
  , "        ]).\n\n"
  >>}.

generate_template(_Options) ->
  file:read_file(filename:join([code:priv_dir(pegjs), "pegjs2_functional.template"])).

generate_file_parse(#entry{rules = [H|_]}, Options) ->
  Root = case proplists:get_value(start_rule, Options) of
           undefined -> H#entry.name;
           Name      -> Name
         end,
  Out =
    <<"-spec file(file:name()) -> any().\n"
    , "file(Filename) -> file(Filename, '", Root/binary, "').\n\n"
    , "-spec file(file:name(), binary()) -> any().\n"
    , "file(Filename, Root) ->\n"
    , "  case file:read_file(Filename) of \n"
    , "    {ok,Bin} -> parse(Bin, Root);\n"
    , "    Err      -> Err\n"
    , "end.\n\n"
    , "-spec parse(binary() | list()) -> any().\n"
    , "parse(List) when is_list(List) -> parse(list_to_binary(List));\n"
    , "parse(Input) when is_binary(Input) ->\n"
    , "  parse(Input, '", Root/binary, "').\n\n"
    , "-spec parse(binary() | list(), binary()) -> any().\n"
    , "parse(List, Root) when is_list(List) -> \n"
    , "  parse(list_to_binary(List), Root);\n"
    , "parse(Input, Root) when is_binary(Input) ->\n"
    , "  setup_memo(),\n"
    , "  RuleResult = pegjs_rule(Root, #pegjs_node{ input = Input \n"
    , "                                           , index = {{line,1},{column,1}}}),\n"
    , "  Result =  case input(RuleResult) of\n"
    , "              <<>>          ->\n"
    , "                match(RuleResult);\n"
    , "             {error, Error} ->\n"
    , "                {error, Error};\n"
    , "              Unparsed      ->\n"
    , "                {error, {no_match, {Unparsed, index(RuleResult)}}}\n"
    , "           end,\n"
    , "  release_memo(),\n"
    , "Result.\n\n"
    >>,
  {ok, Out}.

generate_initializer(#entry{initializer = #entry{code = I}}) ->
  {ok, I};
generate_initializer(#entry{}) ->
  {ok, <<>>}.

generate_combinators(Combinators) ->
  File = filename:join([code:priv_dir(pegjs), "pegjs_functional.eterm"]),
  {ok, Existing} = file:consult(File),
  Required = [C || {C, _} <- dict:to_list(Combinators)],
  generate_combinators(Required, Existing).

generate_combinators(Required, Existing) ->
  Cs = lists:foldl(fun(C, Acc) ->
                     {_, Reqs, Code} = lists:keyfind(C, 1, Existing),
                     {ok, MoreCode} = generate_combinators(Reqs, Existing),
                     [Code, MoreCode | Acc]
                   end, [], Required),
  {ok, lists:usort(Cs)}.

generate_functions(Funs) ->
  {ok, lists:foldl(fun generate_function_body/2, <<>>, dict:to_list(Funs))}.

generate_function_body({_, #function{ arg   = Arg
                                    , code  = Code
                                    , index = Index}}, Acc) ->
  FName = generate_function_name(Index),
  F = <<"-spec ", FName/binary, "(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.\n"
      , FName/binary, "(", Arg/binary, ") -> \n"
      , Code/binary
      , ".\n\n"
      >>,
  <<Acc/binary, F/binary>>.

generate_function_name({{line, Line}, {column, Column}}) ->
 <<"pegjs_custom_fun_"
 , (list_to_binary(integer_to_list(Line)))/binary
 , "_"
 , (list_to_binary(integer_to_list(Column)))/binary
 >>.

list_of_ints_to_binary(Ints) ->
  <<"<<", (to_list_of_ints(Ints))/binary, ">>">>.

to_list_of_ints(<<>>) ->
  <<>>;
to_list_of_ints(<<I/integer, R/binary>>) when R =:= <<>> ->
  <<(int_to_binary(I))/binary>>;
to_list_of_ints(<<I/integer, H/binary>>) ->
  <<(int_to_binary(I))/binary
  , ","
  , (to_list_of_ints(H))/binary>>.

%% Accomodates earlier versions of Erlang which don't have
%% integer_to_binary/1
int_to_binary(I) ->
  list_to_binary(integer_to_list(I)).