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
-module(pegjs2_javascript_fn).

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
                  , combinators = Combinators}) ->
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
%%          , {fun emit/1, [Grammar]}
%%          , {fun code_for/1, [Combinators]}
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
                    , expression = [Expression]}) ->
  <<"pegjs_rule('", Name/binary, "', Node) -> \n"
  , "  pegjs( Node\n"
  , "       , '", Name/binary, "'\n"
  , "       , fun(N) -> \n"
  , "           (", (generate_combinator_call(Expression))/binary, ")(N)\n"
  , "         end\n"
  , "       )"
  >>.

generate_combinator_call(#entry{type = <<"any">>}) ->
  <<"any()">>;
generate_combinator_call(#entry{ type = <<"choice">>
                               , alternatives = Elements}) ->
  <<"choice(["
  , (generate_combinator_list(Elements))/binary
  , "])">>;
generate_combinator_call(#entry{ type = <<"sequence">>
                               , elements = Elements}) ->
  <<"sequence(["
  , (generate_combinator_list(Elements))/binary
  , "])">>;
generate_combinator_call(#entry{ type = <<"literal">>
                               , value = Text
                               , ignore_case = IgnoreCase}) ->
  Chars0 = unicode:characters_to_list(iolist_to_binary(Text)),
  Chars = io_lib:format("~p", [Chars0]),
  <<"literal("
  , "unicode:characters_to_binary(", (iolist_to_binary(Chars))/binary, "), "
  , (atom_to_binary(IgnoreCase, latin1))/binary
  , ")">>;
generate_combinator_call(#entry{ type = <<"class">>
                               , parts = Parts
                               , ignore_case = IgnoreCase
                               , inverted = Inverted}) ->
  Ps0 = lists:map(fun([B]) when is_binary(B) ->
    pegjs_util:escape_for_regex(B);
    ([[B1, B2]]) ->
      << (pegjs_util:escape_for_regex(B1))/binary
      , "-"
      , (pegjs_util:escape_for_regex(B2))/binary
      >>
  end, Parts),
  Ps = iolist_to_binary([<<"^[">>
                        , case Inverted of true -> <<"^">>; false -> <<>> end
                        , Ps0
                        , <<"]">>
                        ]),
  Chars0 = unicode:characters_to_list(Ps),
  Chars = io_lib:format("~p", [Chars0]),
  <<"regexp("
  , "unicode:characters_to_binary(", (iolist_to_binary(Chars))/binary, "), "
  , (atom_to_binary(IgnoreCase, latin1))/binary
  , ")">>;
generate_combinator_call(#entry{ type = <<"rule_ref">>
                               , name = Name}) ->
  <<"rule_ref('", Name/binary, "')">>;
generate_combinator_call(#entry{ type = Type
                               , expression = [Expression]})
  when Type =:= <<"zero_or_more">>
     ; Type =:= <<"one_or_more">>
     ; Type =:= <<"optional">>   ->
  <<"suffixed('", Type/binary, "', "
  , (generate_combinator_call(Expression))/binary
  ,")">>;
generate_combinator_call(#entry{ type = Type
                               , expression = [Expression]})
  when Type =:= <<"simple_and">>
     ; Type =:= <<"simple_not">> ->
  <<"prefixed('", Type/binary, "', "
  , (generate_combinator_call(Expression))/binary
  ,")">>;
generate_combinator_call(#entry{ type = <<"text">>
                               , expression = [Expression]}) ->
  <<"text("
  , (generate_combinator_call(Expression))/binary
  ,")">>;
generate_combinator_call(#entry{ type = <<"labeled">>
                               , label = Label
                               , expression = [Expression]}) ->
  <<"labeled(<<\"", Label/binary, "\">>, "
  , (generate_combinator_call(Expression))/binary
  ,")">>.


generate_combinator_list([]) ->
  <<>>;
generate_combinator_list([H|[]]) ->
  <<(generate_combinator_call(H))/binary>>;
generate_combinator_list([H | T]) ->
  <<(generate_combinator_call(H))/binary
  , ", "
  , (generate_combinator_list(T))/binary>>.

%% emit(#entry{ type = <<"grammar">>
%%            , rules = Rules
%%            }) ->
%%   RuleDefs = dict:new(),
%%   R = lists:foldl(fun(#entry{name = Name} = Entry, Acc) ->
%%                 dict:store(Name, emit(Entry), Acc)
%%               end, RuleDefs, Rules),
%%   {ok, [C || {_, C} <- dict:to_list(R)]};
%% emit(#entry{ type = <<"rule">>
%%            , name = Name
%%            , expression = [Expression]}) ->
%%   E = emit(Expression),
%%   <<"'", Name/binary, "'(Input, Index) -> \n"
%%   , E/binary, ".\n\n">>;
%% emit(#entry{type = <<"any">>}) ->
%%   <<"any(Input, Index)">>;
%% emit(_) ->
%%   "".
%%
%% code_for(Combinators) ->
%%   {ok,
%%   lists:foldl(fun({Combinator, _}, Acc) ->
%%              [generate_code_for(Combinator) | Acc]
%%             end, [], dict:to_list(Combinators))}.
%%
%% generate_code_for(<<"any">>) ->
%%   <<"ch(Input, State) ->\n"
%%   , "  Pos = pos(State),\n"
%%   , "  case Pos > length(Input) of\n"
%%   , "    true  -> failed(?ANY, ?EOI);\n"
%%   , "    false -> get_input(Input, Pos + 1)\n"
%%   , "  end.\n\n">>;
%% generate_code_for(C) ->
%%   io:format("~p~n", [C]),
%%   [].

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
  file:read_file(filename:join([code:priv_dir(pegjs), "pegjs2_fn.template"])).

generate_file_parse(#entry{rules = [H|_]}, Options) ->
  Root = case proplists:get_value(starte_rule, Options) of
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
