-module(pegjs_parse).
-export([ parse/1
        , parse/2
        , file/1
        , file/2
        ]).

-define(anything, true).
-define(choice, true).
-define(labeled, true).
-define(literal, true).
-define(prefixed, true).
-define(regexp, true).
-define(sequence, true).
-define(suffixed, true).


-include("pegjs.hrl").

-spec get_value(atom(), proplists:proplist()) -> term().
get_value(K, L) when is_list(L)->
  case lists:keyfind(K, 1, L) of
    false  -> [];
    {_, V} -> V
  end;
get_value(_, _) ->
  [].


-spec get_attr(atom(), #charclass{}) -> term().
get_attr(raw_text, #charclass{raw_text = RawText}) ->
  RawText;
get_attr(raw_text, #character_range{raw_text = RawText}) ->
  RawText.

-spec quote_for_regexp_class(string()) -> string().
%% Note: Unlike PEG.js, we don't need to escape Unicode symbols for regexps
quote_for_regexp_class(<<$\\>>) -> <<$\\, $\\>>; % backslash
quote_for_regexp_class(<<$/>>) -> <<$\\, $\\, $/>>; % closing slash
quote_for_regexp_class(<<$]>>) -> <<$\\, $\\, $]>>; % closing bracket
quote_for_regexp_class(<<$^>>) -> <<$\\, $\\, $^>>; % caret
quote_for_regexp_class(<<$->>) -> <<$\\, $\\, $->>; % dash
quote_for_regexp_class(<<$\0>>) -> <<$\\, $\\, $0>>; % null
quote_for_regexp_class(<<$\t>>) -> <<$\\, $\\, $t>>; % horizontal tab
quote_for_regexp_class(<<$\n>>) -> <<$\\, $\\, $n>>; % line feed
quote_for_regexp_class(<<$\v>>) -> <<$\\, $\\, $v>>; % vertical tab
quote_for_regexp_class(<<$\f>>) -> <<$\\, $\\, $f>>; % form feed
quote_for_regexp_class(<<$\r>>) -> <<$\\, $\\, $r>>; % carriage return
quote_for_regexp_class(<<C0:1/binary>>) when C0 =< <<127>> -> % ASCII
  <<C0/binary>>;
quote_for_regexp_class(C) when C =< 127 ->  <<$\\, $x, (to_hex(C))/binary>>; % standalone ASCII
quote_for_regexp_class(C) -> C.

to_hex(C0) when is_binary(C0) ->
  [C] = binary_to_list(C0),
  to_hex(C);
to_hex(C) when is_integer(C), C =< 127 ->
  to_hex(integer_to_list(C, 16));
to_hex([C | []]) ->
  list_to_binary(["0", C]);
to_hex(Hex) when is_list(Hex) ->
  list_to_binary(Hex);
to_hex(Other) ->
  <<Other>>.


do_convert_to_iolist(AST) when is_list(AST) ->
  do_convert_to_iolist(AST, []);
do_convert_to_iolist(AST) when is_binary(AST) ->
  unicode:characters_to_list(AST).

do_convert_to_iolist([], Acc) ->
  lists:reverse(Acc);
do_convert_to_iolist([H | T], Acc) ->
  do_convert_to_iolist(T, [do_convert_to_iolist(H) | Acc]);
do_convert_to_iolist({Label, Value}, Acc) when is_binary(Label) ->
  [do_convert_to_iolist(Value) | Acc];
do_convert_to_iolist(Other, Acc) ->
  do_convert_to_iolist([], [Other | Acc]).

escape(<<>>) -> <<>>;
escape(<<$\">>) -> <<"\\\"">>;
escape(<<$\n>>) -> <<"\\n">>;
escape(<<$\r>>) -> <<"\\r">>;
escape(<<$\f>>) -> <<"\\f">>;
escape(<<$\t>>) -> <<"\\t">>;
escape(<<$\\>>) -> <<"\\\\">>;
escape($\") -> <<"\\\"">>;
escape($\n) -> <<"\\n">>;
escape($\r) -> <<"\\r">>;
escape($\f) -> <<"\\f">>;
escape($\t) -> <<"\\t">>;
escape($\\) -> <<"\\\\">>;
escape(<<B:1/binary>>) -> B;
escape(<<"\\x", _Rest/binary>> = B) -> B;
escape(<<B:1/binary, Rest/binary>>) -> << (escape(B))/binary
                                        , (escape(Rest))/binary>>;
escape([H|T]) -> lists:flatten([escape(H)] ++ [escape(T)]);
escape([]) -> <<>>;
escape(Other) -> Other.


-spec file(file:name()) -> any().
file(Filename) -> file(Filename, <<"grammar">>).

-spec file(file:name(), binary()) -> any().
file(Filename, Root) ->
  case file:read_file(Filename) of 
    {ok,Bin} -> parse(Bin, Root);
    Err      -> Err
end.

-spec parse(binary() | list()) -> any().
parse(List) when is_list(List) -> parse(list_to_binary(List));
parse(Input) when is_binary(Input) ->
  parse(Input, <<"grammar">>).

-spec parse(binary() | list(), binary()) -> any().
parse(List, Root) when is_list(List) -> 
  parse(list_to_binary(List), Root);
parse(Input, Root) when is_binary(Input) ->
  setup_memo(),
  Result = case pegjs_rule(Root, Input,{{line,1},{column,1}}) of
             {AST, <<>>, _Index}     -> AST;
             {_AST, Unparsed, Index} -> {error, {no_match, {Unparsed, Index}}};
             {error, Error}          -> {error, Error}
           end,
  release_memo(),
Result.

-spec pegjs_rule(binary(), binary(), tuple()) -> any().
pegjs_rule(<<"grammar">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"grammar">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"initializer">>, [pegjs_combinator('suffixed', {optional, [pegjs_combinator('rule_ref', <<"initializer">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"rules">>, [pegjs_combinator('suffixed', {one_or_more, [pegjs_combinator('rule_ref', <<"rule">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_98_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"initializer">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"initializer">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {<<"code">>, [pegjs_combinator('rule_ref', <<"action">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('suffixed', {optional, [pegjs_combinator('rule_ref', <<"semicolon">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_107_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"rule">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"rule">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {<<"name">>, [pegjs_combinator('rule_ref', <<"identifier">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"displayName">>, [pegjs_combinator('suffixed', {optional, [pegjs_combinator('rule_ref', <<"string">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"equals">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"expression">>, [pegjs_combinator('rule_ref', <<"expression">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('suffixed', {optional, [pegjs_combinator('rule_ref', <<"semicolon">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_118_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"expression">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"expression">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"choice">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"choice">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"choice">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {<<"head">>, [pegjs_combinator('rule_ref', <<"sequence">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"tail">>, [pegjs_combinator('suffixed', {zero_or_more, [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"slash">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"sequence">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_137_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"sequence">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"sequence">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {<<"elements">>, [pegjs_combinator('suffixed', {zero_or_more, [pegjs_combinator('rule_ref', <<"labeled">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"code">>, [pegjs_combinator('rule_ref', <<"action">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_153_3/2), pegjs_combinator('sequence', [pegjs_combinator('labeled', {<<"elements">>, [pegjs_combinator('suffixed', {zero_or_more, [pegjs_combinator('rule_ref', <<"labeled">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_166_1/2)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"labeled">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"labeled">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {<<"label">>, [pegjs_combinator('rule_ref', <<"identifier">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"colon">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"expression">>, [pegjs_combinator('rule_ref', <<"prefixed">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_175_3/2), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"prefixed">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_182_1/2)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"prefixed">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"prefixed">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {<<"dollar">>, [pegjs_combinator('rule_ref', <<"dollar">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"expression">>, [pegjs_combinator('rule_ref', <<"suffixed">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_190_3/2), pegjs_combinator('sequence', [pegjs_combinator('labeled', {<<"and">>, [pegjs_combinator('rule_ref', <<"and">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"code">>, [pegjs_combinator('rule_ref', <<"action">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_200_3/2), pegjs_combinator('sequence', [pegjs_combinator('labeled', {<<"and">>, [pegjs_combinator('rule_ref', <<"and">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"expression">>, [pegjs_combinator('rule_ref', <<"suffixed">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_208_3/2), pegjs_combinator('sequence', [pegjs_combinator('labeled', {<<"not">>, [pegjs_combinator('rule_ref', <<"not">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"code">>, [pegjs_combinator('rule_ref', <<"action">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_218_3/2), pegjs_combinator('sequence', [pegjs_combinator('labeled', {<<"not">>, [pegjs_combinator('rule_ref', <<"not">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"expression">>, [pegjs_combinator('rule_ref', <<"suffixed">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_226_3/2), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"suffixed">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"suffixed">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"suffixed">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {<<"expression">>, [pegjs_combinator('rule_ref', <<"primary">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"suffix">>, [pegjs_combinator('rule_ref', <<"question">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_237_3/2), pegjs_combinator('sequence', [pegjs_combinator('labeled', {<<"expression">>, [pegjs_combinator('rule_ref', <<"primary">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"suffix">>, [pegjs_combinator('rule_ref', <<"star">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_245_3/2), pegjs_combinator('sequence', [pegjs_combinator('labeled', {<<"expression">>, [pegjs_combinator('rule_ref', <<"primary">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"suffix">>, [pegjs_combinator('rule_ref', <<"plus">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_253_3/2), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"primary">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"primary">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"primary">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {<<"name">>, [pegjs_combinator('rule_ref', <<"identifier">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('suffixed', {optional, [pegjs_combinator('rule_ref', <<"string">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"equals">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_263_3/2), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"literal">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"class">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"dot">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_269_3/2), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"lparen">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"expression">>, [pegjs_combinator('rule_ref', <<"expression">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"rparen">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_277_1/2)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"action">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"action">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {<<"braced">>, [pegjs_combinator('rule_ref', <<"braced">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_284_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"braced">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"braced">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("{")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('suffixed', {zero_or_more, [pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"braced">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"nonBraceCharacters">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("}")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_291_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"nonBraceCharacters">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"nonBraceCharacters">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('suffixed', {one_or_more, [pegjs_combinator('rule_ref', <<"nonBraceCharacter">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"nonBraceCharacter">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"nonBraceCharacter">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('regexp', {[unicode:characters_to_binary("[^{}]")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"equals">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"equals">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("=")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_298_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"colon">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"colon">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary(":")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_299_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"semicolon">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"semicolon">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary(";")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_300_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"slash">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"slash">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("/")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_301_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"and">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"and">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("&")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_302_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"not">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"not">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("!")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_303_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"dollar">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"dollar">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("$")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_304_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"question">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"question">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("?")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_305_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"star">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"star">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("*")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_306_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"plus">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"plus">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("+")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_307_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"lparen">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"lparen">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("(")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_308_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"rparen">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"rparen">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary(")")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_309_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"dot">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"dot">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary(".")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_330_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"identifier">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"identifier">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {<<"chars">>, [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"letter">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("_")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('suffixed', {zero_or_more, [pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"letter">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"digit">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("_")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_341_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"literal">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"literal">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {<<"value">>, [pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"doubleQuotedString">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"singleQuotedString">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"flags">>, [pegjs_combinator('suffixed', {optional, [pegjs_combinator('literal', {[unicode:characters_to_binary("i")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_350_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"string">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"string">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {<<"string">>, [pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"doubleQuotedString">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"singleQuotedString">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_355_4/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"doubleQuotedString">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"doubleQuotedString">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\"")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"chars">>, [pegjs_combinator('suffixed', {zero_or_more, [pegjs_combinator('rule_ref', <<"doubleQuotedCharacter">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\"")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_364_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"doubleQuotedCharacter">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"doubleQuotedCharacter">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"simpleDoubleQuotedCharacter">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"simpleEscapeSequence">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"zeroEscapeSequence">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"hexEscapeSequence">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"unicodeEscapeSequence">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"eolEscapeSequence">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"simpleDoubleQuotedCharacter">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"simpleDoubleQuotedCharacter">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\"")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\\")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"eolChar">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"char_">>, [pegjs_combinator('anything', [], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_379_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"singleQuotedString">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"singleQuotedString">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("'")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"chars">>, [pegjs_combinator('suffixed', {zero_or_more, [pegjs_combinator('rule_ref', <<"singleQuotedCharacter">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("'")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_386_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"singleQuotedCharacter">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"singleQuotedCharacter">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"simpleSingleQuotedCharacter">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"simpleEscapeSequence">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"zeroEscapeSequence">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"hexEscapeSequence">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"unicodeEscapeSequence">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"eolEscapeSequence">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"simpleSingleQuotedCharacter">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"simpleSingleQuotedCharacter">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("'")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\\")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"eolChar">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"char_">>, [pegjs_combinator('anything', [], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_401_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"class">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"class">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("[")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"inverted">>, [pegjs_combinator('suffixed', {optional, [pegjs_combinator('literal', {[unicode:characters_to_binary("^")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"parts">>, [pegjs_combinator('suffixed', {zero_or_more, [pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"classCharacterRange">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"classCharacter">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("]")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"flags">>, [pegjs_combinator('suffixed', {optional, [pegjs_combinator('literal', {[unicode:characters_to_binary("i")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_424_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"classCharacterRange">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"classCharacterRange">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {<<"begin">>, [pegjs_combinator('rule_ref', <<"classCharacter">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("-")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"end">>, [pegjs_combinator('rule_ref', <<"classCharacter">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_442_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"classCharacter">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"classCharacter">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"bracketDelimitedCharacter">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_452_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"bracketDelimitedCharacter">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"bracketDelimitedCharacter">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"simpleBracketDelimitedCharacter">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"simpleEscapeSequence">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"zeroEscapeSequence">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"hexEscapeSequence">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"unicodeEscapeSequence">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"eolEscapeSequence">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"simpleBracketDelimitedCharacter">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"simpleBracketDelimitedCharacter">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("]")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\\")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"eolChar">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"char_">>, [pegjs_combinator('anything', [], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_467_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"simpleEscapeSequence">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"simpleEscapeSequence">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\\")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"digit">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("x")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("u")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"eolChar">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"char_">>, [pegjs_combinator('anything', [], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_482_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"zeroEscapeSequence">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"zeroEscapeSequence">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\\0")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"digit">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_488_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"hexEscapeSequence">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"hexEscapeSequence">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\\x")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"digits">>, [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"hexDigit">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"hexDigit">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_497_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"unicodeEscapeSequence">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"unicodeEscapeSequence">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\\u")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"digits">>, [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"hexDigit">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"hexDigit">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"hexDigit">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"hexDigit">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_506_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"eolEscapeSequence">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"eolEscapeSequence">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\\")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"eol">>, [pegjs_combinator('rule_ref', <<"eol">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_511_4/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"digit">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"digit">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('regexp', {[unicode:characters_to_binary("[0-9]")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"hexDigit">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"hexDigit">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('regexp', {[unicode:characters_to_binary("[0-9a-fA-F]")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"letter">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"letter">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"lowerCaseLetter">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"upperCaseLetter">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"lowerCaseLetter">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"lowerCaseLetter">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('regexp', {[unicode:characters_to_binary("[a-z]")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"upperCaseLetter">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"upperCaseLetter">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('regexp', {[unicode:characters_to_binary("[A-Z]")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"__">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"__">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('suffixed', {zero_or_more, [pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"whitespace">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"eol">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"comment">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"comment">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"comment">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"singleLineComment">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"multiLineComment">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"singleLineComment">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"singleLineComment">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("//")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('suffixed', {zero_or_more, [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"eolChar">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('anything', [], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"multiLineComment">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"multiLineComment">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("/*")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('suffixed', {zero_or_more, [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('literal', {[unicode:characters_to_binary("*/")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('anything', [], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("*/")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"eol">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"eol">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\n")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\r\n")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\r")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('regexp', {[unicode:characters_to_binary("[\x{2028}]")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('regexp', {[unicode:characters_to_binary("[\x{2029}]")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"eolChar">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"eolChar">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('regexp', {[unicode:characters_to_binary("[\\n\\r\x{2028}\x{2029}]")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"whitespace">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"whitespace">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('regexp', {[unicode:characters_to_binary("[ \\t\\v\\f\x{00A0}\x{FEFF}\x{1680}\x{180E}\x{2000}-\x{200A}\x{202F}\x{205F}\x{3000}]")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       ).

-spec pegjs_code_98_1(iolist(), index()) -> parse_result().
pegjs_code_98_1(Node, Idx) ->

    [_, {_, Initializer}, {_, Rules}] = Node,
    #grammar{ initializer = Initializer
            , rules       = Rules
            , index       = Idx
            }
  .

-spec pegjs_code_107_1(iolist(), index()) -> parse_result().
pegjs_code_107_1(Node, Idx) ->

    [{_, Code}, _] = Node,
    #code{ code  = Code
         , index = Idx
         }
  .

-spec pegjs_code_118_1(iolist(), index()) -> parse_result().
pegjs_code_118_1(Node, Idx) ->

    [{_, Name}, {_, DisplayName}, _, {_, Expression}, _] = Node,
    #rule{ name         = Name
         , display_name = DisplayName
         , expression   = Expression
         , index        = Idx
         }
  .

-spec pegjs_code_137_1(iolist(), index()) -> parse_result().
pegjs_code_137_1(Node, Idx) ->

    [{_, Head}, {_, Tail}] = Node,
    case Tail of
      [] ->
        Head;
      _List ->
        Alternatives0 = [Sequence || [_, Sequence] <- Tail],
        Alternatives = [Head | Alternatives0],
        #choice{ alternatives = Alternatives
               , index        = Idx
               }
    end
  .

-spec pegjs_code_153_3(iolist(), index()) -> parse_result().
pegjs_code_153_3(Node, Idx) ->

       [{_, Elements}, {_, Code}] = Node,
       Expression = case Elements of
                      [OneElement] -> OneElement;
                      _ ->
                       Elements
                    end,
       #sequence{ elements = Expression
                , code     = #code{ code  = Code
                                  , index = Idx
                                  }
                , index    = Idx
                }
    .

-spec pegjs_code_166_1(iolist(), index()) -> parse_result().
pegjs_code_166_1(Node, Idx) ->

    [{_, Elements}] = Node,
    Expression = case Elements of
                   [OneElement] -> OneElement;
                   _ ->
                    Elements
                 end,
    #sequence{ elements = Expression
             , index    = Idx
             }
  .

-spec pegjs_code_175_3(iolist(), index()) -> parse_result().
pegjs_code_175_3(Node, Idx) ->

      [{_, Label}, _, {_, Expression}] = Node,
      #labeled{ label      = Label
              , expression = Expression
              , index      = Idx
              }
    .

-spec pegjs_code_182_1(iolist(), index()) -> parse_result().
pegjs_code_182_1(Node, Idx) ->

      #labeled{ expression = Node
              , index      = Idx
              }
    .

-spec pegjs_code_190_3(iolist(), index()) -> parse_result().
pegjs_code_190_3(Node, Idx) ->

      [_, {_, Expression}] = Node,
      #text{ expression = Expression
           , index      = Idx
           }
    .

-spec pegjs_code_200_3(iolist(), index()) -> parse_result().
pegjs_code_200_3(Node, Idx) ->

      [_, {_, Code}] = Node,
      #prefixed{ type  = semantic_and
               , code  = #code{ code  = Code
                              , index = Idx
                              }
               , index = Idx
               }
    .

-spec pegjs_code_208_3(iolist(), index()) -> parse_result().
pegjs_code_208_3(Node, Idx) ->

      [_, {_, Expression}] = Node,
      #prefixed{ type       = simple_and
               , expression = Expression
               , index      = Idx
               }
    .

-spec pegjs_code_218_3(iolist(), index()) -> parse_result().
pegjs_code_218_3(Node, Idx) ->

      [_, {_, Code}] = Node,
      #prefixed{ type  = semantic_not
               , code  = #code{ code  = Code
                              , index = Idx
                              }
               , index = Idx
               }
    .

-spec pegjs_code_226_3(iolist(), index()) -> parse_result().
pegjs_code_226_3(Node, Idx) ->

      [_, {_, Expression}] = Node,
      #prefixed{ type       = simple_not
               , expression = Expression
               , index      = Idx
               }
    .

-spec pegjs_code_237_3(iolist(), index()) -> parse_result().
pegjs_code_237_3(Node, Idx) ->

      [{_, Expression}, _] = Node,
      #suffixed{ type       = optional
               , expression = Expression
               , index      = Idx
               }
    .

-spec pegjs_code_245_3(iolist(), index()) -> parse_result().
pegjs_code_245_3(Node, Idx) ->

      [{_, Expression}, _] = Node,
      #suffixed{ type       = zero_or_more
               , expression = Expression
               , index      = Idx
               }
    .

-spec pegjs_code_253_3(iolist(), index()) -> parse_result().
pegjs_code_253_3(Node, Idx) ->

      [{_, Expression}, _] = Node,
      #suffixed{ type       = one_or_more
               , expression = Expression
               , index      = Idx
               }
    .

-spec pegjs_code_263_3(iolist(), index()) -> parse_result().
pegjs_code_263_3(Node, Idx) ->

      [{_, Name}, _] = Node,
      #rule_ref{ name  = Name
               , index = Idx
               }
    .

-spec pegjs_code_269_3(iolist(), index()) -> parse_result().
pegjs_code_269_3(_Node, Idx) ->

      #anything{ index = Idx }
    .

-spec pegjs_code_277_1(iolist(), index()) -> parse_result().
pegjs_code_277_1(Node, _Idx) ->

      [_, {_, Expression}, _] = Node,
      Expression
    .

-spec pegjs_code_284_1(iolist(), index()) -> parse_result().
pegjs_code_284_1(Node, _Idx) ->

    [{_, Braced}, _] = Node,
    binary:part(Braced, 1, size(Braced) - 2)
  .

-spec pegjs_code_291_1(iolist(), index()) -> parse_result().
pegjs_code_291_1(Node, _Idx) ->

    iolist_to_binary(Node)
  .

-spec pegjs_code_298_1(iolist(), index()) -> parse_result().
pegjs_code_298_1(_Node, _Idx) ->
 <<"=">> .

-spec pegjs_code_299_1(iolist(), index()) -> parse_result().
pegjs_code_299_1(_Node, _Idx) ->
 <<":">> .

-spec pegjs_code_300_1(iolist(), index()) -> parse_result().
pegjs_code_300_1(_Node, _Idx) ->
 <<";">> .

-spec pegjs_code_301_1(iolist(), index()) -> parse_result().
pegjs_code_301_1(_Node, _Idx) ->
 <<"/">> .

-spec pegjs_code_302_1(iolist(), index()) -> parse_result().
pegjs_code_302_1(_Node, _Idx) ->
 <<"&">> .

-spec pegjs_code_303_1(iolist(), index()) -> parse_result().
pegjs_code_303_1(_Node, _Idx) ->
 <<"!">> .

-spec pegjs_code_304_1(iolist(), index()) -> parse_result().
pegjs_code_304_1(_Node, _Idx) ->
 <<"$">> .

-spec pegjs_code_305_1(iolist(), index()) -> parse_result().
pegjs_code_305_1(_Node, _Idx) ->
 <<"?">> .

-spec pegjs_code_306_1(iolist(), index()) -> parse_result().
pegjs_code_306_1(_Node, _Idx) ->
 <<"*">> .

-spec pegjs_code_307_1(iolist(), index()) -> parse_result().
pegjs_code_307_1(_Node, _Idx) ->
 <<"+">> .

-spec pegjs_code_308_1(iolist(), index()) -> parse_result().
pegjs_code_308_1(_Node, _Idx) ->
 <<"(">> .

-spec pegjs_code_309_1(iolist(), index()) -> parse_result().
pegjs_code_309_1(_Node, _Idx) ->
 <<")">> .

-spec pegjs_code_330_1(iolist(), index()) -> parse_result().
pegjs_code_330_1(_Node, _Idx) ->
 <<".">> .

-spec pegjs_code_341_1(iolist(), index()) -> parse_result().
pegjs_code_341_1(Node, _Idx) ->

    [{_, Chars}, _] = Node,
    iolist_to_binary(do_convert_to_iolist(Chars))
  .

-spec pegjs_code_350_1(iolist(), index()) -> parse_result().
pegjs_code_350_1(Node, _Idx) ->

    [{_, [Value]}, {_, Flags}, _] = Node,
    #literal{ value       = escape(lists:flatten(Value))
            , ignore_case = Flags == <<"i">>
            }
  .

-spec pegjs_code_355_4(iolist(), index()) -> parse_result().
pegjs_code_355_4(Node, _Idx) ->

    [{_, String}, _] = Node,
    String
  .

-spec pegjs_code_364_1(iolist(), index()) -> parse_result().
pegjs_code_364_1(Node, _Idx) ->

    [_, {_, Chars}, _] = Node,
    Chars
  .

-spec pegjs_code_379_1(iolist(), index()) -> parse_result().
pegjs_code_379_1(Node, _Idx) ->

    [_, {_, Char_}] = Node,
    Char_
  .

-spec pegjs_code_386_1(iolist(), index()) -> parse_result().
pegjs_code_386_1(Node, _Idx) ->

    [_, {_, Chars}, _] = Node,
    Chars
  .

-spec pegjs_code_401_1(iolist(), index()) -> parse_result().
pegjs_code_401_1(Node, _Idx) ->

    [_, {_, Char_}] = Node,
    Char_
  .

-spec pegjs_code_424_1(iolist(), index()) -> parse_result().
pegjs_code_424_1(Node, Idx) ->

    [_, {_, Inverted0}, {_, Parts0}, _, {_, Flags0}, _] = Node,
    Inverted = case Inverted0 of [] -> <<>>; I -> I end,
    Parts = lists:flatten(Parts0),
    Flags    = case Flags0 of [] -> <<>>; F -> F end,

    PartsConverted = lists:map(fun(Part) -> get_value(data, Part) end, Parts),
    PartsRawText   = << <<(get_attr(raw_text, Part))/binary>> || Part <- Parts>>,
    RawText = << "["
               , Inverted/binary
               , PartsRawText/binary
               , "]"
              >>,
    #regexp{ parts       = PartsConverted
           , raw_text    = RawText
           , inverted    = Inverted == <<"^">>
           , ignore_case = Flags == <<"i">>
           , index       = Idx
           }
  .

-spec pegjs_code_442_1(iolist(), index()) -> parse_result().
pegjs_code_442_1(Node, Idx) ->

    [{_, Begin}, _, {_, End}] = Node,
    RawBegin = get_attr(raw_text, Begin),
    RawEnd   = get_attr(raw_text, End),
    case Begin > End of
      true ->
        error({invalid_character_range, {RawBegin, RawEnd, Idx}});
      false ->
        #character_range{ 'begin'  = get_value(data, Begin)
                        , 'end'    = get_value(data, End)
                        , raw_text = <<RawBegin/binary, "-", RawEnd/binary>>
                        , index    = Idx
                        }
    end
  .

-spec pegjs_code_452_1(iolist(), index()) -> parse_result().
pegjs_code_452_1(Node, Idx) ->

    [[Data]] = Node,
    #charclass{ data     = Data
              , raw_text = quote_for_regexp_class(Data)
              , index    = Idx
              }
  .

-spec pegjs_code_467_1(iolist(), index()) -> parse_result().
pegjs_code_467_1(Node, _Idx) ->

    [_, {_, Char_}] = Node,
    Char_
  .

-spec pegjs_code_482_1(iolist(), index()) -> parse_result().
pegjs_code_482_1(Node, _Idx) ->

    [_, _, {_, Char_}] = Node,
    case Char_ of
      <<"b">> -> <<"\b">>;
      <<"f">> -> <<"\f">>;
      <<"n">> -> <<"\n">>;
      <<"r">> -> <<"\r">>;
      <<"t">> -> <<"\t">>;
      <<"v">> -> <<"\v">>; %% or "\x{0B}"
      _       -> Char_
    end
  .

-spec pegjs_code_488_1(iolist(), index()) -> parse_result().
pegjs_code_488_1(_Node, _Idx) ->

    "\x{00}"
  .

-spec pegjs_code_497_1(iolist(), index()) -> parse_result().
pegjs_code_497_1(Node, _Idx) ->

    [_, {_, Digits0}] = Node,
    Digits = lists:foldl( fun(D, Acc) -> <<Acc/binary, D/binary>> end
                        , <<>>, lists:flatten(Digits0)),
    <<"\\x{", Digits/binary, "}">>
  .

-spec pegjs_code_506_1(iolist(), index()) -> parse_result().
pegjs_code_506_1(Node, _Idx) ->

    [_, {_, Digits0}] = Node,
    Digits = lists:foldl( fun(D, Acc) -> <<Acc/binary, D/binary>> end
                        , <<>>, lists:flatten(Digits0)),
    <<"\\x{", Digits/binary, "}">>
  .

-spec pegjs_code_511_4(iolist(), index()) -> parse_result().
pegjs_code_511_4(Node, _Idx) ->

    [_, {_, Eol}] = Node,
    Eol
  .

%% -type index() :: {{line, pos_integer()}, {column, pos_integer()}}.
-type input() :: binary().
-ifndef(index).
-type index() :: {{line, integer()}, {column, integer()}}.
-endif.
-type parse_failure() :: {error, term()}.
-type parse_success() :: {term(), input(), index()}.
-type parse_result() :: parse_failure() | parse_success().
-type parse_fun() :: fun((input(), index()) -> parse_result()).
-type xform_fun() :: fun((input(), index()) -> term()).

-spec pegjs(input(), index(), binary(), parse_fun(), xform_fun()) -> parse_result().
pegjs(Inp, StartIndex, Name, ParseFun, TransformFun) ->
  case get_memo(StartIndex, Name) of      % See if the current reduction is memoized
    {ok, Memo} -> %Memo;                     % If it is, return the stored result
      Memo;
    _ ->                                        % If not, attempt to parse
      Result = case ParseFun(Inp, StartIndex) of
        {error,_} = Failure ->                       % If it fails, memoize the failure
          Failure;
        {Match, InpRem, NewIndex} ->               % If it passes, transform and memoize the result.
          Transformed = TransformFun(Match, StartIndex),
          {Transformed, InpRem, NewIndex}
      end,
      memoize(StartIndex, Name, Result),
      Result
  end.

-spec setup_memo() -> ets:tid().
setup_memo() ->
  put({parse_memo_table, ?MODULE}, ets:new(?MODULE, [set])).

-spec release_memo() -> true.
release_memo() ->
  ets:delete(memo_table_name()).

-spec memoize(index(), atom(), term()) -> true.
memoize(Index, Name, Result) ->
  Memo = case ets:lookup(memo_table_name(), Index) of
              [] -> [];
              [{Index, Plist}] -> Plist
         end,
  ets:insert(memo_table_name(), {Index, [{Name, Result}|Memo]}).

-spec get_memo(index(), atom()) -> {ok, term()} | {error, not_found}.
get_memo(Index, Name) ->
  case ets:lookup(memo_table_name(), Index) of
    [] -> {error, not_found};
    [{Index, Plist}] ->
      case proplists:lookup(Name, Plist) of
        {Name, Result}  -> {ok, Result};
        _  -> {error, not_found}
      end
    end.

-spec memo_table_name() -> ets:tid().
memo_table_name() ->
    get({parse_memo_table, ?MODULE}).

%%_* Implemented combinators ===================================================

pegjs_combinator('anything', _, _) ->
  fun(<<>>, Index) -> {error, {expected, any_character, Index}};
     (Input, Index) when is_binary(Input) ->
          <<C/utf8, Rest/binary>> = Input,
          {<<C/utf8>>, Rest, p_advance_index(<<C/utf8>>, Index)}
  end;

pegjs_combinator('choice', Parsers, TransformFun) ->
  fun(Input, Index) ->
      pegjs_combinator('attempt', {Parsers, Input, Index, none}, TransformFun)
  end;

pegjs_combinator('attempt', {[], _Input, _Index, Failure}, _) -> Failure;
pegjs_combinator('attempt', {[P|Parsers], Input, Index, FirstFailure}, TransformFun)->
  case P(Input, Index) of
    {error, _} = Failure ->
      case FirstFailure of
        none -> pegjs_combinator('attempt', {Parsers, Input, Index, Failure}, TransformFun);
        _ -> pegjs_combinator('attempt', {Parsers, Input, Index, FirstFailure}, TransformFun)
      end;
    Result -> Result
  end;

pegjs_combinator('sequence', P, TransformFun) ->
  fun(Input, Index) ->
      pegjs_combinator('all', {P, Input, Index, []}, TransformFun)
  end;

pegjs_combinator('all', {[], Inp, Index, Accum}, TransformFun) ->
  Result = lists:reverse(Accum),
  case TransformFun(Result, Index) of
    {error, _} = E -> E;
    Node -> {Node, Inp, Index}
  end;
pegjs_combinator('all', {[P|Parsers], Inp, Index, Accum}, TransformFun) ->
  case P(Inp, Index) of
    {error, _} = Failure -> Failure;
    {Result, InpRem, NewIndex} -> pegjs_combinator('all', {Parsers, InpRem, NewIndex, [Result|Accum]}, TransformFun)
  end;

pegjs_combinator('text', P, _) ->
  fun(Input, Index) ->
      Result = P(Input, Index),
      case Result of
        {error, _} = E -> E;
        {MaybeIoList, NewInput, NewIndex} ->
          IoList = convert_to_iolist(MaybeIoList),
          {iolist_to_binary(IoList), NewInput, NewIndex}
      end
  end;

pegjs_combinator('labeled', {Tag, [P]}, _) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {error,_} = Failure ->
           Failure;
        {Result, InpRem, NewIndex} ->
          case Tag of
            undefined -> {Result, InpRem, NewIndex};
            _         -> {{Tag, Result}, InpRem, NewIndex}
          end
      end
  end;

pegjs_combinator('prefixed', {simple_not, [P]}, _) ->
  fun(Input, Index)->
      case P(Input,Index) of
        {error,_} ->
          {[], Input, Index};
        {Result, _, _} -> {error, {expected, {no_match, Result}, Index}}
      end
  end;
pegjs_combinator('prefixed', {simple_and, [P]}, _) ->
  fun(Input, Index) ->
    case P(Input, Index) of
      {error, _} = Failure -> Failure;
      _ -> {[], Input, Index}
    end
  end;
pegjs_combinator('prefixed', {semantic_and, _}, TransformFun) ->
  fun(Input, Index) ->
    case TransformFun(Input, Index) of
      {error, Reason} -> {error, {Reason, Index}};
      _ -> {[], Input, Index}
    end
  end;
pegjs_combinator('prefixed', {semantic_not, _}, TransformFun) ->
  fun(Input, Index) ->
    case TransformFun(Input, Index) of
      {error, _} -> {[], Input, Index};
      Result -> {error, {expected, {no_match, Result}, Index}}
    end
  end;

pegjs_combinator('suffixed', {zero_or_more, [P]}, TransformFun) ->
  fun(Input, Index) ->
      pegjs_combinator('scan', {P, Input, Index, []}, TransformFun)
  end;
pegjs_combinator('suffixed', {one_or_more, [P]}, TransformFun) ->
  fun(Input, Index)->
      Result = pegjs_combinator('scan', {P, Input, Index, []}, TransformFun),
      case Result of
        {[_|_], _, _} ->
          Result;
        _ ->
          {error, {_, Failure, _}} = P(Input,Index),
          {error, {expected, {at_least_one, Failure}, Index}}
      end
  end;
pegjs_combinator('suffixed', {optional, [P]}, _) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {error,_} -> {[], Input, Index};
        {_, _, _} = Success -> Success
      end
  end;

pegjs_combinator('scan', { _, [], Index, Accum}, _) -> {lists:reverse( Accum ), [], Index};
pegjs_combinator('scan', {P, Inp, Index, Accum}, TransformFun) ->
  case P(Inp, Index) of
    {error,_N} -> {lists:reverse(Accum), Inp, Index};
    {Result, InpRem, NewIndex} -> pegjs_combinator('scan', {P, InpRem, NewIndex, [Result | Accum]}, TransformFun)
  end;

pegjs_combinator('literal', {[String], IsCaseInsensitive}, _) ->
    Length = erlang:byte_size(String),
    fun(Input, Index) ->
      case Input of
        <<String:Length/binary, Rest/binary>> ->
          {String, Rest, p_advance_index(String, Index)};
        _ ->
          Modifier = case IsCaseInsensitive of
                       true  -> case_insensitive;
                       false -> case_sensitive
                     end,
          case IsCaseInsensitive of
            false ->
              {error, {match_failed, {{String, Modifier}, Input}, Index}};
            true ->
              case re:run( Input
                         , <<"^[", String/binary, "](.*)">>
                         , [caseless]) of
                {match, [{_, Len}, _]} ->
                  <<Result:Len/binary, Rest/binary>> = Input,
                  { Result
                  , Rest
                  , p_advance_index(Result, Index)
                  };
                {match, _} ->
                  { Input
                  , <<>>
                  , p_advance_index(String, Index)
                  };
                nomatch    ->
                  {error, {match_failed, {{String, Modifier}, Input}, Index}}
              end
          end
      end
    end;

pegjs_combinator('regexp', {Regexp0, IsIgnoreCase}, _) ->
    Regexp = unicode:characters_to_binary(Regexp0),
    Options0 = [unicode, dotall, anchored],
    Options = case IsIgnoreCase of
                true  -> [caseless | Options0];
                false -> Options0
              end,
    {ok, RE} = re:compile(Regexp, Options),
    fun(Inp, Index) ->
        case re:run(Inp, RE) of
            {match, [{0, Length}|_]} ->
                {Head, Tail} = erlang:split_binary(Inp, Length),
                {Head, Tail, p_advance_index(Head, Index)};
            _ ->
              Modifier = case IsIgnoreCase of
                             true  -> case_insensitive;
                             false -> case_sensitive
                           end,
              {error, { expected
                      , {regexp, binary_to_list(Regexp), Modifier}
                      , Index}}
        end
    end;

pegjs_combinator('rule_ref', Rule, _) ->
    fun(Input, Index) ->
        pegjs_rule(Rule, Input, Index)
    end;

pegjs_combinator(Name, _, _) ->
  {error, {invalid_combinator, Name}}.

p_advance_index(MatchedInput, Index) when is_list(MatchedInput) orelse is_binary(MatchedInput)-> % strings
  lists:foldl(fun p_advance_index/2, Index, unicode:characters_to_list(MatchedInput));
p_advance_index(MatchedInput, Index) when is_integer(MatchedInput) -> % single characters
  {{line, Line}, {column, Col}} = Index,
  case MatchedInput of
    $\n -> {{line, Line+1}, {column, 1}};
    _ -> {{line, Line}, {column, Col+1}}
  end.

convert_to_iolist(MaybeIoList) ->
  convert_to_iolist(MaybeIoList, []).

convert_to_iolist([], Acc) ->
  lists:reverse(Acc);
convert_to_iolist([H | T], Acc) ->
  convert_to_iolist(T, [convert_to_iolist(H) | Acc]);
convert_to_iolist({Label, Value}, Acc) when is_binary(Label) ->
  [convert_to_iolist(Value) | Acc];
convert_to_iolist(Other, Acc) ->
  convert_to_iolist([], [Other | Acc]).
