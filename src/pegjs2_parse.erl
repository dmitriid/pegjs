-module(pegjs2_parse).
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



-include("pegjs2.hrl").

int(C) when $0 =< C, C =< $9 ->
    C - $0;
int(C) when $A =< C, C =< $F ->
    C - $A + 10;
int(C) when $a =< C, C =< $f ->
    C - $a + 10.

%% if it's in the form of \u00xx, skip the first two zeroes
hexstr_to_bin(<<"0", "0", T/binary>>) ->
    hexstr_to_bin(T);
hexstr_to_bin(<<X/integer,Y/integer, T/binary>>) ->
  Int = int(X)*16 + int(Y),
  <<Int/integer
  , (hexstr_to_bin(T))/binary
  >>;
hexstr_to_bin(<<>>) ->
  <<>>.

filter_empty_strings(Strings) ->
  filter_empty_strings(Strings, []).

filter_empty_strings([], Acc) ->
  lists:reverse(Acc);
filter_empty_strings([H|T], Acc) ->
  filter_empty_strings(T, [filter_empty_strings(H) | Acc]);
filter_empty_strings(<<>>, Acc) ->
  lists:reverse(Acc);
filter_empty_strings(Binary, _) when is_binary(Binary) ->
  Binary.

text(Node) ->
  TextNode = [T || Match <- Node
                 , T <- case Match of {_, Part} -> [Part]; Part -> [Part] end
             ],
  iolist_to_binary(TextNode).

entries(#entry{} = E) ->
  E;
entries(List) ->
  [E || E = #entry{} <- lists:flatten(List)].

ops_to_prefixed_types([H|_])   -> ops_to_prefixed_types(H);
ops_to_prefixed_types(<<"$">>) -> <<"text">>;
ops_to_prefixed_types(<<"&">>) -> <<"simple_and">>;
ops_to_prefixed_types(<<"!">>) -> <<"simple_not">>.

ops_to_suffixed_types([H|_])   -> ops_to_suffixed_types(H);
ops_to_suffixed_types(<<"?">>) -> <<"optional">>;
ops_to_suffixed_types(<<"*">>) -> <<"zero_or_more">>;
ops_to_suffixed_types(<<"+">>) -> <<"one_or_more">>.

ops_to_semantic_predicate_types([H|_])   -> ops_to_semantic_predicate_types(H);
ops_to_semantic_predicate_types(<<"&">>) -> <<"semantic_and">>;
ops_to_semantic_predicate_types(<<"!">>) -> <<"semantic_not">>.


-spec file(file:name()) -> any().
file(Filename) -> file(Filename, <<"Grammar">>).

-spec file(file:name(), binary()) -> any().
file(Filename, Root) ->
  case file:read_file(Filename) of 
    {ok,Bin} -> parse(Bin, Root);
    Err      -> Err
end.

-spec parse(binary() | list()) -> any().
parse(List) when is_list(List) -> parse(list_to_binary(List));
parse(Input) when is_binary(Input) ->
  parse(Input, <<"Grammar">>).

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
pegjs_rule(<<"Grammar">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"Grammar">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"initializer">>, [pegjs_combinator('suffixed', {optional, [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"Initializer">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"rules">>, [pegjs_combinator('suffixed', {one_or_more, [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"Rule">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_98_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"Initializer">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"Initializer">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {<<"code">>, [pegjs_combinator('rule_ref', <<"CodeBlock">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"EOS">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_101_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"Rule">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"Rule">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {<<"name">>, [pegjs_combinator('rule_ref', <<"IdentifierName">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"displayName">>, [pegjs_combinator('suffixed', {optional, [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"StringLiteral">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("=")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"expression">>, [pegjs_combinator('rule_ref', <<"Expression">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"EOS">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_124_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"Expression">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"Expression">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"ChoiceExpression">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"ChoiceExpression">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"ChoiceExpression">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {<<"first">>, [pegjs_combinator('rule_ref', <<"ActionExpression">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"rest">>, [pegjs_combinator('suffixed', {zero_or_more, [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("/")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"ActionExpression">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_141_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"ActionExpression">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"ActionExpression">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {<<"expression">>, [pegjs_combinator('rule_ref', <<"SequenceExpression">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"code">>, [pegjs_combinator('suffixed', {optional, [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"CodeBlock">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_156_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"SequenceExpression">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"SequenceExpression">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {<<"first">>, [pegjs_combinator('rule_ref', <<"LabeledExpression">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"rest">>, [pegjs_combinator('suffixed', {zero_or_more, [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"LabeledExpression">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_170_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"LabeledExpression">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"LabeledExpression">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {<<"label">>, [pegjs_combinator('rule_ref', <<"Identifier">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary(":")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"expression">>, [pegjs_combinator('rule_ref', <<"PrefixedExpression">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_179_3/2), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"PrefixedExpression">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"PrefixedExpression">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"PrefixedExpression">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {<<"operator">>, [pegjs_combinator('rule_ref', <<"PrefixedOperator">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"expression">>, [pegjs_combinator('rule_ref', <<"SuffixedExpression">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_189_3/2), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"SuffixedExpression">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"PrefixedOperator">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"PrefixedOperator">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("$")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("&")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("!")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"SuffixedExpression">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"SuffixedExpression">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {<<"expression">>, [pegjs_combinator('rule_ref', <<"PrimaryExpression">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"operator">>, [pegjs_combinator('rule_ref', <<"SuffixedOperator">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_204_3/2), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"PrimaryExpression">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"SuffixedOperator">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"SuffixedOperator">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("?")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("*")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("+")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"PrimaryExpression">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"PrimaryExpression">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"LiteralMatcher">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"CharacterClassMatcher">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"AnyMatcher">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"RuleReferenceExpression">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"SemanticPredicateExpression">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("(")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"expression">>, [pegjs_combinator('rule_ref', <<"Expression">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary(")")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_219_1/2)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"RuleReferenceExpression">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"RuleReferenceExpression">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {<<"name">>, [pegjs_combinator('rule_ref', <<"IdentifierName">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('suffixed', {optional, [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"StringLiteral">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("=")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_228_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"SemanticPredicateExpression">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"SemanticPredicateExpression">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {<<"operator">>, [pegjs_combinator('rule_ref', <<"SemanticPredicateOperator">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"code">>, [pegjs_combinator('rule_ref', <<"CodeBlock">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_237_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"SemanticPredicateOperator">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"SemanticPredicateOperator">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("&")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("!")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"SourceCharacter">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"SourceCharacter">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('anything', [], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"WhiteSpace">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"WhiteSpace">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\t")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\f")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary(" ")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\x{00A0}")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\x{FEFF}")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"Zs">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"LineTerminator">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"LineTerminator">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('regexp', {[unicode:characters_to_binary("[\\n\\r\x{2028}\x{2029}]")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"LineTerminatorSequence">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"LineTerminatorSequence">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\n")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\r\n")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\r")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\x{2028}")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\x{2029}")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"Comment">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"Comment">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"MultiLineComment">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"SingleLineComment">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"MultiLineComment">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"MultiLineComment">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("/*")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('suffixed', {zero_or_more, [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('literal', {[unicode:characters_to_binary("*/")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"SourceCharacter">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("*/")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"MultiLineCommentNoLineTerminator">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"MultiLineCommentNoLineTerminator">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("/*")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('suffixed', {zero_or_more, [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("*/")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"LineTerminator">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"SourceCharacter">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("*/")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"SingleLineComment">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"SingleLineComment">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("//")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('suffixed', {zero_or_more, [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"LineTerminator">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"SourceCharacter">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"Identifier">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"Identifier">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"ReservedWord">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"name">>, [pegjs_combinator('rule_ref', <<"IdentifierName">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_281_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"IdentifierName">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"IdentifierName">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {<<"first">>, [pegjs_combinator('rule_ref', <<"IdentifierStart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"rest">>, [pegjs_combinator('suffixed', {zero_or_more, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_284_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"IdentifierStart">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"IdentifierStart">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"UnicodeLetter">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("$")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("_")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\\")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"sequence">>, [pegjs_combinator('rule_ref', <<"UnicodeEscapeSequence">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_290_1/2)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"IdentifierPart">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"IdentifierPart">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"IdentifierStart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"UnicodeCombiningMark">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"UnicodeDigit">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"UnicodeConnectorPunctuation">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\x{200C}")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\x{200D}")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"UnicodeLetter">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"UnicodeLetter">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"Lu">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"Ll">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"Lt">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"Lm">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"Lo">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"Nl">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"UnicodeCombiningMark">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"UnicodeCombiningMark">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"Mn">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"Mc">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"UnicodeDigit">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"UnicodeDigit">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"Nd">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"UnicodeConnectorPunctuation">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"UnicodeConnectorPunctuation">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"Pc">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"ReservedWord">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"ReservedWord">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"Keyword">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"FutureReservedWord">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"NullLiteral">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"BooleanLiteral">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"Keyword">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"Keyword">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"BreakToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"CaseToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"CatchToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"ContinueToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"DebuggerToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"DefaultToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"DeleteToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"DoToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"ElseToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"FinallyToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"ForToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"FunctionToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"IfToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"InstanceofToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"InToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"NewToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"ReturnToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"SwitchToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"ThisToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"ThrowToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"TryToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"TypeofToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"VarToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"VoidToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"WhileToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"WithToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"FutureReservedWord">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"FutureReservedWord">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"ClassToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"ConstToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"EnumToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"ExportToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"ExtendsToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"ImportToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"SuperToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"NullLiteral">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"NullLiteral">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"NullToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"BooleanLiteral">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"BooleanLiteral">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"TrueToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"FalseToken">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"LiteralMatcher">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"LiteralMatcher">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {<<"value">>, [pegjs_combinator('rule_ref', <<"StringLiteral">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"ignoreCase">>, [pegjs_combinator('suffixed', {optional, [pegjs_combinator('literal', {[unicode:characters_to_binary("i")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_376_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"StringLiteral">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"StringLiteral">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\"")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"chars">>, [pegjs_combinator('suffixed', {zero_or_more, [pegjs_combinator('rule_ref', <<"DoubleStringCharacter">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\"")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_378_3/2), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("'")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"chars">>, [pegjs_combinator('suffixed', {zero_or_more, [pegjs_combinator('rule_ref', <<"SingleStringCharacter">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("'")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_380_1/2)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"DoubleStringCharacter">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"DoubleStringCharacter">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\"")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\\")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"LineTerminator">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"SourceCharacter">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_382_3/2), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\\")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"sequence">>, [pegjs_combinator('rule_ref', <<"EscapeSequence">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_383_3/2), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"LineContinuation">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"SingleStringCharacter">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"SingleStringCharacter">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("'")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\\")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"LineTerminator">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"SourceCharacter">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_387_3/2), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\\")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"sequence">>, [pegjs_combinator('rule_ref', <<"EscapeSequence">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_388_3/2), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"LineContinuation">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"CharacterClassMatcher">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"CharacterClassMatcher">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("[")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"inverted">>, [pegjs_combinator('suffixed', {optional, [pegjs_combinator('literal', {[unicode:characters_to_binary("^")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"parts">>, [pegjs_combinator('suffixed', {zero_or_more, [pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"ClassCharacterRange">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"ClassCharacter">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("]")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"ignoreCase">>, [pegjs_combinator('suffixed', {optional, [pegjs_combinator('literal', {[unicode:characters_to_binary("i")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_408_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"ClassCharacterRange">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"ClassCharacterRange">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {<<"begin">>, [pegjs_combinator('rule_ref', <<"ClassCharacter">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("-")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"end">>, [pegjs_combinator('rule_ref', <<"ClassCharacter">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_419_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"ClassCharacter">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"ClassCharacter">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("]")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\\")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"LineTerminator">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"SourceCharacter">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_421_3/2), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\\")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"sequence">>, [pegjs_combinator('rule_ref', <<"EscapeSequence">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_422_3/2), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"LineContinuation">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_424_1/2)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"LineContinuation">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"LineContinuation">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\\")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"LineTerminatorSequence">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_427_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"EscapeSequence">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"EscapeSequence">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"CharacterEscapeSequence">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("0")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"DecimalDigit">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_430_3/2), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"HexEscapeSequence">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"UnicodeEscapeSequence">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"CharacterEscapeSequence">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"CharacterEscapeSequence">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"SingleEscapeCharacter">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"NonEscapeCharacter">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"SingleEscapeCharacter">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"SingleEscapeCharacter">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("'")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\"")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\\")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("b")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_442_3/2), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("f")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_443_3/2), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("n")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_444_3/2), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("r")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_445_3/2), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("t")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_446_3/2), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("v")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_448_1/2)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"NonEscapeCharacter">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"NonEscapeCharacter">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"EscapeCharacter">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"LineTerminator">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"SourceCharacter">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_451_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"EscapeCharacter">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"EscapeCharacter">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"SingleEscapeCharacter">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"DecimalDigit">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("x")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("u")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"HexEscapeSequence">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"HexEscapeSequence">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("x")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"digits">>, [pegjs_combinator('text', pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"HexDigit">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"HexDigit">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_463_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"UnicodeEscapeSequence">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"UnicodeEscapeSequence">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("u")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"digits">>, [pegjs_combinator('text', pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"HexDigit">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"HexDigit">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"HexDigit">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"HexDigit">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_469_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"DecimalDigit">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"DecimalDigit">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('regexp', {[unicode:characters_to_binary("[0-9]")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"HexDigit">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"HexDigit">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('regexp', {[unicode:characters_to_binary("[0-9a-f]")], true}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"AnyMatcher">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"AnyMatcher">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary(".")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_478_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"CodeBlock">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"CodeBlock">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("{")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {<<"code">>, [pegjs_combinator('rule_ref', <<"Code">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("}")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_481_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"Code">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"Code">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('text', pegjs_combinator('suffixed', {zero_or_more, [pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('suffixed', {one_or_more, [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('regexp', {[unicode:characters_to_binary("[{}]")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"SourceCharacter">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("{")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"Code">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("}")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"Ll">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"Ll">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('regexp', {[unicode:characters_to_binary("[\x{0061}-\x{007A}\x{00B5}\x{00DF}-\x{00F6}\x{00F8}-\x{00FF}\x{0101}\x{0103}\x{0105}\x{0107}\x{0109}\x{010B}\x{010D}\x{010F}\x{0111}\x{0113}\x{0115}\x{0117}\x{0119}\x{011B}\x{011D}\x{011F}\x{0121}\x{0123}\x{0125}\x{0127}\x{0129}\x{012B}\x{012D}\x{012F}\x{0131}\x{0133}\x{0135}\x{0137}-\x{0138}\x{013A}\x{013C}\x{013E}\x{0140}\x{0142}\x{0144}\x{0146}\x{0148}-\x{0149}\x{014B}\x{014D}\x{014F}\x{0151}\x{0153}\x{0155}\x{0157}\x{0159}\x{015B}\x{015D}\x{015F}\x{0161}\x{0163}\x{0165}\x{0167}\x{0169}\x{016B}\x{016D}\x{016F}\x{0171}\x{0173}\x{0175}\x{0177}\x{017A}\x{017C}\x{017E}-\x{0180}\x{0183}\x{0185}\x{0188}\x{018C}-\x{018D}\x{0192}\x{0195}\x{0199}-\x{019B}\x{019E}\x{01A1}\x{01A3}\x{01A5}\x{01A8}\x{01AA}-\x{01AB}\x{01AD}\x{01B0}\x{01B4}\x{01B6}\x{01B9}-\x{01BA}\x{01BD}-\x{01BF}\x{01C6}\x{01C9}\x{01CC}\x{01CE}\x{01D0}\x{01D2}\x{01D4}\x{01D6}\x{01D8}\x{01DA}\x{01DC}-\x{01DD}\x{01DF}\x{01E1}\x{01E3}\x{01E5}\x{01E7}\x{01E9}\x{01EB}\x{01ED}\x{01EF}-\x{01F0}\x{01F3}\x{01F5}\x{01F9}\x{01FB}\x{01FD}\x{01FF}\x{0201}\x{0203}\x{0205}\x{0207}\x{0209}\x{020B}\x{020D}\x{020F}\x{0211}\x{0213}\x{0215}\x{0217}\x{0219}\x{021B}\x{021D}\x{021F}\x{0221}\x{0223}\x{0225}\x{0227}\x{0229}\x{022B}\x{022D}\x{022F}\x{0231}\x{0233}-\x{0239}\x{023C}\x{023F}-\x{0240}\x{0242}\x{0247}\x{0249}\x{024B}\x{024D}\x{024F}-\x{0293}\x{0295}-\x{02AF}\x{0371}\x{0373}\x{0377}\x{037B}-\x{037D}\x{0390}\x{03AC}-\x{03CE}\x{03D0}-\x{03D1}\x{03D5}-\x{03D7}\x{03D9}\x{03DB}\x{03DD}\x{03DF}\x{03E1}\x{03E3}\x{03E5}\x{03E7}\x{03E9}\x{03EB}\x{03ED}\x{03EF}-\x{03F3}\x{03F5}\x{03F8}\x{03FB}-\x{03FC}\x{0430}-\x{045F}\x{0461}\x{0463}\x{0465}\x{0467}\x{0469}\x{046B}\x{046D}\x{046F}\x{0471}\x{0473}\x{0475}\x{0477}\x{0479}\x{047B}\x{047D}\x{047F}\x{0481}\x{048B}\x{048D}\x{048F}\x{0491}\x{0493}\x{0495}\x{0497}\x{0499}\x{049B}\x{049D}\x{049F}\x{04A1}\x{04A3}\x{04A5}\x{04A7}\x{04A9}\x{04AB}\x{04AD}\x{04AF}\x{04B1}\x{04B3}\x{04B5}\x{04B7}\x{04B9}\x{04BB}\x{04BD}\x{04BF}\x{04C2}\x{04C4}\x{04C6}\x{04C8}\x{04CA}\x{04CC}\x{04CE}-\x{04CF}\x{04D1}\x{04D3}\x{04D5}\x{04D7}\x{04D9}\x{04DB}\x{04DD}\x{04DF}\x{04E1}\x{04E3}\x{04E5}\x{04E7}\x{04E9}\x{04EB}\x{04ED}\x{04EF}\x{04F1}\x{04F3}\x{04F5}\x{04F7}\x{04F9}\x{04FB}\x{04FD}\x{04FF}\x{0501}\x{0503}\x{0505}\x{0507}\x{0509}\x{050B}\x{050D}\x{050F}\x{0511}\x{0513}\x{0515}\x{0517}\x{0519}\x{051B}\x{051D}\x{051F}\x{0521}\x{0523}\x{0525}\x{0527}\x{0561}-\x{0587}\x{1D00}-\x{1D2B}\x{1D6B}-\x{1D77}\x{1D79}-\x{1D9A}\x{1E01}\x{1E03}\x{1E05}\x{1E07}\x{1E09}\x{1E0B}\x{1E0D}\x{1E0F}\x{1E11}\x{1E13}\x{1E15}\x{1E17}\x{1E19}\x{1E1B}\x{1E1D}\x{1E1F}\x{1E21}\x{1E23}\x{1E25}\x{1E27}\x{1E29}\x{1E2B}\x{1E2D}\x{1E2F}\x{1E31}\x{1E33}\x{1E35}\x{1E37}\x{1E39}\x{1E3B}\x{1E3D}\x{1E3F}\x{1E41}\x{1E43}\x{1E45}\x{1E47}\x{1E49}\x{1E4B}\x{1E4D}\x{1E4F}\x{1E51}\x{1E53}\x{1E55}\x{1E57}\x{1E59}\x{1E5B}\x{1E5D}\x{1E5F}\x{1E61}\x{1E63}\x{1E65}\x{1E67}\x{1E69}\x{1E6B}\x{1E6D}\x{1E6F}\x{1E71}\x{1E73}\x{1E75}\x{1E77}\x{1E79}\x{1E7B}\x{1E7D}\x{1E7F}\x{1E81}\x{1E83}\x{1E85}\x{1E87}\x{1E89}\x{1E8B}\x{1E8D}\x{1E8F}\x{1E91}\x{1E93}\x{1E95}-\x{1E9D}\x{1E9F}\x{1EA1}\x{1EA3}\x{1EA5}\x{1EA7}\x{1EA9}\x{1EAB}\x{1EAD}\x{1EAF}\x{1EB1}\x{1EB3}\x{1EB5}\x{1EB7}\x{1EB9}\x{1EBB}\x{1EBD}\x{1EBF}\x{1EC1}\x{1EC3}\x{1EC5}\x{1EC7}\x{1EC9}\x{1ECB}\x{1ECD}\x{1ECF}\x{1ED1}\x{1ED3}\x{1ED5}\x{1ED7}\x{1ED9}\x{1EDB}\x{1EDD}\x{1EDF}\x{1EE1}\x{1EE3}\x{1EE5}\x{1EE7}\x{1EE9}\x{1EEB}\x{1EED}\x{1EEF}\x{1EF1}\x{1EF3}\x{1EF5}\x{1EF7}\x{1EF9}\x{1EFB}\x{1EFD}\x{1EFF}-\x{1F07}\x{1F10}-\x{1F15}\x{1F20}-\x{1F27}\x{1F30}-\x{1F37}\x{1F40}-\x{1F45}\x{1F50}-\x{1F57}\x{1F60}-\x{1F67}\x{1F70}-\x{1F7D}\x{1F80}-\x{1F87}\x{1F90}-\x{1F97}\x{1FA0}-\x{1FA7}\x{1FB0}-\x{1FB4}\x{1FB6}-\x{1FB7}\x{1FBE}\x{1FC2}-\x{1FC4}\x{1FC6}-\x{1FC7}\x{1FD0}-\x{1FD3}\x{1FD6}-\x{1FD7}\x{1FE0}-\x{1FE7}\x{1FF2}-\x{1FF4}\x{1FF6}-\x{1FF7}\x{210A}\x{210E}-\x{210F}\x{2113}\x{212F}\x{2134}\x{2139}\x{213C}-\x{213D}\x{2146}-\x{2149}\x{214E}\x{2184}\x{2C30}-\x{2C5E}\x{2C61}\x{2C65}-\x{2C66}\x{2C68}\x{2C6A}\x{2C6C}\x{2C71}\x{2C73}-\x{2C74}\x{2C76}-\x{2C7B}\x{2C81}\x{2C83}\x{2C85}\x{2C87}\x{2C89}\x{2C8B}\x{2C8D}\x{2C8F}\x{2C91}\x{2C93}\x{2C95}\x{2C97}\x{2C99}\x{2C9B}\x{2C9D}\x{2C9F}\x{2CA1}\x{2CA3}\x{2CA5}\x{2CA7}\x{2CA9}\x{2CAB}\x{2CAD}\x{2CAF}\x{2CB1}\x{2CB3}\x{2CB5}\x{2CB7}\x{2CB9}\x{2CBB}\x{2CBD}\x{2CBF}\x{2CC1}\x{2CC3}\x{2CC5}\x{2CC7}\x{2CC9}\x{2CCB}\x{2CCD}\x{2CCF}\x{2CD1}\x{2CD3}\x{2CD5}\x{2CD7}\x{2CD9}\x{2CDB}\x{2CDD}\x{2CDF}\x{2CE1}\x{2CE3}-\x{2CE4}\x{2CEC}\x{2CEE}\x{2CF3}\x{2D00}-\x{2D25}\x{2D27}\x{2D2D}\x{A641}\x{A643}\x{A645}\x{A647}\x{A649}\x{A64B}\x{A64D}\x{A64F}\x{A651}\x{A653}\x{A655}\x{A657}\x{A659}\x{A65B}\x{A65D}\x{A65F}\x{A661}\x{A663}\x{A665}\x{A667}\x{A669}\x{A66B}\x{A66D}\x{A681}\x{A683}\x{A685}\x{A687}\x{A689}\x{A68B}\x{A68D}\x{A68F}\x{A691}\x{A693}\x{A695}\x{A697}\x{A723}\x{A725}\x{A727}\x{A729}\x{A72B}\x{A72D}\x{A72F}-\x{A731}\x{A733}\x{A735}\x{A737}\x{A739}\x{A73B}\x{A73D}\x{A73F}\x{A741}\x{A743}\x{A745}\x{A747}\x{A749}\x{A74B}\x{A74D}\x{A74F}\x{A751}\x{A753}\x{A755}\x{A757}\x{A759}\x{A75B}\x{A75D}\x{A75F}\x{A761}\x{A763}\x{A765}\x{A767}\x{A769}\x{A76B}\x{A76D}\x{A76F}\x{A771}-\x{A778}\x{A77A}\x{A77C}\x{A77F}\x{A781}\x{A783}\x{A785}\x{A787}\x{A78C}\x{A78E}\x{A791}\x{A793}\x{A7A1}\x{A7A3}\x{A7A5}\x{A7A7}\x{A7A9}\x{A7FA}\x{FB00}-\x{FB06}\x{FB13}-\x{FB17}\x{FF41}-\x{FF5A}]")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"Lm">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"Lm">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('regexp', {[unicode:characters_to_binary("[\x{02B0}-\x{02C1}\x{02C6}-\x{02D1}\x{02E0}-\x{02E4}\x{02EC}\x{02EE}\x{0374}\x{037A}\x{0559}\x{0640}\x{06E5}-\x{06E6}\x{07F4}-\x{07F5}\x{07FA}\x{081A}\x{0824}\x{0828}\x{0971}\x{0E46}\x{0EC6}\x{10FC}\x{17D7}\x{1843}\x{1AA7}\x{1C78}-\x{1C7D}\x{1D2C}-\x{1D6A}\x{1D78}\x{1D9B}-\x{1DBF}\x{2071}\x{207F}\x{2090}-\x{209C}\x{2C7C}-\x{2C7D}\x{2D6F}\x{2E2F}\x{3005}\x{3031}-\x{3035}\x{303B}\x{309D}-\x{309E}\x{30FC}-\x{30FE}\x{A015}\x{A4F8}-\x{A4FD}\x{A60C}\x{A67F}\x{A717}-\x{A71F}\x{A770}\x{A788}\x{A7F8}-\x{A7F9}\x{A9CF}\x{AA70}\x{AADD}\x{AAF3}-\x{AAF4}\x{FF70}\x{FF9E}-\x{FF9F}]")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"Lo">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"Lo">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('regexp', {[unicode:characters_to_binary("[\x{00AA}\x{00BA}\x{01BB}\x{01C0}-\x{01C3}\x{0294}\x{05D0}-\x{05EA}\x{05F0}-\x{05F2}\x{0620}-\x{063F}\x{0641}-\x{064A}\x{066E}-\x{066F}\x{0671}-\x{06D3}\x{06D5}\x{06EE}-\x{06EF}\x{06FA}-\x{06FC}\x{06FF}\x{0710}\x{0712}-\x{072F}\x{074D}-\x{07A5}\x{07B1}\x{07CA}-\x{07EA}\x{0800}-\x{0815}\x{0840}-\x{0858}\x{08A0}\x{08A2}-\x{08AC}\x{0904}-\x{0939}\x{093D}\x{0950}\x{0958}-\x{0961}\x{0972}-\x{0977}\x{0979}-\x{097F}\x{0985}-\x{098C}\x{098F}-\x{0990}\x{0993}-\x{09A8}\x{09AA}-\x{09B0}\x{09B2}\x{09B6}-\x{09B9}\x{09BD}\x{09CE}\x{09DC}-\x{09DD}\x{09DF}-\x{09E1}\x{09F0}-\x{09F1}\x{0A05}-\x{0A0A}\x{0A0F}-\x{0A10}\x{0A13}-\x{0A28}\x{0A2A}-\x{0A30}\x{0A32}-\x{0A33}\x{0A35}-\x{0A36}\x{0A38}-\x{0A39}\x{0A59}-\x{0A5C}\x{0A5E}\x{0A72}-\x{0A74}\x{0A85}-\x{0A8D}\x{0A8F}-\x{0A91}\x{0A93}-\x{0AA8}\x{0AAA}-\x{0AB0}\x{0AB2}-\x{0AB3}\x{0AB5}-\x{0AB9}\x{0ABD}\x{0AD0}\x{0AE0}-\x{0AE1}\x{0B05}-\x{0B0C}\x{0B0F}-\x{0B10}\x{0B13}-\x{0B28}\x{0B2A}-\x{0B30}\x{0B32}-\x{0B33}\x{0B35}-\x{0B39}\x{0B3D}\x{0B5C}-\x{0B5D}\x{0B5F}-\x{0B61}\x{0B71}\x{0B83}\x{0B85}-\x{0B8A}\x{0B8E}-\x{0B90}\x{0B92}-\x{0B95}\x{0B99}-\x{0B9A}\x{0B9C}\x{0B9E}-\x{0B9F}\x{0BA3}-\x{0BA4}\x{0BA8}-\x{0BAA}\x{0BAE}-\x{0BB9}\x{0BD0}\x{0C05}-\x{0C0C}\x{0C0E}-\x{0C10}\x{0C12}-\x{0C28}\x{0C2A}-\x{0C33}\x{0C35}-\x{0C39}\x{0C3D}\x{0C58}-\x{0C59}\x{0C60}-\x{0C61}\x{0C85}-\x{0C8C}\x{0C8E}-\x{0C90}\x{0C92}-\x{0CA8}\x{0CAA}-\x{0CB3}\x{0CB5}-\x{0CB9}\x{0CBD}\x{0CDE}\x{0CE0}-\x{0CE1}\x{0CF1}-\x{0CF2}\x{0D05}-\x{0D0C}\x{0D0E}-\x{0D10}\x{0D12}-\x{0D3A}\x{0D3D}\x{0D4E}\x{0D60}-\x{0D61}\x{0D7A}-\x{0D7F}\x{0D85}-\x{0D96}\x{0D9A}-\x{0DB1}\x{0DB3}-\x{0DBB}\x{0DBD}\x{0DC0}-\x{0DC6}\x{0E01}-\x{0E30}\x{0E32}-\x{0E33}\x{0E40}-\x{0E45}\x{0E81}-\x{0E82}\x{0E84}\x{0E87}-\x{0E88}\x{0E8A}\x{0E8D}\x{0E94}-\x{0E97}\x{0E99}-\x{0E9F}\x{0EA1}-\x{0EA3}\x{0EA5}\x{0EA7}\x{0EAA}-\x{0EAB}\x{0EAD}-\x{0EB0}\x{0EB2}-\x{0EB3}\x{0EBD}\x{0EC0}-\x{0EC4}\x{0EDC}-\x{0EDF}\x{0F00}\x{0F40}-\x{0F47}\x{0F49}-\x{0F6C}\x{0F88}-\x{0F8C}\x{1000}-\x{102A}\x{103F}\x{1050}-\x{1055}\x{105A}-\x{105D}\x{1061}\x{1065}-\x{1066}\x{106E}-\x{1070}\x{1075}-\x{1081}\x{108E}\x{10D0}-\x{10FA}\x{10FD}-\x{1248}\x{124A}-\x{124D}\x{1250}-\x{1256}\x{1258}\x{125A}-\x{125D}\x{1260}-\x{1288}\x{128A}-\x{128D}\x{1290}-\x{12B0}\x{12B2}-\x{12B5}\x{12B8}-\x{12BE}\x{12C0}\x{12C2}-\x{12C5}\x{12C8}-\x{12D6}\x{12D8}-\x{1310}\x{1312}-\x{1315}\x{1318}-\x{135A}\x{1380}-\x{138F}\x{13A0}-\x{13F4}\x{1401}-\x{166C}\x{166F}-\x{167F}\x{1681}-\x{169A}\x{16A0}-\x{16EA}\x{1700}-\x{170C}\x{170E}-\x{1711}\x{1720}-\x{1731}\x{1740}-\x{1751}\x{1760}-\x{176C}\x{176E}-\x{1770}\x{1780}-\x{17B3}\x{17DC}\x{1820}-\x{1842}\x{1844}-\x{1877}\x{1880}-\x{18A8}\x{18AA}\x{18B0}-\x{18F5}\x{1900}-\x{191C}\x{1950}-\x{196D}\x{1970}-\x{1974}\x{1980}-\x{19AB}\x{19C1}-\x{19C7}\x{1A00}-\x{1A16}\x{1A20}-\x{1A54}\x{1B05}-\x{1B33}\x{1B45}-\x{1B4B}\x{1B83}-\x{1BA0}\x{1BAE}-\x{1BAF}\x{1BBA}-\x{1BE5}\x{1C00}-\x{1C23}\x{1C4D}-\x{1C4F}\x{1C5A}-\x{1C77}\x{1CE9}-\x{1CEC}\x{1CEE}-\x{1CF1}\x{1CF5}-\x{1CF6}\x{2135}-\x{2138}\x{2D30}-\x{2D67}\x{2D80}-\x{2D96}\x{2DA0}-\x{2DA6}\x{2DA8}-\x{2DAE}\x{2DB0}-\x{2DB6}\x{2DB8}-\x{2DBE}\x{2DC0}-\x{2DC6}\x{2DC8}-\x{2DCE}\x{2DD0}-\x{2DD6}\x{2DD8}-\x{2DDE}\x{3006}\x{303C}\x{3041}-\x{3096}\x{309F}\x{30A1}-\x{30FA}\x{30FF}\x{3105}-\x{312D}\x{3131}-\x{318E}\x{31A0}-\x{31BA}\x{31F0}-\x{31FF}\x{3400}-\x{4DB5}\x{4E00}-\x{9FCC}\x{A000}-\x{A014}\x{A016}-\x{A48C}\x{A4D0}-\x{A4F7}\x{A500}-\x{A60B}\x{A610}-\x{A61F}\x{A62A}-\x{A62B}\x{A66E}\x{A6A0}-\x{A6E5}\x{A7FB}-\x{A801}\x{A803}-\x{A805}\x{A807}-\x{A80A}\x{A80C}-\x{A822}\x{A840}-\x{A873}\x{A882}-\x{A8B3}\x{A8F2}-\x{A8F7}\x{A8FB}\x{A90A}-\x{A925}\x{A930}-\x{A946}\x{A960}-\x{A97C}\x{A984}-\x{A9B2}\x{AA00}-\x{AA28}\x{AA40}-\x{AA42}\x{AA44}-\x{AA4B}\x{AA60}-\x{AA6F}\x{AA71}-\x{AA76}\x{AA7A}\x{AA80}-\x{AAAF}\x{AAB1}\x{AAB5}-\x{AAB6}\x{AAB9}-\x{AABD}\x{AAC0}\x{AAC2}\x{AADB}-\x{AADC}\x{AAE0}-\x{AAEA}\x{AAF2}\x{AB01}-\x{AB06}\x{AB09}-\x{AB0E}\x{AB11}-\x{AB16}\x{AB20}-\x{AB26}\x{AB28}-\x{AB2E}\x{ABC0}-\x{ABE2}\x{AC00}-\x{D7A3}\x{D7B0}-\x{D7C6}\x{D7CB}-\x{D7FB}\x{F900}-\x{FA6D}\x{FA70}-\x{FAD9}\x{FB1D}\x{FB1F}-\x{FB28}\x{FB2A}-\x{FB36}\x{FB38}-\x{FB3C}\x{FB3E}\x{FB40}-\x{FB41}\x{FB43}-\x{FB44}\x{FB46}-\x{FBB1}\x{FBD3}-\x{FD3D}\x{FD50}-\x{FD8F}\x{FD92}-\x{FDC7}\x{FDF0}-\x{FDFB}\x{FE70}-\x{FE74}\x{FE76}-\x{FEFC}\x{FF66}-\x{FF6F}\x{FF71}-\x{FF9D}\x{FFA0}-\x{FFBE}\x{FFC2}-\x{FFC7}\x{FFCA}-\x{FFCF}\x{FFD2}-\x{FFD7}\x{FFDA}-\x{FFDC}]")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"Lt">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"Lt">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('regexp', {[unicode:characters_to_binary("[\x{01C5}\x{01C8}\x{01CB}\x{01F2}\x{1F88}-\x{1F8F}\x{1F98}-\x{1F9F}\x{1FA8}-\x{1FAF}\x{1FBC}\x{1FCC}\x{1FFC}]")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"Lu">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"Lu">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('regexp', {[unicode:characters_to_binary("[\x{0041}-\x{005A}\x{00C0}-\x{00D6}\x{00D8}-\x{00DE}\x{0100}\x{0102}\x{0104}\x{0106}\x{0108}\x{010A}\x{010C}\x{010E}\x{0110}\x{0112}\x{0114}\x{0116}\x{0118}\x{011A}\x{011C}\x{011E}\x{0120}\x{0122}\x{0124}\x{0126}\x{0128}\x{012A}\x{012C}\x{012E}\x{0130}\x{0132}\x{0134}\x{0136}\x{0139}\x{013B}\x{013D}\x{013F}\x{0141}\x{0143}\x{0145}\x{0147}\x{014A}\x{014C}\x{014E}\x{0150}\x{0152}\x{0154}\x{0156}\x{0158}\x{015A}\x{015C}\x{015E}\x{0160}\x{0162}\x{0164}\x{0166}\x{0168}\x{016A}\x{016C}\x{016E}\x{0170}\x{0172}\x{0174}\x{0176}\x{0178}-\x{0179}\x{017B}\x{017D}\x{0181}-\x{0182}\x{0184}\x{0186}-\x{0187}\x{0189}-\x{018B}\x{018E}-\x{0191}\x{0193}-\x{0194}\x{0196}-\x{0198}\x{019C}-\x{019D}\x{019F}-\x{01A0}\x{01A2}\x{01A4}\x{01A6}-\x{01A7}\x{01A9}\x{01AC}\x{01AE}-\x{01AF}\x{01B1}-\x{01B3}\x{01B5}\x{01B7}-\x{01B8}\x{01BC}\x{01C4}\x{01C7}\x{01CA}\x{01CD}\x{01CF}\x{01D1}\x{01D3}\x{01D5}\x{01D7}\x{01D9}\x{01DB}\x{01DE}\x{01E0}\x{01E2}\x{01E4}\x{01E6}\x{01E8}\x{01EA}\x{01EC}\x{01EE}\x{01F1}\x{01F4}\x{01F6}-\x{01F8}\x{01FA}\x{01FC}\x{01FE}\x{0200}\x{0202}\x{0204}\x{0206}\x{0208}\x{020A}\x{020C}\x{020E}\x{0210}\x{0212}\x{0214}\x{0216}\x{0218}\x{021A}\x{021C}\x{021E}\x{0220}\x{0222}\x{0224}\x{0226}\x{0228}\x{022A}\x{022C}\x{022E}\x{0230}\x{0232}\x{023A}-\x{023B}\x{023D}-\x{023E}\x{0241}\x{0243}-\x{0246}\x{0248}\x{024A}\x{024C}\x{024E}\x{0370}\x{0372}\x{0376}\x{0386}\x{0388}-\x{038A}\x{038C}\x{038E}-\x{038F}\x{0391}-\x{03A1}\x{03A3}-\x{03AB}\x{03CF}\x{03D2}-\x{03D4}\x{03D8}\x{03DA}\x{03DC}\x{03DE}\x{03E0}\x{03E2}\x{03E4}\x{03E6}\x{03E8}\x{03EA}\x{03EC}\x{03EE}\x{03F4}\x{03F7}\x{03F9}-\x{03FA}\x{03FD}-\x{042F}\x{0460}\x{0462}\x{0464}\x{0466}\x{0468}\x{046A}\x{046C}\x{046E}\x{0470}\x{0472}\x{0474}\x{0476}\x{0478}\x{047A}\x{047C}\x{047E}\x{0480}\x{048A}\x{048C}\x{048E}\x{0490}\x{0492}\x{0494}\x{0496}\x{0498}\x{049A}\x{049C}\x{049E}\x{04A0}\x{04A2}\x{04A4}\x{04A6}\x{04A8}\x{04AA}\x{04AC}\x{04AE}\x{04B0}\x{04B2}\x{04B4}\x{04B6}\x{04B8}\x{04BA}\x{04BC}\x{04BE}\x{04C0}-\x{04C1}\x{04C3}\x{04C5}\x{04C7}\x{04C9}\x{04CB}\x{04CD}\x{04D0}\x{04D2}\x{04D4}\x{04D6}\x{04D8}\x{04DA}\x{04DC}\x{04DE}\x{04E0}\x{04E2}\x{04E4}\x{04E6}\x{04E8}\x{04EA}\x{04EC}\x{04EE}\x{04F0}\x{04F2}\x{04F4}\x{04F6}\x{04F8}\x{04FA}\x{04FC}\x{04FE}\x{0500}\x{0502}\x{0504}\x{0506}\x{0508}\x{050A}\x{050C}\x{050E}\x{0510}\x{0512}\x{0514}\x{0516}\x{0518}\x{051A}\x{051C}\x{051E}\x{0520}\x{0522}\x{0524}\x{0526}\x{0531}-\x{0556}\x{10A0}-\x{10C5}\x{10C7}\x{10CD}\x{1E00}\x{1E02}\x{1E04}\x{1E06}\x{1E08}\x{1E0A}\x{1E0C}\x{1E0E}\x{1E10}\x{1E12}\x{1E14}\x{1E16}\x{1E18}\x{1E1A}\x{1E1C}\x{1E1E}\x{1E20}\x{1E22}\x{1E24}\x{1E26}\x{1E28}\x{1E2A}\x{1E2C}\x{1E2E}\x{1E30}\x{1E32}\x{1E34}\x{1E36}\x{1E38}\x{1E3A}\x{1E3C}\x{1E3E}\x{1E40}\x{1E42}\x{1E44}\x{1E46}\x{1E48}\x{1E4A}\x{1E4C}\x{1E4E}\x{1E50}\x{1E52}\x{1E54}\x{1E56}\x{1E58}\x{1E5A}\x{1E5C}\x{1E5E}\x{1E60}\x{1E62}\x{1E64}\x{1E66}\x{1E68}\x{1E6A}\x{1E6C}\x{1E6E}\x{1E70}\x{1E72}\x{1E74}\x{1E76}\x{1E78}\x{1E7A}\x{1E7C}\x{1E7E}\x{1E80}\x{1E82}\x{1E84}\x{1E86}\x{1E88}\x{1E8A}\x{1E8C}\x{1E8E}\x{1E90}\x{1E92}\x{1E94}\x{1E9E}\x{1EA0}\x{1EA2}\x{1EA4}\x{1EA6}\x{1EA8}\x{1EAA}\x{1EAC}\x{1EAE}\x{1EB0}\x{1EB2}\x{1EB4}\x{1EB6}\x{1EB8}\x{1EBA}\x{1EBC}\x{1EBE}\x{1EC0}\x{1EC2}\x{1EC4}\x{1EC6}\x{1EC8}\x{1ECA}\x{1ECC}\x{1ECE}\x{1ED0}\x{1ED2}\x{1ED4}\x{1ED6}\x{1ED8}\x{1EDA}\x{1EDC}\x{1EDE}\x{1EE0}\x{1EE2}\x{1EE4}\x{1EE6}\x{1EE8}\x{1EEA}\x{1EEC}\x{1EEE}\x{1EF0}\x{1EF2}\x{1EF4}\x{1EF6}\x{1EF8}\x{1EFA}\x{1EFC}\x{1EFE}\x{1F08}-\x{1F0F}\x{1F18}-\x{1F1D}\x{1F28}-\x{1F2F}\x{1F38}-\x{1F3F}\x{1F48}-\x{1F4D}\x{1F59}\x{1F5B}\x{1F5D}\x{1F5F}\x{1F68}-\x{1F6F}\x{1FB8}-\x{1FBB}\x{1FC8}-\x{1FCB}\x{1FD8}-\x{1FDB}\x{1FE8}-\x{1FEC}\x{1FF8}-\x{1FFB}\x{2102}\x{2107}\x{210B}-\x{210D}\x{2110}-\x{2112}\x{2115}\x{2119}-\x{211D}\x{2124}\x{2126}\x{2128}\x{212A}-\x{212D}\x{2130}-\x{2133}\x{213E}-\x{213F}\x{2145}\x{2183}\x{2C00}-\x{2C2E}\x{2C60}\x{2C62}-\x{2C64}\x{2C67}\x{2C69}\x{2C6B}\x{2C6D}-\x{2C70}\x{2C72}\x{2C75}\x{2C7E}-\x{2C80}\x{2C82}\x{2C84}\x{2C86}\x{2C88}\x{2C8A}\x{2C8C}\x{2C8E}\x{2C90}\x{2C92}\x{2C94}\x{2C96}\x{2C98}\x{2C9A}\x{2C9C}\x{2C9E}\x{2CA0}\x{2CA2}\x{2CA4}\x{2CA6}\x{2CA8}\x{2CAA}\x{2CAC}\x{2CAE}\x{2CB0}\x{2CB2}\x{2CB4}\x{2CB6}\x{2CB8}\x{2CBA}\x{2CBC}\x{2CBE}\x{2CC0}\x{2CC2}\x{2CC4}\x{2CC6}\x{2CC8}\x{2CCA}\x{2CCC}\x{2CCE}\x{2CD0}\x{2CD2}\x{2CD4}\x{2CD6}\x{2CD8}\x{2CDA}\x{2CDC}\x{2CDE}\x{2CE0}\x{2CE2}\x{2CEB}\x{2CED}\x{2CF2}\x{A640}\x{A642}\x{A644}\x{A646}\x{A648}\x{A64A}\x{A64C}\x{A64E}\x{A650}\x{A652}\x{A654}\x{A656}\x{A658}\x{A65A}\x{A65C}\x{A65E}\x{A660}\x{A662}\x{A664}\x{A666}\x{A668}\x{A66A}\x{A66C}\x{A680}\x{A682}\x{A684}\x{A686}\x{A688}\x{A68A}\x{A68C}\x{A68E}\x{A690}\x{A692}\x{A694}\x{A696}\x{A722}\x{A724}\x{A726}\x{A728}\x{A72A}\x{A72C}\x{A72E}\x{A732}\x{A734}\x{A736}\x{A738}\x{A73A}\x{A73C}\x{A73E}\x{A740}\x{A742}\x{A744}\x{A746}\x{A748}\x{A74A}\x{A74C}\x{A74E}\x{A750}\x{A752}\x{A754}\x{A756}\x{A758}\x{A75A}\x{A75C}\x{A75E}\x{A760}\x{A762}\x{A764}\x{A766}\x{A768}\x{A76A}\x{A76C}\x{A76E}\x{A779}\x{A77B}\x{A77D}-\x{A77E}\x{A780}\x{A782}\x{A784}\x{A786}\x{A78B}\x{A78D}\x{A790}\x{A792}\x{A7A0}\x{A7A2}\x{A7A4}\x{A7A6}\x{A7A8}\x{A7AA}\x{FF21}-\x{FF3A}]")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"Mc">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"Mc">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('regexp', {[unicode:characters_to_binary("[\x{0903}\x{093B}\x{093E}-\x{0940}\x{0949}-\x{094C}\x{094E}-\x{094F}\x{0982}-\x{0983}\x{09BE}-\x{09C0}\x{09C7}-\x{09C8}\x{09CB}-\x{09CC}\x{09D7}\x{0A03}\x{0A3E}-\x{0A40}\x{0A83}\x{0ABE}-\x{0AC0}\x{0AC9}\x{0ACB}-\x{0ACC}\x{0B02}-\x{0B03}\x{0B3E}\x{0B40}\x{0B47}-\x{0B48}\x{0B4B}-\x{0B4C}\x{0B57}\x{0BBE}-\x{0BBF}\x{0BC1}-\x{0BC2}\x{0BC6}-\x{0BC8}\x{0BCA}-\x{0BCC}\x{0BD7}\x{0C01}-\x{0C03}\x{0C41}-\x{0C44}\x{0C82}-\x{0C83}\x{0CBE}\x{0CC0}-\x{0CC4}\x{0CC7}-\x{0CC8}\x{0CCA}-\x{0CCB}\x{0CD5}-\x{0CD6}\x{0D02}-\x{0D03}\x{0D3E}-\x{0D40}\x{0D46}-\x{0D48}\x{0D4A}-\x{0D4C}\x{0D57}\x{0D82}-\x{0D83}\x{0DCF}-\x{0DD1}\x{0DD8}-\x{0DDF}\x{0DF2}-\x{0DF3}\x{0F3E}-\x{0F3F}\x{0F7F}\x{102B}-\x{102C}\x{1031}\x{1038}\x{103B}-\x{103C}\x{1056}-\x{1057}\x{1062}-\x{1064}\x{1067}-\x{106D}\x{1083}-\x{1084}\x{1087}-\x{108C}\x{108F}\x{109A}-\x{109C}\x{17B6}\x{17BE}-\x{17C5}\x{17C7}-\x{17C8}\x{1923}-\x{1926}\x{1929}-\x{192B}\x{1930}-\x{1931}\x{1933}-\x{1938}\x{19B0}-\x{19C0}\x{19C8}-\x{19C9}\x{1A19}-\x{1A1A}\x{1A55}\x{1A57}\x{1A61}\x{1A63}-\x{1A64}\x{1A6D}-\x{1A72}\x{1B04}\x{1B35}\x{1B3B}\x{1B3D}-\x{1B41}\x{1B43}-\x{1B44}\x{1B82}\x{1BA1}\x{1BA6}-\x{1BA7}\x{1BAA}\x{1BAC}-\x{1BAD}\x{1BE7}\x{1BEA}-\x{1BEC}\x{1BEE}\x{1BF2}-\x{1BF3}\x{1C24}-\x{1C2B}\x{1C34}-\x{1C35}\x{1CE1}\x{1CF2}-\x{1CF3}\x{302E}-\x{302F}\x{A823}-\x{A824}\x{A827}\x{A880}-\x{A881}\x{A8B4}-\x{A8C3}\x{A952}-\x{A953}\x{A983}\x{A9B4}-\x{A9B5}\x{A9BA}-\x{A9BB}\x{A9BD}-\x{A9C0}\x{AA2F}-\x{AA30}\x{AA33}-\x{AA34}\x{AA4D}\x{AA7B}\x{AAEB}\x{AAEE}-\x{AAEF}\x{AAF5}\x{ABE3}-\x{ABE4}\x{ABE6}-\x{ABE7}\x{ABE9}-\x{ABEA}\x{ABEC}]")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"Mn">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"Mn">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('regexp', {[unicode:characters_to_binary("[\x{0300}-\x{036F}\x{0483}-\x{0487}\x{0591}-\x{05BD}\x{05BF}\x{05C1}-\x{05C2}\x{05C4}-\x{05C5}\x{05C7}\x{0610}-\x{061A}\x{064B}-\x{065F}\x{0670}\x{06D6}-\x{06DC}\x{06DF}-\x{06E4}\x{06E7}-\x{06E8}\x{06EA}-\x{06ED}\x{0711}\x{0730}-\x{074A}\x{07A6}-\x{07B0}\x{07EB}-\x{07F3}\x{0816}-\x{0819}\x{081B}-\x{0823}\x{0825}-\x{0827}\x{0829}-\x{082D}\x{0859}-\x{085B}\x{08E4}-\x{08FE}\x{0900}-\x{0902}\x{093A}\x{093C}\x{0941}-\x{0948}\x{094D}\x{0951}-\x{0957}\x{0962}-\x{0963}\x{0981}\x{09BC}\x{09C1}-\x{09C4}\x{09CD}\x{09E2}-\x{09E3}\x{0A01}-\x{0A02}\x{0A3C}\x{0A41}-\x{0A42}\x{0A47}-\x{0A48}\x{0A4B}-\x{0A4D}\x{0A51}\x{0A70}-\x{0A71}\x{0A75}\x{0A81}-\x{0A82}\x{0ABC}\x{0AC1}-\x{0AC5}\x{0AC7}-\x{0AC8}\x{0ACD}\x{0AE2}-\x{0AE3}\x{0B01}\x{0B3C}\x{0B3F}\x{0B41}-\x{0B44}\x{0B4D}\x{0B56}\x{0B62}-\x{0B63}\x{0B82}\x{0BC0}\x{0BCD}\x{0C3E}-\x{0C40}\x{0C46}-\x{0C48}\x{0C4A}-\x{0C4D}\x{0C55}-\x{0C56}\x{0C62}-\x{0C63}\x{0CBC}\x{0CBF}\x{0CC6}\x{0CCC}-\x{0CCD}\x{0CE2}-\x{0CE3}\x{0D41}-\x{0D44}\x{0D4D}\x{0D62}-\x{0D63}\x{0DCA}\x{0DD2}-\x{0DD4}\x{0DD6}\x{0E31}\x{0E34}-\x{0E3A}\x{0E47}-\x{0E4E}\x{0EB1}\x{0EB4}-\x{0EB9}\x{0EBB}-\x{0EBC}\x{0EC8}-\x{0ECD}\x{0F18}-\x{0F19}\x{0F35}\x{0F37}\x{0F39}\x{0F71}-\x{0F7E}\x{0F80}-\x{0F84}\x{0F86}-\x{0F87}\x{0F8D}-\x{0F97}\x{0F99}-\x{0FBC}\x{0FC6}\x{102D}-\x{1030}\x{1032}-\x{1037}\x{1039}-\x{103A}\x{103D}-\x{103E}\x{1058}-\x{1059}\x{105E}-\x{1060}\x{1071}-\x{1074}\x{1082}\x{1085}-\x{1086}\x{108D}\x{109D}\x{135D}-\x{135F}\x{1712}-\x{1714}\x{1732}-\x{1734}\x{1752}-\x{1753}\x{1772}-\x{1773}\x{17B4}-\x{17B5}\x{17B7}-\x{17BD}\x{17C6}\x{17C9}-\x{17D3}\x{17DD}\x{180B}-\x{180D}\x{18A9}\x{1920}-\x{1922}\x{1927}-\x{1928}\x{1932}\x{1939}-\x{193B}\x{1A17}-\x{1A18}\x{1A1B}\x{1A56}\x{1A58}-\x{1A5E}\x{1A60}\x{1A62}\x{1A65}-\x{1A6C}\x{1A73}-\x{1A7C}\x{1A7F}\x{1B00}-\x{1B03}\x{1B34}\x{1B36}-\x{1B3A}\x{1B3C}\x{1B42}\x{1B6B}-\x{1B73}\x{1B80}-\x{1B81}\x{1BA2}-\x{1BA5}\x{1BA8}-\x{1BA9}\x{1BAB}\x{1BE6}\x{1BE8}-\x{1BE9}\x{1BED}\x{1BEF}-\x{1BF1}\x{1C2C}-\x{1C33}\x{1C36}-\x{1C37}\x{1CD0}-\x{1CD2}\x{1CD4}-\x{1CE0}\x{1CE2}-\x{1CE8}\x{1CED}\x{1CF4}\x{1DC0}-\x{1DE6}\x{1DFC}-\x{1DFF}\x{20D0}-\x{20DC}\x{20E1}\x{20E5}-\x{20F0}\x{2CEF}-\x{2CF1}\x{2D7F}\x{2DE0}-\x{2DFF}\x{302A}-\x{302D}\x{3099}-\x{309A}\x{A66F}\x{A674}-\x{A67D}\x{A69F}\x{A6F0}-\x{A6F1}\x{A802}\x{A806}\x{A80B}\x{A825}-\x{A826}\x{A8C4}\x{A8E0}-\x{A8F1}\x{A926}-\x{A92D}\x{A947}-\x{A951}\x{A980}-\x{A982}\x{A9B3}\x{A9B6}-\x{A9B9}\x{A9BC}\x{AA29}-\x{AA2E}\x{AA31}-\x{AA32}\x{AA35}-\x{AA36}\x{AA43}\x{AA4C}\x{AAB0}\x{AAB2}-\x{AAB4}\x{AAB7}-\x{AAB8}\x{AABE}-\x{AABF}\x{AAC1}\x{AAEC}-\x{AAED}\x{AAF6}\x{ABE5}\x{ABE8}\x{ABED}\x{FB1E}\x{FE00}-\x{FE0F}\x{FE20}-\x{FE26}]")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"Nd">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"Nd">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('regexp', {[unicode:characters_to_binary("[\x{0030}-\x{0039}\x{0660}-\x{0669}\x{06F0}-\x{06F9}\x{07C0}-\x{07C9}\x{0966}-\x{096F}\x{09E6}-\x{09EF}\x{0A66}-\x{0A6F}\x{0AE6}-\x{0AEF}\x{0B66}-\x{0B6F}\x{0BE6}-\x{0BEF}\x{0C66}-\x{0C6F}\x{0CE6}-\x{0CEF}\x{0D66}-\x{0D6F}\x{0E50}-\x{0E59}\x{0ED0}-\x{0ED9}\x{0F20}-\x{0F29}\x{1040}-\x{1049}\x{1090}-\x{1099}\x{17E0}-\x{17E9}\x{1810}-\x{1819}\x{1946}-\x{194F}\x{19D0}-\x{19D9}\x{1A80}-\x{1A89}\x{1A90}-\x{1A99}\x{1B50}-\x{1B59}\x{1BB0}-\x{1BB9}\x{1C40}-\x{1C49}\x{1C50}-\x{1C59}\x{A620}-\x{A629}\x{A8D0}-\x{A8D9}\x{A900}-\x{A909}\x{A9D0}-\x{A9D9}\x{AA50}-\x{AA59}\x{ABF0}-\x{ABF9}\x{FF10}-\x{FF19}]")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"Nl">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"Nl">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('regexp', {[unicode:characters_to_binary("[\x{16EE}-\x{16F0}\x{2160}-\x{2182}\x{2185}-\x{2188}\x{3007}\x{3021}-\x{3029}\x{3038}-\x{303A}\x{A6E6}-\x{A6EF}]")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"Pc">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"Pc">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('regexp', {[unicode:characters_to_binary("[\x{005F}\x{203F}-\x{2040}\x{2054}\x{FE33}-\x{FE34}\x{FE4D}-\x{FE4F}\x{FF3F}]")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"Zs">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"Zs">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('regexp', {[unicode:characters_to_binary("[\x{0020}\x{00A0}\x{1680}\x{2000}-\x{200A}\x{202F}\x{205F}\x{3000}]")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"BreakToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"BreakToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("break")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"CaseToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"CaseToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("case")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"CatchToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"CatchToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("catch")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"ClassToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"ClassToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("class")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"ConstToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"ConstToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("const")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"ContinueToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"ContinueToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("continue")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"DebuggerToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"DebuggerToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("debugger")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"DefaultToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"DefaultToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("default")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"DeleteToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"DeleteToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("delete")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"DoToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"DoToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("do")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"ElseToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"ElseToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("else")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"EnumToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"EnumToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("enum")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"ExportToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"ExportToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("export")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"ExtendsToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"ExtendsToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("extends")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"FalseToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"FalseToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("false")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"FinallyToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"FinallyToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("finally")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"ForToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"ForToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("for")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"FunctionToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"FunctionToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("function")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"IfToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"IfToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("if")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"ImportToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"ImportToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("import")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"InstanceofToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"InstanceofToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("instanceof")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"InToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"InToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("in")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"NewToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"NewToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("new")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"NullToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"NullToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("null")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"ReturnToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"ReturnToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("return")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"SuperToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"SuperToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("super")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"SwitchToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"SwitchToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("switch")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"ThisToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"ThisToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("this")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"ThrowToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"ThrowToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("throw")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"TrueToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"TrueToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("true")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"TryToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"TryToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("try")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"TypeofToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"TypeofToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("typeof")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"VarToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"VarToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("var")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"VoidToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"VoidToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("void")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"WhileToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"WhileToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("while")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"WithToken">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"WithToken">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("with")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('rule_ref', <<"IdentifierPart">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"__">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"__">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('suffixed', {zero_or_more, [pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"WhiteSpace">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"LineTerminatorSequence">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"Comment">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"_">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"_">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('suffixed', {zero_or_more, [pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"WhiteSpace">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"MultiLineCommentNoLineTerminator">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"EOS">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"EOS">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary(";")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"_">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('suffixed', {optional, [pegjs_combinator('rule_ref', <<"SingleLineComment">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"LineTerminatorSequence">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"__">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"EOF">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"EOF">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"EOF">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('anything', [], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       ).

-spec pegjs_code_98_1(iolist(), index()) -> parse_result().
pegjs_code_98_1(Node, Idx) ->

      [_, {_, Initializer}, {_, Rules}] = Node,
      #entry{ type        = <<"grammar">>
            , initializer = case Initializer of [I, _] -> I; [] -> [] end
            , rules       = entries(Rules)
            , index       = Idx
            }
    .

-spec pegjs_code_101_1(iolist(), index()) -> parse_result().
pegjs_code_101_1(Node, Idx) ->
 [{_, Code}, _] = Node, #entry{type = <<"initializer">>, code = Code, index = Idx} .

-spec pegjs_code_124_1(iolist(), index()) -> parse_result().
pegjs_code_124_1(Node, Idx) ->

      [{_, Name}, _, {_, DisplayName}, _, _, {_, Expression}, _] = Node,
      #entry{ type = <<"rule">>
            , name = Name
            , expression = case DisplayName of
                              [String, _] ->
                                #entry{ type       = <<"named">>
                                      , name       = String
                                      , expression = entries(Expression)
                                      , index      = Idx
                                      };
                              [] ->
                                entries(Expression)
                           end
            , index = Idx
            }
    .

-spec pegjs_code_141_1(iolist(), index()) -> parse_result().
pegjs_code_141_1(Node, Idx) ->

      [{_, First}, {_, Rest}] = Node,
      case Rest of
        [] ->
          entries(First);
        _ ->
          #entry{ type         = <<"choice">>
                , alternatives = entries([First | Rest])
                , index       = Idx
                }
      end
    .

-spec pegjs_code_156_1(iolist(), index()) -> parse_result().
pegjs_code_156_1(Node, Idx) ->

      [{_, Expression}, {_, Code}] = Node,
      case Code of
        [] ->
          entries(Expression);
        [_, C]  ->
          #entry{ type       = <<"action">>
                , expression = entries(Expression)
                , code       = C
                , index       = Idx
                }
      end
    .

-spec pegjs_code_170_1(iolist(), index()) -> parse_result().
pegjs_code_170_1(Node, Idx) ->

      [{_, First}, {_, Rest}] = Node,
      case Rest of
        [] ->
          entries(First);
        _ ->
          #entry{ type     = <<"sequence">>
                , elements = entries([First | Rest])
                , index       = Idx
                }
      end
    .

-spec pegjs_code_179_3(iolist(), index()) -> parse_result().
pegjs_code_179_3(Node, Idx) ->

      [{_, Label}, _, _, _, {_, Expression}] = Node,
      #entry{ type       = <<"labeled">>
            , label      = Label
            , expression = entries(Expression)
            , index       = Idx
            }
    .

-spec pegjs_code_189_3(iolist(), index()) -> parse_result().
pegjs_code_189_3(Node, Idx) ->

      [{_, Operator}, _, {_, Expression}] = Node,
      #entry{ type       = ops_to_prefixed_types(Operator)
            , expression = entries(Expression)
            , index       = Idx
            }
    .

-spec pegjs_code_204_3(iolist(), index()) -> parse_result().
pegjs_code_204_3(Node, Idx) ->

      [{_, Expression}, _, {_, Operator}] = Node,
      #entry{ type       = ops_to_suffixed_types(Operator)
            , expression = entries(Expression)
            , index       = Idx
            }
    .

-spec pegjs_code_219_1(iolist(), index()) -> parse_result().
pegjs_code_219_1(Node, _Idx) ->
 [_, _, {_, Expression}, _, _] = Node, Expression .

-spec pegjs_code_228_1(iolist(), index()) -> parse_result().
pegjs_code_228_1(Node, Idx) ->

      [{_, Name}, _] = Node,
      #entry{ type  = <<"rule_ref">>
            , name  = Name
            , index = Idx
            }
    .

-spec pegjs_code_237_1(iolist(), index()) -> parse_result().
pegjs_code_237_1(Node, Idx) ->

      [{_, Operator}, _, {_, Code}] = Node,
      #entry{ type  = ops_to_semantic_predicate_types(Operator)
            , code  = Code
            , index = Idx
            }
    .

-spec pegjs_code_281_1(iolist(), index()) -> parse_result().
pegjs_code_281_1(Node, _Idx) ->
 [_, {_, Name}] = Node, Name .

-spec pegjs_code_284_1(iolist(), index()) -> parse_result().
pegjs_code_284_1(Node, _Idx) ->
 [{_, First}, {_, Rest}] = Node, iolist_to_binary([First, Rest]) .

-spec pegjs_code_290_1(iolist(), index()) -> parse_result().
pegjs_code_290_1(Node, _Idx) ->
 [_, {_, Sequence}] = Node, Sequence .

-spec pegjs_code_376_1(iolist(), index()) -> parse_result().
pegjs_code_376_1(Node, Idx) ->

      [{_, Value}, {_, IgnoreCase}] = Node,
      #entry{ type        = <<"literal">>
            , value       = Value
            , ignore_case = IgnoreCase /= []
            , index       = Idx
            }
    .

-spec pegjs_code_378_3(iolist(), index()) -> parse_result().
pegjs_code_378_3(Node, _Idx) ->
 [_, {_, Chars}, _] = Node, iolist_to_binary(Chars) .

-spec pegjs_code_380_1(iolist(), index()) -> parse_result().
pegjs_code_380_1(Node, _Idx) ->
 [_, {_, Chars}, _] = Node, iolist_to_binary(Chars) .

-spec pegjs_code_382_3(iolist(), index()) -> parse_result().
pegjs_code_382_3(Node, _Idx) ->
 text(Node) .

-spec pegjs_code_383_3(iolist(), index()) -> parse_result().
pegjs_code_383_3(Node, _Idx) ->
 [_, {_, Sequence}] = Node, Sequence .

-spec pegjs_code_387_3(iolist(), index()) -> parse_result().
pegjs_code_387_3(Node, _Idx) ->
 text(Node) .

-spec pegjs_code_388_3(iolist(), index()) -> parse_result().
pegjs_code_388_3(Node, _Idx) ->
 [_, {_, Sequence}] = Node, Sequence .

-spec pegjs_code_408_1(iolist(), index()) -> parse_result().
pegjs_code_408_1(Node, Idx) ->

      [_, {_, Inverted}, {_, Parts0}, _, {_, IgnoreCase}] = Node,
      Parts = filter_empty_strings(Parts0),
      #entry{ type        = <<"class">>
            , parts       = Parts
            , inverted    = Inverted /= []
            , ignore_case = IgnoreCase /= []
            , raw_text    = text(Node)
            , index       = Idx
            }
    .

-spec pegjs_code_419_1(iolist(), index()) -> parse_result().
pegjs_code_419_1(Node, Idx) ->

      [{_, Begin}, _, {_, End}] = Node,
      case Begin > End of
        true ->
          error({invalid_character_range, {Begin, End, Idx}});
        false ->
          [Begin, End]
      end
    .

-spec pegjs_code_421_3(iolist(), index()) -> parse_result().
pegjs_code_421_3(Node, _Idx) ->
 text(Node) .

-spec pegjs_code_422_3(iolist(), index()) -> parse_result().
pegjs_code_422_3(Node, _Idx) ->
 [_, {_, Sequence}] = Node, iolist_to_binary(Sequence) .

-spec pegjs_code_424_1(iolist(), index()) -> parse_result().
pegjs_code_424_1(Node, _Idx) ->
 text(Node) .

-spec pegjs_code_427_1(iolist(), index()) -> parse_result().
pegjs_code_427_1(_Node, _Idx) ->
 <<"">> .

-spec pegjs_code_430_3(iolist(), index()) -> parse_result().
pegjs_code_430_3(_Node, _Idx) ->
 <<"\0">> .

-spec pegjs_code_442_3(iolist(), index()) -> parse_result().
pegjs_code_442_3(_Node, _Idx) ->
 <<"\b">>   .

-spec pegjs_code_443_3(iolist(), index()) -> parse_result().
pegjs_code_443_3(_Node, _Idx) ->
 <<"\f">>   .

-spec pegjs_code_444_3(iolist(), index()) -> parse_result().
pegjs_code_444_3(_Node, _Idx) ->
 <<"\n">>   .

-spec pegjs_code_445_3(iolist(), index()) -> parse_result().
pegjs_code_445_3(_Node, _Idx) ->
 <<"\r">>   .

-spec pegjs_code_446_3(iolist(), index()) -> parse_result().
pegjs_code_446_3(_Node, _Idx) ->
 <<"\t">>   .

-spec pegjs_code_448_1(iolist(), index()) -> parse_result().
pegjs_code_448_1(_Node, _Idx) ->
 <<"\v">>   .

-spec pegjs_code_451_1(iolist(), index()) -> parse_result().
pegjs_code_451_1(Node, _Idx) ->
 text(Node) .

-spec pegjs_code_463_1(iolist(), index()) -> parse_result().
pegjs_code_463_1(Node, _Idx) ->

      [_, {_, Digits}] = Node,
      hexstr_to_bin(Digits)
    .

-spec pegjs_code_469_1(iolist(), index()) -> parse_result().
pegjs_code_469_1(Node, _Idx) ->

      [_, {_, Digits}] = Node,
      hexstr_to_bin(Digits)
    .

-spec pegjs_code_478_1(iolist(), index()) -> parse_result().
pegjs_code_478_1(_Node, _Idx) ->
 #entry{type = <<"any">>} .

-spec pegjs_code_481_1(iolist(), index()) -> parse_result().
pegjs_code_481_1(Node, _Idx) ->
 [_, {_, Code}, _] = Node, Code .

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
