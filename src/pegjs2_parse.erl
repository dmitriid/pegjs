-module(pegjs2_parse).

%%_* API ===================================================================
-export([ file/1
        , file/2
        , parse/1
        , parse/2
        ]).

-type input() :: binary().
-type parse_failure() :: {error, term()}.
-type parse_success() :: {term(), input(), index()}.
-type parse_result() :: parse_failure() | parse_success().
-type parse_fun() :: fun((input(), index()) -> parse_result()).
%-type xform_fun() :: fun((input(), index()) -> term()).

-record(pegjs_node, { input = <<>>
                    , index = {{line, 1}, {column, 1}}
                    , options = []
                    , match = []
                    }).

-define(DEBUG(W, Z), io:format("\n-------------\n" ++ W, Z)).

%-spec pegjs(input(), index(), binary(), parse_fun()
%, xform_fun()
%) -> parse_result().
-spec pegjs(#pegjs_node{}, binary(), parse_fun()) -> parse_result().
pegjs(#pegjs_node{index = StartIndex} = Node, Name, ParseFun) ->
  case get_memo(StartIndex, Name) of                 % See if the current reduction is memoized
    {ok, Memo} ->                                    % If it is, return the stored result
      Memo;
    _ ->                                             % If not, attempt to parse
      Result = case ParseFun(Node) of
        {error,_} = Failure ->                       % If it fails, memoize the failure
          Failure;
        #pegjs_node{} = Match ->                     % If it passes, transform and memoize the result.
          % Transformed = TransformFun(Match, StartIndex),
          %{Transformed, InpRem, NewIndex}
          Match;
        Other -> match(Other, Node)                  % for convenience, just return a match, no the full node
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

p_advance_index(MatchedInput, Index) when is_list(MatchedInput) orelse is_binary(MatchedInput)-> % strings
  lists:foldl(fun p_advance_index/2, Index, unicode:characters_to_list(MatchedInput));
p_advance_index(MatchedInput, Index) when is_integer(MatchedInput) -> % single characters
  {{line, Line}, {column, Col}} = Index,
  case MatchedInput of
    $\n -> {{line, Line+1}, {column, 1}};
    _ -> {{line, Line}, {column, Col+1}}
  end.

match(#pegjs_node{match = Match}) -> Match;
match(Other)                      -> Other.

match(New, Node)                  -> Node#pegjs_node{match = New}.

index(#pegjs_node{index = Index}) -> Index;
index(Other)                      -> Other.

input(#pegjs_node{input = Input}) -> Input;
input(Other)                      -> Other.

value(Label, #pegjs_node{match = Match}) ->
  case lists:keyfind(Label, 1, Match) of
    {_, Value} -> Value;
    false      -> undefined
  end.

text(Node) ->
  TextNode = [T || Match <- match(Node)
                 , T <- case Match of {_, Part} -> [Part]; Part -> [Part] end
             ],
  iolist_to_binary(TextNode).


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
hexstr_to_bin(<<D1/integer, D2/integer, D3/integer, D4/integer>>) ->
  Int = int(D1)*4096 + int(D2)*256 + int(D3)*16 + int(D4),
  unicode:characters_to_binary([Int]);
hexstr_to_bin(<<D1/integer, D2/integer>>) ->
  Int = int(D1) * 16 + int(D2),
  Codepoints = [Int],
  unicode:characters_to_binary(Codepoints);
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
file(Filename) -> file(Filename, 'Grammar').

-spec file(file:name(), binary()) -> any().
file(Filename, Root) ->
  case file:read_file(Filename) of 
    {ok,Bin} -> parse(Bin, Root);
    Err      -> Err
end.

-spec parse(binary() | list()) -> any().
parse(List) when is_list(List) -> parse(list_to_binary(List));
parse(Input) when is_binary(Input) ->
  parse(Input, 'Grammar').

-spec parse(binary() | list(), binary()) -> any().
parse(List, Root) when is_list(List) -> 
  parse(list_to_binary(List), Root);
parse(Input, Root) when is_binary(Input) ->
  setup_memo(),
  RuleResult = pegjs_rule(Root, #pegjs_node{ input = Input 
                                           , index = {{line,1},{column,1}}}),
  Result =  case input(RuleResult) of
              <<>>          ->
                match(RuleResult);
             {error, Error} ->
                {error, Error};
              Unparsed      ->
                {error, {no_match, {Unparsed, index(RuleResult)}}}
           end,
  release_memo(),
Result.

pegjs_rule('Grammar', Node) -> 
  pegjs( Node
       , 'Grammar'
       , fun(N) -> 
           (pegjs_combinator_action(fun pegjs_custom_fun_93_6/1, pegjs_combinator_sequence([pegjs_combinator_rule_ref('__'), pegjs_combinator_labeled(<<"initializer">>, pegjs_combinator_suffixed('optional', pegjs_combinator_sequence([pegjs_combinator_rule_ref('Initializer'), pegjs_combinator_rule_ref('__')]))), pegjs_combinator_labeled(<<"rules">>, pegjs_combinator_suffixed('one_or_more', pegjs_combinator_sequence([pegjs_combinator_rule_ref('Rule'), pegjs_combinator_rule_ref('__')])))])))(N)
         end
       );
pegjs_rule('Initializer', Node) -> 
  pegjs( Node
       , 'Initializer'
       , fun(N) -> 
           (pegjs_combinator_action(fun pegjs_custom_fun_96_126/1, pegjs_combinator_sequence([pegjs_combinator_labeled(<<"code">>, pegjs_combinator_rule_ref('CodeBlock')), pegjs_combinator_rule_ref('EOS')])))(N)
         end
       );
pegjs_rule('Rule', Node) -> 
  pegjs( Node
       , 'Rule'
       , fun(N) -> 
           (pegjs_combinator_action(fun pegjs_custom_fun_121_6/1, pegjs_combinator_sequence([pegjs_combinator_labeled(<<"name">>, pegjs_combinator_rule_ref('IdentifierName')), pegjs_combinator_rule_ref('__'), pegjs_combinator_labeled(<<"displayName">>, pegjs_combinator_suffixed('optional', pegjs_combinator_sequence([pegjs_combinator_rule_ref('StringLiteral'), pegjs_combinator_rule_ref('__')]))), pegjs_combinator_literal(<<61>>, false), pegjs_combinator_rule_ref('__'), pegjs_combinator_labeled(<<"expression">>, pegjs_combinator_rule_ref('Expression')), pegjs_combinator_rule_ref('EOS')])))(N)
         end
       );
pegjs_rule('Expression', Node) -> 
  pegjs( Node
       , 'Expression'
       , fun(N) -> 
           (pegjs_combinator_rule_ref('ChoiceExpression'))(N)
         end
       );
pegjs_rule('ChoiceExpression', Node) -> 
  pegjs( Node
       , 'ChoiceExpression'
       , fun(N) -> 
           (pegjs_combinator_action(fun pegjs_custom_fun_139_6/1, pegjs_combinator_sequence([pegjs_combinator_labeled(<<"first">>, pegjs_combinator_rule_ref('ActionExpression')), pegjs_combinator_labeled(<<"rest">>, pegjs_combinator_suffixed('zero_or_more', pegjs_combinator_sequence([pegjs_combinator_rule_ref('__'), pegjs_combinator_literal(<<47>>, false), pegjs_combinator_rule_ref('__'), pegjs_combinator_rule_ref('ActionExpression')])))])))(N)
         end
       );
pegjs_rule('ActionExpression', Node) -> 
  pegjs( Node
       , 'ActionExpression'
       , fun(N) -> 
           (pegjs_combinator_action(fun pegjs_custom_fun_155_6/1, pegjs_combinator_sequence([pegjs_combinator_labeled(<<"expression">>, pegjs_combinator_rule_ref('SequenceExpression')), pegjs_combinator_labeled(<<"code">>, pegjs_combinator_suffixed('optional', pegjs_combinator_sequence([pegjs_combinator_rule_ref('__'), pegjs_combinator_rule_ref('CodeBlock')])))])))(N)
         end
       );
pegjs_rule('SequenceExpression', Node) -> 
  pegjs( Node
       , 'SequenceExpression'
       , fun(N) -> 
           (pegjs_combinator_action(fun pegjs_custom_fun_170_6/1, pegjs_combinator_sequence([pegjs_combinator_labeled(<<"first">>, pegjs_combinator_rule_ref('LabeledExpression')), pegjs_combinator_labeled(<<"rest">>, pegjs_combinator_suffixed('zero_or_more', pegjs_combinator_sequence([pegjs_combinator_rule_ref('__'), pegjs_combinator_rule_ref('LabeledExpression')])))])))(N)
         end
       );
pegjs_rule('LabeledExpression', Node) -> 
  pegjs( Node
       , 'LabeledExpression'
       , fun(N) -> 
           (pegjs_combinator_choice([pegjs_combinator_action(fun pegjs_custom_fun_181_6/1, pegjs_combinator_sequence([pegjs_combinator_labeled(<<"label">>, pegjs_combinator_rule_ref('Identifier')), pegjs_combinator_rule_ref('__'), pegjs_combinator_literal(<<58>>, false), pegjs_combinator_rule_ref('__'), pegjs_combinator_labeled(<<"expression">>, pegjs_combinator_rule_ref('PrefixedExpression'))])), pegjs_combinator_rule_ref('PrefixedExpression')]))(N)
         end
       );
pegjs_rule('PrefixedExpression', Node) -> 
  pegjs( Node
       , 'PrefixedExpression'
       , fun(N) -> 
           (pegjs_combinator_choice([pegjs_combinator_action(fun pegjs_custom_fun_192_6/1, pegjs_combinator_sequence([pegjs_combinator_labeled(<<"operator">>, pegjs_combinator_rule_ref('PrefixedOperator')), pegjs_combinator_rule_ref('__'), pegjs_combinator_labeled(<<"expression">>, pegjs_combinator_rule_ref('SuffixedExpression'))])), pegjs_combinator_rule_ref('SuffixedExpression')]))(N)
         end
       );
pegjs_rule('PrefixedOperator', Node) -> 
  pegjs( Node
       , 'PrefixedOperator'
       , fun(N) -> 
           (pegjs_combinator_choice([pegjs_combinator_literal(<<36>>, false), pegjs_combinator_literal(<<38>>, false), pegjs_combinator_literal(<<33>>, false)]))(N)
         end
       );
pegjs_rule('SuffixedExpression', Node) -> 
  pegjs( Node
       , 'SuffixedExpression'
       , fun(N) -> 
           (pegjs_combinator_choice([pegjs_combinator_action(fun pegjs_custom_fun_208_6/1, pegjs_combinator_sequence([pegjs_combinator_labeled(<<"expression">>, pegjs_combinator_rule_ref('PrimaryExpression')), pegjs_combinator_rule_ref('__'), pegjs_combinator_labeled(<<"operator">>, pegjs_combinator_rule_ref('SuffixedOperator'))])), pegjs_combinator_rule_ref('PrimaryExpression')]))(N)
         end
       );
pegjs_rule('SuffixedOperator', Node) -> 
  pegjs( Node
       , 'SuffixedOperator'
       , fun(N) -> 
           (pegjs_combinator_choice([pegjs_combinator_literal(<<63>>, false), pegjs_combinator_literal(<<42>>, false), pegjs_combinator_literal(<<43>>, false)]))(N)
         end
       );
pegjs_rule('PrimaryExpression', Node) -> 
  pegjs( Node
       , 'PrimaryExpression'
       , fun(N) -> 
           (pegjs_combinator_choice([pegjs_combinator_rule_ref('LiteralMatcher'), pegjs_combinator_rule_ref('CharacterClassMatcher'), pegjs_combinator_rule_ref('AnyMatcher'), pegjs_combinator_rule_ref('RuleReferenceExpression'), pegjs_combinator_rule_ref('SemanticPredicateExpression'), pegjs_combinator_action(fun pegjs_custom_fun_222_74/1, pegjs_combinator_sequence([pegjs_combinator_literal(<<40>>, false), pegjs_combinator_rule_ref('__'), pegjs_combinator_labeled(<<"expression">>, pegjs_combinator_rule_ref('Expression')), pegjs_combinator_rule_ref('__'), pegjs_combinator_literal(<<41>>, false)]))]))(N)
         end
       );
pegjs_rule('RuleReferenceExpression', Node) -> 
  pegjs( Node
       , 'RuleReferenceExpression'
       , fun(N) -> 
           (pegjs_combinator_action(fun pegjs_custom_fun_230_6/1, pegjs_combinator_sequence([pegjs_combinator_labeled(<<"name">>, pegjs_combinator_rule_ref('IdentifierName')), pegjs_combinator_prefixed('simple_not', pegjs_combinator_sequence([pegjs_combinator_rule_ref('__'), pegjs_combinator_suffixed('optional', pegjs_combinator_sequence([pegjs_combinator_rule_ref('StringLiteral'), pegjs_combinator_rule_ref('__')])), pegjs_combinator_literal(<<61>>, false)]))])))(N)
         end
       );
pegjs_rule('SemanticPredicateExpression', Node) -> 
  pegjs( Node
       , 'SemanticPredicateExpression'
       , fun(N) -> 
           (pegjs_combinator_action(fun pegjs_custom_fun_240_6/1, pegjs_combinator_sequence([pegjs_combinator_labeled(<<"operator">>, pegjs_combinator_rule_ref('SemanticPredicateOperator')), pegjs_combinator_rule_ref('__'), pegjs_combinator_labeled(<<"code">>, pegjs_combinator_rule_ref('CodeBlock'))])))(N)
         end
       );
pegjs_rule('SemanticPredicateOperator', Node) -> 
  pegjs( Node
       , 'SemanticPredicateOperator'
       , fun(N) -> 
           (pegjs_combinator_choice([pegjs_combinator_literal(<<38>>, false), pegjs_combinator_literal(<<33>>, false)]))(N)
         end
       );
pegjs_rule('SourceCharacter', Node) -> 
  pegjs( Node
       , 'SourceCharacter'
       , fun(N) -> 
           (pegjs_combinator_any())(N)
         end
       );
pegjs_rule('WhiteSpace', Node) -> 
  pegjs( Node
       , 'WhiteSpace'
       , fun(N) -> 
           (pegjs_combinator_named(pegjs_combinator_choice([pegjs_combinator_literal(<<9>>, false), pegjs_combinator_literal(<<11>>, false), pegjs_combinator_literal(<<12>>, false), pegjs_combinator_literal(<<32>>, false), pegjs_combinator_literal(<<194,160>>, false), pegjs_combinator_literal(<<239,187,191>>, false), pegjs_combinator_rule_ref('Zs')])))(N)
         end
       );
pegjs_rule('LineTerminator', Node) -> 
  pegjs( Node
       , 'LineTerminator'
       , fun(N) -> 
           (pegjs_combinator_regexp(<<94,91,10,13,226,128,168,226,128,169,93>>, false))(N)
         end
       );
pegjs_rule('LineTerminatorSequence', Node) -> 
  pegjs( Node
       , 'LineTerminatorSequence'
       , fun(N) -> 
           (pegjs_combinator_named(pegjs_combinator_choice([pegjs_combinator_literal(<<10>>, false), pegjs_combinator_literal(<<13,10>>, false), pegjs_combinator_literal(<<13>>, false), pegjs_combinator_literal(<<226,128,168>>, false), pegjs_combinator_literal(<<226,128,169>>, false)])))(N)
         end
       );
pegjs_rule('Comment', Node) -> 
  pegjs( Node
       , 'Comment'
       , fun(N) -> 
           (pegjs_combinator_named(pegjs_combinator_choice([pegjs_combinator_rule_ref('MultiLineComment'), pegjs_combinator_rule_ref('SingleLineComment')])))(N)
         end
       );
pegjs_rule('MultiLineComment', Node) -> 
  pegjs( Node
       , 'MultiLineComment'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<47,42>>, false), pegjs_combinator_suffixed('zero_or_more', pegjs_combinator_sequence([pegjs_combinator_prefixed('simple_not', pegjs_combinator_literal(<<42,47>>, false)), pegjs_combinator_rule_ref('SourceCharacter')])), pegjs_combinator_literal(<<42,47>>, false)]))(N)
         end
       );
pegjs_rule('MultiLineCommentNoLineTerminator', Node) -> 
  pegjs( Node
       , 'MultiLineCommentNoLineTerminator'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<47,42>>, false), pegjs_combinator_suffixed('zero_or_more', pegjs_combinator_sequence([pegjs_combinator_prefixed('simple_not', pegjs_combinator_choice([pegjs_combinator_literal(<<42,47>>, false), pegjs_combinator_rule_ref('LineTerminator')])), pegjs_combinator_rule_ref('SourceCharacter')])), pegjs_combinator_literal(<<42,47>>, false)]))(N)
         end
       );
pegjs_rule('SingleLineComment', Node) -> 
  pegjs( Node
       , 'SingleLineComment'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<47,47>>, false), pegjs_combinator_suffixed('zero_or_more', pegjs_combinator_sequence([pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('LineTerminator')), pegjs_combinator_rule_ref('SourceCharacter')]))]))(N)
         end
       );
pegjs_rule('Identifier', Node) -> 
  pegjs( Node
       , 'Identifier'
       , fun(N) -> 
           (pegjs_combinator_action(fun pegjs_custom_fun_284_66/1, pegjs_combinator_sequence([pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('ReservedWord')), pegjs_combinator_labeled(<<"name">>, pegjs_combinator_rule_ref('IdentifierName'))])))(N)
         end
       );
pegjs_rule('IdentifierName', Node) -> 
  pegjs( Node
       , 'IdentifierName'
       , fun(N) -> 
           (pegjs_combinator_named(pegjs_combinator_action(fun pegjs_custom_fun_287_121/1, pegjs_combinator_sequence([pegjs_combinator_labeled(<<"first">>, pegjs_combinator_rule_ref('IdentifierStart')), pegjs_combinator_labeled(<<"rest">>, pegjs_combinator_suffixed('zero_or_more', pegjs_combinator_rule_ref('IdentifierPart')))]))))(N)
         end
       );
pegjs_rule('IdentifierStart', Node) -> 
  pegjs( Node
       , 'IdentifierStart'
       , fun(N) -> 
           (pegjs_combinator_choice([pegjs_combinator_rule_ref('UnicodeLetter'), pegjs_combinator_literal(<<36>>, false), pegjs_combinator_literal(<<95>>, false), pegjs_combinator_action(fun pegjs_custom_fun_293_72/1, pegjs_combinator_sequence([pegjs_combinator_literal(<<92>>, false), pegjs_combinator_labeled(<<"sequence">>, pegjs_combinator_rule_ref('UnicodeEscapeSequence'))]))]))(N)
         end
       );
pegjs_rule('IdentifierPart', Node) -> 
  pegjs( Node
       , 'IdentifierPart'
       , fun(N) -> 
           (pegjs_combinator_choice([pegjs_combinator_rule_ref('IdentifierStart'), pegjs_combinator_rule_ref('UnicodeCombiningMark'), pegjs_combinator_rule_ref('UnicodeDigit'), pegjs_combinator_rule_ref('UnicodeConnectorPunctuation'), pegjs_combinator_literal(<<226,128,140>>, false), pegjs_combinator_literal(<<226,128,141>>, false)]))(N)
         end
       );
pegjs_rule('UnicodeLetter', Node) -> 
  pegjs( Node
       , 'UnicodeLetter'
       , fun(N) -> 
           (pegjs_combinator_choice([pegjs_combinator_rule_ref('Lu'), pegjs_combinator_rule_ref('Ll'), pegjs_combinator_rule_ref('Lt'), pegjs_combinator_rule_ref('Lm'), pegjs_combinator_rule_ref('Lo'), pegjs_combinator_rule_ref('Nl')]))(N)
         end
       );
pegjs_rule('UnicodeCombiningMark', Node) -> 
  pegjs( Node
       , 'UnicodeCombiningMark'
       , fun(N) -> 
           (pegjs_combinator_choice([pegjs_combinator_rule_ref('Mn'), pegjs_combinator_rule_ref('Mc')]))(N)
         end
       );
pegjs_rule('UnicodeDigit', Node) -> 
  pegjs( Node
       , 'UnicodeDigit'
       , fun(N) -> 
           (pegjs_combinator_rule_ref('Nd'))(N)
         end
       );
pegjs_rule('UnicodeConnectorPunctuation', Node) -> 
  pegjs( Node
       , 'UnicodeConnectorPunctuation'
       , fun(N) -> 
           (pegjs_combinator_rule_ref('Pc'))(N)
         end
       );
pegjs_rule('ReservedWord', Node) -> 
  pegjs( Node
       , 'ReservedWord'
       , fun(N) -> 
           (pegjs_combinator_choice([pegjs_combinator_rule_ref('Keyword'), pegjs_combinator_rule_ref('FutureReservedWord'), pegjs_combinator_rule_ref('NullLiteral'), pegjs_combinator_rule_ref('BooleanLiteral')]))(N)
         end
       );
pegjs_rule('Keyword', Node) -> 
  pegjs( Node
       , 'Keyword'
       , fun(N) -> 
           (pegjs_combinator_choice([pegjs_combinator_rule_ref('BreakToken'), pegjs_combinator_rule_ref('CaseToken'), pegjs_combinator_rule_ref('CatchToken'), pegjs_combinator_rule_ref('ContinueToken'), pegjs_combinator_rule_ref('DebuggerToken'), pegjs_combinator_rule_ref('DefaultToken'), pegjs_combinator_rule_ref('DeleteToken'), pegjs_combinator_rule_ref('DoToken'), pegjs_combinator_rule_ref('ElseToken'), pegjs_combinator_rule_ref('FinallyToken'), pegjs_combinator_rule_ref('ForToken'), pegjs_combinator_rule_ref('FunctionToken'), pegjs_combinator_rule_ref('IfToken'), pegjs_combinator_rule_ref('InstanceofToken'), pegjs_combinator_rule_ref('InToken'), pegjs_combinator_rule_ref('NewToken'), pegjs_combinator_rule_ref('ReturnToken'), pegjs_combinator_rule_ref('SwitchToken'), pegjs_combinator_rule_ref('ThisToken'), pegjs_combinator_rule_ref('ThrowToken'), pegjs_combinator_rule_ref('TryToken'), pegjs_combinator_rule_ref('TypeofToken'), pegjs_combinator_rule_ref('VarToken'), pegjs_combinator_rule_ref('VoidToken'), pegjs_combinator_rule_ref('WhileToken'), pegjs_combinator_rule_ref('WithToken')]))(N)
         end
       );
pegjs_rule('FutureReservedWord', Node) -> 
  pegjs( Node
       , 'FutureReservedWord'
       , fun(N) -> 
           (pegjs_combinator_choice([pegjs_combinator_rule_ref('ClassToken'), pegjs_combinator_rule_ref('ConstToken'), pegjs_combinator_rule_ref('EnumToken'), pegjs_combinator_rule_ref('ExportToken'), pegjs_combinator_rule_ref('ExtendsToken'), pegjs_combinator_rule_ref('ImportToken'), pegjs_combinator_rule_ref('SuperToken')]))(N)
         end
       );
pegjs_rule('NullLiteral', Node) -> 
  pegjs( Node
       , 'NullLiteral'
       , fun(N) -> 
           (pegjs_combinator_rule_ref('NullToken'))(N)
         end
       );
pegjs_rule('BooleanLiteral', Node) -> 
  pegjs( Node
       , 'BooleanLiteral'
       , fun(N) -> 
           (pegjs_combinator_choice([pegjs_combinator_rule_ref('TrueToken'), pegjs_combinator_rule_ref('FalseToken')]))(N)
         end
       );
pegjs_rule('LiteralMatcher', Node) -> 
  pegjs( Node
       , 'LiteralMatcher'
       , fun(N) -> 
           (pegjs_combinator_named(pegjs_combinator_action(fun pegjs_custom_fun_378_6/1, pegjs_combinator_sequence([pegjs_combinator_labeled(<<"value">>, pegjs_combinator_rule_ref('StringLiteral')), pegjs_combinator_labeled(<<"ignoreCase">>, pegjs_combinator_suffixed('optional', pegjs_combinator_literal(<<105>>, false)))]))))(N)
         end
       );
pegjs_rule('StringLiteral', Node) -> 
  pegjs( Node
       , 'StringLiteral'
       , fun(N) -> 
           (pegjs_combinator_named(pegjs_combinator_choice([pegjs_combinator_action(fun pegjs_custom_fun_381_88/1, pegjs_combinator_sequence([pegjs_combinator_literal(<<34>>, false), pegjs_combinator_labeled(<<"chars">>, pegjs_combinator_suffixed('zero_or_more', pegjs_combinator_rule_ref('DoubleStringCharacter'))), pegjs_combinator_literal(<<34>>, false)])), pegjs_combinator_action(fun pegjs_custom_fun_382_88/1, pegjs_combinator_sequence([pegjs_combinator_literal(<<39>>, false), pegjs_combinator_labeled(<<"chars">>, pegjs_combinator_suffixed('zero_or_more', pegjs_combinator_rule_ref('SingleStringCharacter'))), pegjs_combinator_literal(<<39>>, false)]))])))(N)
         end
       );
pegjs_rule('DoubleStringCharacter', Node) -> 
  pegjs( Node
       , 'DoubleStringCharacter'
       , fun(N) -> 
           (pegjs_combinator_choice([pegjs_combinator_action(fun pegjs_custom_fun_385_66/1, pegjs_combinator_sequence([pegjs_combinator_prefixed('simple_not', pegjs_combinator_choice([pegjs_combinator_literal(<<34>>, false), pegjs_combinator_literal(<<92>>, false), pegjs_combinator_rule_ref('LineTerminator')])), pegjs_combinator_rule_ref('SourceCharacter')])), pegjs_combinator_action(fun pegjs_custom_fun_386_65/1, pegjs_combinator_sequence([pegjs_combinator_literal(<<92>>, false), pegjs_combinator_labeled(<<"sequence">>, pegjs_combinator_rule_ref('EscapeSequence'))])), pegjs_combinator_rule_ref('LineContinuation')]))(N)
         end
       );
pegjs_rule('SingleStringCharacter', Node) -> 
  pegjs( Node
       , 'SingleStringCharacter'
       , fun(N) -> 
           (pegjs_combinator_choice([pegjs_combinator_action(fun pegjs_custom_fun_390_66/1, pegjs_combinator_sequence([pegjs_combinator_prefixed('simple_not', pegjs_combinator_choice([pegjs_combinator_literal(<<39>>, false), pegjs_combinator_literal(<<92>>, false), pegjs_combinator_rule_ref('LineTerminator')])), pegjs_combinator_rule_ref('SourceCharacter')])), pegjs_combinator_action(fun pegjs_custom_fun_391_65/1, pegjs_combinator_sequence([pegjs_combinator_literal(<<92>>, false), pegjs_combinator_labeled(<<"sequence">>, pegjs_combinator_rule_ref('EscapeSequence'))])), pegjs_combinator_rule_ref('LineContinuation')]))(N)
         end
       );
pegjs_rule('CharacterClassMatcher', Node) -> 
  pegjs( Node
       , 'CharacterClassMatcher'
       , fun(N) -> 
           (pegjs_combinator_named(pegjs_combinator_action(fun pegjs_custom_fun_412_6/1, pegjs_combinator_sequence([pegjs_combinator_literal(<<91>>, false), pegjs_combinator_labeled(<<"inverted">>, pegjs_combinator_suffixed('optional', pegjs_combinator_literal(<<94>>, false))), pegjs_combinator_labeled(<<"parts">>, pegjs_combinator_suffixed('zero_or_more', pegjs_combinator_choice([pegjs_combinator_rule_ref('ClassCharacterRange'), pegjs_combinator_rule_ref('ClassCharacter')]))), pegjs_combinator_literal(<<93>>, false), pegjs_combinator_labeled(<<"ignoreCase">>, pegjs_combinator_suffixed('optional', pegjs_combinator_literal(<<105>>, false)))]))))(N)
         end
       );
pegjs_rule('ClassCharacterRange', Node) -> 
  pegjs( Node
       , 'ClassCharacterRange'
       , fun(N) -> 
           (pegjs_combinator_action(fun pegjs_custom_fun_424_6/1, pegjs_combinator_sequence([pegjs_combinator_labeled(<<"begin">>, pegjs_combinator_rule_ref('ClassCharacter')), pegjs_combinator_literal(<<45>>, false), pegjs_combinator_labeled(<<"end">>, pegjs_combinator_rule_ref('ClassCharacter'))])))(N)
         end
       );
pegjs_rule('ClassCharacter', Node) -> 
  pegjs( Node
       , 'ClassCharacter'
       , fun(N) -> 
           (pegjs_combinator_choice([pegjs_combinator_action(fun pegjs_custom_fun_427_66/1, pegjs_combinator_sequence([pegjs_combinator_prefixed('simple_not', pegjs_combinator_choice([pegjs_combinator_literal(<<93>>, false), pegjs_combinator_literal(<<92>>, false), pegjs_combinator_rule_ref('LineTerminator')])), pegjs_combinator_rule_ref('SourceCharacter')])), pegjs_combinator_action(fun pegjs_custom_fun_428_83/1, pegjs_combinator_sequence([pegjs_combinator_literal(<<92>>, false), pegjs_combinator_labeled(<<"sequence">>, pegjs_combinator_rule_ref('EscapeSequence'))])), pegjs_combinator_action(fun pegjs_custom_fun_429_36/1, pegjs_combinator_rule_ref('LineContinuation'))]))(N)
         end
       );
pegjs_rule('LineContinuation', Node) -> 
  pegjs( Node
       , 'LineContinuation'
       , fun(N) -> 
           (pegjs_combinator_action(fun pegjs_custom_fun_432_43/1, pegjs_combinator_sequence([pegjs_combinator_literal(<<92>>, false), pegjs_combinator_rule_ref('LineTerminatorSequence')])))(N)
         end
       );
pegjs_rule('EscapeSequence', Node) -> 
  pegjs( Node
       , 'EscapeSequence'
       , fun(N) -> 
           (pegjs_combinator_choice([pegjs_combinator_rule_ref('CharacterEscapeSequence'), pegjs_combinator_action(fun pegjs_custom_fun_436_35/1, pegjs_combinator_sequence([pegjs_combinator_literal(<<48>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('DecimalDigit'))])), pegjs_combinator_rule_ref('HexEscapeSequence'), pegjs_combinator_rule_ref('UnicodeEscapeSequence')]))(N)
         end
       );
pegjs_rule('CharacterEscapeSequence', Node) -> 
  pegjs( Node
       , 'CharacterEscapeSequence'
       , fun(N) -> 
           (pegjs_combinator_choice([pegjs_combinator_rule_ref('SingleEscapeCharacter'), pegjs_combinator_rule_ref('NonEscapeCharacter')]))(N)
         end
       );
pegjs_rule('SingleEscapeCharacter', Node) -> 
  pegjs( Node
       , 'SingleEscapeCharacter'
       , fun(N) -> 
           (pegjs_combinator_choice([pegjs_combinator_literal(<<39>>, false), pegjs_combinator_literal(<<34>>, false), pegjs_combinator_literal(<<92>>, false), pegjs_combinator_action(fun pegjs_custom_fun_448_24/1, pegjs_combinator_literal(<<98>>, false)), pegjs_combinator_action(fun pegjs_custom_fun_449_24/1, pegjs_combinator_literal(<<102>>, false)), pegjs_combinator_action(fun pegjs_custom_fun_450_24/1, pegjs_combinator_literal(<<110>>, false)), pegjs_combinator_action(fun pegjs_custom_fun_451_24/1, pegjs_combinator_literal(<<114>>, false)), pegjs_combinator_action(fun pegjs_custom_fun_452_24/1, pegjs_combinator_literal(<<116>>, false)), pegjs_combinator_action(fun pegjs_custom_fun_453_24/1, pegjs_combinator_literal(<<118>>, false))]))(N)
         end
       );
pegjs_rule('NonEscapeCharacter', Node) -> 
  pegjs( Node
       , 'NonEscapeCharacter'
       , fun(N) -> 
           (pegjs_combinator_action(fun pegjs_custom_fun_456_71/1, pegjs_combinator_sequence([pegjs_combinator_prefixed('simple_not', pegjs_combinator_choice([pegjs_combinator_rule_ref('EscapeCharacter'), pegjs_combinator_rule_ref('LineTerminator')])), pegjs_combinator_rule_ref('SourceCharacter')])))(N)
         end
       );
pegjs_rule('EscapeCharacter', Node) -> 
  pegjs( Node
       , 'EscapeCharacter'
       , fun(N) -> 
           (pegjs_combinator_choice([pegjs_combinator_rule_ref('SingleEscapeCharacter'), pegjs_combinator_rule_ref('DecimalDigit'), pegjs_combinator_literal(<<120>>, false), pegjs_combinator_literal(<<117>>, false)]))(N)
         end
       );
pegjs_rule('HexEscapeSequence', Node) -> 
  pegjs( Node
       , 'HexEscapeSequence'
       , fun(N) -> 
           (pegjs_combinator_action(fun pegjs_custom_fun_467_6/1, pegjs_combinator_sequence([pegjs_combinator_literal(<<120>>, false), pegjs_combinator_labeled(<<"digits">>, pegjs_combinator_text(pegjs_combinator_sequence([pegjs_combinator_rule_ref('HexDigit'), pegjs_combinator_rule_ref('HexDigit')])))])))(N)
         end
       );
pegjs_rule('UnicodeEscapeSequence', Node) -> 
  pegjs( Node
       , 'UnicodeEscapeSequence'
       , fun(N) -> 
           (pegjs_combinator_action(fun pegjs_custom_fun_472_6/1, pegjs_combinator_sequence([pegjs_combinator_literal(<<117>>, false), pegjs_combinator_labeled(<<"digits">>, pegjs_combinator_text(pegjs_combinator_sequence([pegjs_combinator_rule_ref('HexDigit'), pegjs_combinator_rule_ref('HexDigit'), pegjs_combinator_rule_ref('HexDigit'), pegjs_combinator_rule_ref('HexDigit')])))])))(N)
         end
       );
pegjs_rule('DecimalDigit', Node) -> 
  pegjs( Node
       , 'DecimalDigit'
       , fun(N) -> 
           (pegjs_combinator_regexp(<<94,91,48,45,57,93>>, false))(N)
         end
       );
pegjs_rule('HexDigit', Node) -> 
  pegjs( Node
       , 'HexDigit'
       , fun(N) -> 
           (pegjs_combinator_regexp(<<94,91,48,45,57,97,45,102,93>>, true))(N)
         end
       );
pegjs_rule('AnyMatcher', Node) -> 
  pegjs( Node
       , 'AnyMatcher'
       , fun(N) -> 
           (pegjs_combinator_action(fun pegjs_custom_fun_481_37/1, pegjs_combinator_literal(<<46>>, false)))(N)
         end
       );
pegjs_rule('CodeBlock', Node) -> 
  pegjs( Node
       , 'CodeBlock'
       , fun(N) -> 
           (pegjs_combinator_named(pegjs_combinator_action(fun pegjs_custom_fun_484_50/1, pegjs_combinator_sequence([pegjs_combinator_literal(<<123>>, false), pegjs_combinator_labeled(<<"code">>, pegjs_combinator_rule_ref('Code')), pegjs_combinator_literal(<<125>>, false)]))))(N)
         end
       );
pegjs_rule('Code', Node) -> 
  pegjs( Node
       , 'Code'
       , fun(N) -> 
           (pegjs_combinator_text(pegjs_combinator_suffixed('zero_or_more', pegjs_combinator_choice([pegjs_combinator_suffixed('one_or_more', pegjs_combinator_sequence([pegjs_combinator_prefixed('simple_not', pegjs_combinator_regexp(<<94,91,123,125,93>>, false)), pegjs_combinator_rule_ref('SourceCharacter')])), pegjs_combinator_sequence([pegjs_combinator_literal(<<123>>, false), pegjs_combinator_rule_ref('Code'), pegjs_combinator_literal(<<125>>, false)])]))))(N)
         end
       );
pegjs_rule('Ll', Node) -> 
  pegjs( Node
       , 'Ll'
       , fun(N) -> 
           (pegjs_combinator_regexp(<<94,91,97,45,122,194,181,195,159,45,195,182,195,184,45,195,191,196,129,196,131,196,133,196,135,196,137,196,139,196,141,196,143,196,145,196,147,196,149,196,151,196,153,196,155,196,157,196,159,196,161,196,163,196,165,196,167,196,169,196,171,196,173,196,175,196,177,196,179,196,181,196,183,45,196,184,196,186,196,188,196,190,197,128,197,130,197,132,197,134,197,136,45,197,137,197,139,197,141,197,143,197,145,197,147,197,149,197,151,197,153,197,155,197,157,197,159,197,161,197,163,197,165,197,167,197,169,197,171,197,173,197,175,197,177,197,179,197,181,197,183,197,186,197,188,197,190,45,198,128,198,131,198,133,198,136,198,140,45,198,141,198,146,198,149,198,153,45,198,155,198,158,198,161,198,163,198,165,198,168,198,170,45,198,171,198,173,198,176,198,180,198,182,198,185,45,198,186,198,189,45,198,191,199,134,199,137,199,140,199,142,199,144,199,146,199,148,199,150,199,152,199,154,199,156,45,199,157,199,159,199,161,199,163,199,165,199,167,199,169,199,171,199,173,199,175,45,199,176,199,179,199,181,199,185,199,187,199,189,199,191,200,129,200,131,200,133,200,135,200,137,200,139,200,141,200,143,200,145,200,147,200,149,200,151,200,153,200,155,200,157,200,159,200,161,200,163,200,165,200,167,200,169,200,171,200,173,200,175,200,177,200,179,45,200,185,200,188,200,191,45,201,128,201,130,201,135,201,137,201,139,201,141,201,143,45,202,147,202,149,45,202,175,205,177,205,179,205,183,205,187,45,205,189,206,144,206,172,45,207,142,207,144,45,207,145,207,149,45,207,151,207,153,207,155,207,157,207,159,207,161,207,163,207,165,207,167,207,169,207,171,207,173,207,175,45,207,179,207,181,207,184,207,187,45,207,188,208,176,45,209,159,209,161,209,163,209,165,209,167,209,169,209,171,209,173,209,175,209,177,209,179,209,181,209,183,209,185,209,187,209,189,209,191,210,129,210,139,210,141,210,143,210,145,210,147,210,149,210,151,210,153,210,155,210,157,210,159,210,161,210,163,210,165,210,167,210,169,210,171,210,173,210,175,210,177,210,179,210,181,210,183,210,185,210,187,210,189,210,191,211,130,211,132,211,134,211,136,211,138,211,140,211,142,45,211,143,211,145,211,147,211,149,211,151,211,153,211,155,211,157,211,159,211,161,211,163,211,165,211,167,211,169,211,171,211,173,211,175,211,177,211,179,211,181,211,183,211,185,211,187,211,189,211,191,212,129,212,131,212,133,212,135,212,137,212,139,212,141,212,143,212,145,212,147,212,149,212,151,212,153,212,155,212,157,212,159,212,161,212,163,212,165,212,167,213,161,45,214,135,225,180,128,45,225,180,171,225,181,171,45,225,181,183,225,181,185,45,225,182,154,225,184,129,225,184,131,225,184,133,225,184,135,225,184,137,225,184,139,225,184,141,225,184,143,225,184,145,225,184,147,225,184,149,225,184,151,225,184,153,225,184,155,225,184,157,225,184,159,225,184,161,225,184,163,225,184,165,225,184,167,225,184,169,225,184,171,225,184,173,225,184,175,225,184,177,225,184,179,225,184,181,225,184,183,225,184,185,225,184,187,225,184,189,225,184,191,225,185,129,225,185,131,225,185,133,225,185,135,225,185,137,225,185,139,225,185,141,225,185,143,225,185,145,225,185,147,225,185,149,225,185,151,225,185,153,225,185,155,225,185,157,225,185,159,225,185,161,225,185,163,225,185,165,225,185,167,225,185,169,225,185,171,225,185,173,225,185,175,225,185,177,225,185,179,225,185,181,225,185,183,225,185,185,225,185,187,225,185,189,225,185,191,225,186,129,225,186,131,225,186,133,225,186,135,225,186,137,225,186,139,225,186,141,225,186,143,225,186,145,225,186,147,225,186,149,45,225,186,157,225,186,159,225,186,161,225,186,163,225,186,165,225,186,167,225,186,169,225,186,171,225,186,173,225,186,175,225,186,177,225,186,179,225,186,181,225,186,183,225,186,185,225,186,187,225,186,189,225,186,191,225,187,129,225,187,131,225,187,133,225,187,135,225,187,137,225,187,139,225,187,141,225,187,143,225,187,145,225,187,147,225,187,149,225,187,151,225,187,153,225,187,155,225,187,157,225,187,159,225,187,161,225,187,163,225,187,165,225,187,167,225,187,169,225,187,171,225,187,173,225,187,175,225,187,177,225,187,179,225,187,181,225,187,183,225,187,185,225,187,187,225,187,189,225,187,191,45,225,188,135,225,188,144,45,225,188,149,225,188,160,45,225,188,167,225,188,176,45,225,188,183,225,189,128,45,225,189,133,225,189,144,45,225,189,151,225,189,160,45,225,189,167,225,189,176,45,225,189,189,225,190,128,45,225,190,135,225,190,144,45,225,190,151,225,190,160,45,225,190,167,225,190,176,45,225,190,180,225,190,182,45,225,190,183,225,190,190,225,191,130,45,225,191,132,225,191,134,45,225,191,135,225,191,144,45,225,191,147,225,191,150,45,225,191,151,225,191,160,45,225,191,167,225,191,178,45,225,191,180,225,191,182,45,225,191,183,226,132,138,226,132,142,45,226,132,143,226,132,147,226,132,175,226,132,180,226,132,185,226,132,188,45,226,132,189,226,133,134,45,226,133,137,226,133,142,226,134,132,226,176,176,45,226,177,158,226,177,161,226,177,165,45,226,177,166,226,177,168,226,177,170,226,177,172,226,177,177,226,177,179,45,226,177,180,226,177,182,45,226,177,187,226,178,129,226,178,131,226,178,133,226,178,135,226,178,137,226,178,139,226,178,141,226,178,143,226,178,145,226,178,147,226,178,149,226,178,151,226,178,153,226,178,155,226,178,157,226,178,159,226,178,161,226,178,163,226,178,165,226,178,167,226,178,169,226,178,171,226,178,173,226,178,175,226,178,177,226,178,179,226,178,181,226,178,183,226,178,185,226,178,187,226,178,189,226,178,191,226,179,129,226,179,131,226,179,133,226,179,135,226,179,137,226,179,139,226,179,141,226,179,143,226,179,145,226,179,147,226,179,149,226,179,151,226,179,153,226,179,155,226,179,157,226,179,159,226,179,161,226,179,163,45,226,179,164,226,179,172,226,179,174,226,179,179,226,180,128,45,226,180,165,226,180,167,226,180,173,234,153,129,234,153,131,234,153,133,234,153,135,234,153,137,234,153,139,234,153,141,234,153,143,234,153,145,234,153,147,234,153,149,234,153,151,234,153,153,234,153,155,234,153,157,234,153,159,234,153,161,234,153,163,234,153,165,234,153,167,234,153,169,234,153,171,234,153,173,234,154,129,234,154,131,234,154,133,234,154,135,234,154,137,234,154,139,234,154,141,234,154,143,234,154,145,234,154,147,234,154,149,234,154,151,234,156,163,234,156,165,234,156,167,234,156,169,234,156,171,234,156,173,234,156,175,45,234,156,177,234,156,179,234,156,181,234,156,183,234,156,185,234,156,187,234,156,189,234,156,191,234,157,129,234,157,131,234,157,133,234,157,135,234,157,137,234,157,139,234,157,141,234,157,143,234,157,145,234,157,147,234,157,149,234,157,151,234,157,153,234,157,155,234,157,157,234,157,159,234,157,161,234,157,163,234,157,165,234,157,167,234,157,169,234,157,171,234,157,173,234,157,175,234,157,177,45,234,157,184,234,157,186,234,157,188,234,157,191,234,158,129,234,158,131,234,158,133,234,158,135,234,158,140,234,158,142,234,158,145,234,158,147,234,158,161,234,158,163,234,158,165,234,158,167,234,158,169,234,159,186,239,172,128,45,239,172,134,239,172,147,45,239,172,151,239,189,129,45,239,189,154,93>>, false))(N)
         end
       );
pegjs_rule('Lm', Node) -> 
  pegjs( Node
       , 'Lm'
       , fun(N) -> 
           (pegjs_combinator_regexp(<<94,91,202,176,45,203,129,203,134,45,203,145,203,160,45,203,164,203,172,203,174,205,180,205,186,213,153,217,128,219,165,45,219,166,223,180,45,223,181,223,186,224,160,154,224,160,164,224,160,168,224,165,177,224,185,134,224,187,134,225,131,188,225,159,151,225,161,131,225,170,167,225,177,184,45,225,177,189,225,180,172,45,225,181,170,225,181,184,225,182,155,45,225,182,191,226,129,177,226,129,191,226,130,144,45,226,130,156,226,177,188,45,226,177,189,226,181,175,226,184,175,227,128,133,227,128,177,45,227,128,181,227,128,187,227,130,157,45,227,130,158,227,131,188,45,227,131,190,234,128,149,234,147,184,45,234,147,189,234,152,140,234,153,191,234,156,151,45,234,156,159,234,157,176,234,158,136,234,159,184,45,234,159,185,234,167,143,234,169,176,234,171,157,234,171,179,45,234,171,180,239,189,176,239,190,158,45,239,190,159,93>>, false))(N)
         end
       );
pegjs_rule('Lo', Node) -> 
  pegjs( Node
       , 'Lo'
       , fun(N) -> 
           (pegjs_combinator_regexp(<<94,91,194,170,194,186,198,187,199,128,45,199,131,202,148,215,144,45,215,170,215,176,45,215,178,216,160,45,216,191,217,129,45,217,138,217,174,45,217,175,217,177,45,219,147,219,149,219,174,45,219,175,219,186,45,219,188,219,191,220,144,220,146,45,220,175,221,141,45,222,165,222,177,223,138,45,223,170,224,160,128,45,224,160,149,224,161,128,45,224,161,152,224,162,160,224,162,162,45,224,162,172,224,164,132,45,224,164,185,224,164,189,224,165,144,224,165,152,45,224,165,161,224,165,178,45,224,165,183,224,165,185,45,224,165,191,224,166,133,45,224,166,140,224,166,143,45,224,166,144,224,166,147,45,224,166,168,224,166,170,45,224,166,176,224,166,178,224,166,182,45,224,166,185,224,166,189,224,167,142,224,167,156,45,224,167,157,224,167,159,45,224,167,161,224,167,176,45,224,167,177,224,168,133,45,224,168,138,224,168,143,45,224,168,144,224,168,147,45,224,168,168,224,168,170,45,224,168,176,224,168,178,45,224,168,179,224,168,181,45,224,168,182,224,168,184,45,224,168,185,224,169,153,45,224,169,156,224,169,158,224,169,178,45,224,169,180,224,170,133,45,224,170,141,224,170,143,45,224,170,145,224,170,147,45,224,170,168,224,170,170,45,224,170,176,224,170,178,45,224,170,179,224,170,181,45,224,170,185,224,170,189,224,171,144,224,171,160,45,224,171,161,224,172,133,45,224,172,140,224,172,143,45,224,172,144,224,172,147,45,224,172,168,224,172,170,45,224,172,176,224,172,178,45,224,172,179,224,172,181,45,224,172,185,224,172,189,224,173,156,45,224,173,157,224,173,159,45,224,173,161,224,173,177,224,174,131,224,174,133,45,224,174,138,224,174,142,45,224,174,144,224,174,146,45,224,174,149,224,174,153,45,224,174,154,224,174,156,224,174,158,45,224,174,159,224,174,163,45,224,174,164,224,174,168,45,224,174,170,224,174,174,45,224,174,185,224,175,144,224,176,133,45,224,176,140,224,176,142,45,224,176,144,224,176,146,45,224,176,168,224,176,170,45,224,176,179,224,176,181,45,224,176,185,224,176,189,224,177,152,45,224,177,153,224,177,160,45,224,177,161,224,178,133,45,224,178,140,224,178,142,45,224,178,144,224,178,146,45,224,178,168,224,178,170,45,224,178,179,224,178,181,45,224,178,185,224,178,189,224,179,158,224,179,160,45,224,179,161,224,179,177,45,224,179,178,224,180,133,45,224,180,140,224,180,142,45,224,180,144,224,180,146,45,224,180,186,224,180,189,224,181,142,224,181,160,45,224,181,161,224,181,186,45,224,181,191,224,182,133,45,224,182,150,224,182,154,45,224,182,177,224,182,179,45,224,182,187,224,182,189,224,183,128,45,224,183,134,224,184,129,45,224,184,176,224,184,178,45,224,184,179,224,185,128,45,224,185,133,224,186,129,45,224,186,130,224,186,132,224,186,135,45,224,186,136,224,186,138,224,186,141,224,186,148,45,224,186,151,224,186,153,45,224,186,159,224,186,161,45,224,186,163,224,186,165,224,186,167,224,186,170,45,224,186,171,224,186,173,45,224,186,176,224,186,178,45,224,186,179,224,186,189,224,187,128,45,224,187,132,224,187,156,45,224,187,159,224,188,128,224,189,128,45,224,189,135,224,189,137,45,224,189,172,224,190,136,45,224,190,140,225,128,128,45,225,128,170,225,128,191,225,129,144,45,225,129,149,225,129,154,45,225,129,157,225,129,161,225,129,165,45,225,129,166,225,129,174,45,225,129,176,225,129,181,45,225,130,129,225,130,142,225,131,144,45,225,131,186,225,131,189,45,225,137,136,225,137,138,45,225,137,141,225,137,144,45,225,137,150,225,137,152,225,137,154,45,225,137,157,225,137,160,45,225,138,136,225,138,138,45,225,138,141,225,138,144,45,225,138,176,225,138,178,45,225,138,181,225,138,184,45,225,138,190,225,139,128,225,139,130,45,225,139,133,225,139,136,45,225,139,150,225,139,152,45,225,140,144,225,140,146,45,225,140,149,225,140,152,45,225,141,154,225,142,128,45,225,142,143,225,142,160,45,225,143,180,225,144,129,45,225,153,172,225,153,175,45,225,153,191,225,154,129,45,225,154,154,225,154,160,45,225,155,170,225,156,128,45,225,156,140,225,156,142,45,225,156,145,225,156,160,45,225,156,177,225,157,128,45,225,157,145,225,157,160,45,225,157,172,225,157,174,45,225,157,176,225,158,128,45,225,158,179,225,159,156,225,160,160,45,225,161,130,225,161,132,45,225,161,183,225,162,128,45,225,162,168,225,162,170,225,162,176,45,225,163,181,225,164,128,45,225,164,156,225,165,144,45,225,165,173,225,165,176,45,225,165,180,225,166,128,45,225,166,171,225,167,129,45,225,167,135,225,168,128,45,225,168,150,225,168,160,45,225,169,148,225,172,133,45,225,172,179,225,173,133,45,225,173,139,225,174,131,45,225,174,160,225,174,174,45,225,174,175,225,174,186,45,225,175,165,225,176,128,45,225,176,163,225,177,141,45,225,177,143,225,177,154,45,225,177,183,225,179,169,45,225,179,172,225,179,174,45,225,179,177,225,179,181,45,225,179,182,226,132,181,45,226,132,184,226,180,176,45,226,181,167,226,182,128,45,226,182,150,226,182,160,45,226,182,166,226,182,168,45,226,182,174,226,182,176,45,226,182,182,226,182,184,45,226,182,190,226,183,128,45,226,183,134,226,183,136,45,226,183,142,226,183,144,45,226,183,150,226,183,152,45,226,183,158,227,128,134,227,128,188,227,129,129,45,227,130,150,227,130,159,227,130,161,45,227,131,186,227,131,191,227,132,133,45,227,132,173,227,132,177,45,227,134,142,227,134,160,45,227,134,186,227,135,176,45,227,135,191,227,144,128,45,228,182,181,228,184,128,45,233,191,140,234,128,128,45,234,128,148,234,128,150,45,234,146,140,234,147,144,45,234,147,183,234,148,128,45,234,152,139,234,152,144,45,234,152,159,234,152,170,45,234,152,171,234,153,174,234,154,160,45,234,155,165,234,159,187,45,234,160,129,234,160,131,45,234,160,133,234,160,135,45,234,160,138,234,160,140,45,234,160,162,234,161,128,45,234,161,179,234,162,130,45,234,162,179,234,163,178,45,234,163,183,234,163,187,234,164,138,45,234,164,165,234,164,176,45,234,165,134,234,165,160,45,234,165,188,234,166,132,45,234,166,178,234,168,128,45,234,168,168,234,169,128,45,234,169,130,234,169,132,45,234,169,139,234,169,160,45,234,169,175,234,169,177,45,234,169,182,234,169,186,234,170,128,45,234,170,175,234,170,177,234,170,181,45,234,170,182,234,170,185,45,234,170,189,234,171,128,234,171,130,234,171,155,45,234,171,156,234,171,160,45,234,171,170,234,171,178,234,172,129,45,234,172,134,234,172,137,45,234,172,142,234,172,145,45,234,172,150,234,172,160,45,234,172,166,234,172,168,45,234,172,174,234,175,128,45,234,175,162,234,176,128,45,237,158,163,237,158,176,45,237,159,134,237,159,139,45,237,159,187,239,164,128,45,239,169,173,239,169,176,45,239,171,153,239,172,157,239,172,159,45,239,172,168,239,172,170,45,239,172,182,239,172,184,45,239,172,188,239,172,190,239,173,128,45,239,173,129,239,173,131,45,239,173,132,239,173,134,45,239,174,177,239,175,147,45,239,180,189,239,181,144,45,239,182,143,239,182,146,45,239,183,135,239,183,176,45,239,183,187,239,185,176,45,239,185,180,239,185,182,45,239,187,188,239,189,166,45,239,189,175,239,189,177,45,239,190,157,239,190,160,45,239,190,190,239,191,130,45,239,191,135,239,191,138,45,239,191,143,239,191,146,45,239,191,151,239,191,154,45,239,191,156,93>>, false))(N)
         end
       );
pegjs_rule('Lt', Node) -> 
  pegjs( Node
       , 'Lt'
       , fun(N) -> 
           (pegjs_combinator_regexp(<<94,91,199,133,199,136,199,139,199,178,225,190,136,45,225,190,143,225,190,152,45,225,190,159,225,190,168,45,225,190,175,225,190,188,225,191,140,225,191,188,93>>, false))(N)
         end
       );
pegjs_rule('Lu', Node) -> 
  pegjs( Node
       , 'Lu'
       , fun(N) -> 
           (pegjs_combinator_regexp(<<94,91,65,45,90,195,128,45,195,150,195,152,45,195,158,196,128,196,130,196,132,196,134,196,136,196,138,196,140,196,142,196,144,196,146,196,148,196,150,196,152,196,154,196,156,196,158,196,160,196,162,196,164,196,166,196,168,196,170,196,172,196,174,196,176,196,178,196,180,196,182,196,185,196,187,196,189,196,191,197,129,197,131,197,133,197,135,197,138,197,140,197,142,197,144,197,146,197,148,197,150,197,152,197,154,197,156,197,158,197,160,197,162,197,164,197,166,197,168,197,170,197,172,197,174,197,176,197,178,197,180,197,182,197,184,45,197,185,197,187,197,189,198,129,45,198,130,198,132,198,134,45,198,135,198,137,45,198,139,198,142,45,198,145,198,147,45,198,148,198,150,45,198,152,198,156,45,198,157,198,159,45,198,160,198,162,198,164,198,166,45,198,167,198,169,198,172,198,174,45,198,175,198,177,45,198,179,198,181,198,183,45,198,184,198,188,199,132,199,135,199,138,199,141,199,143,199,145,199,147,199,149,199,151,199,153,199,155,199,158,199,160,199,162,199,164,199,166,199,168,199,170,199,172,199,174,199,177,199,180,199,182,45,199,184,199,186,199,188,199,190,200,128,200,130,200,132,200,134,200,136,200,138,200,140,200,142,200,144,200,146,200,148,200,150,200,152,200,154,200,156,200,158,200,160,200,162,200,164,200,166,200,168,200,170,200,172,200,174,200,176,200,178,200,186,45,200,187,200,189,45,200,190,201,129,201,131,45,201,134,201,136,201,138,201,140,201,142,205,176,205,178,205,182,206,134,206,136,45,206,138,206,140,206,142,45,206,143,206,145,45,206,161,206,163,45,206,171,207,143,207,146,45,207,148,207,152,207,154,207,156,207,158,207,160,207,162,207,164,207,166,207,168,207,170,207,172,207,174,207,180,207,183,207,185,45,207,186,207,189,45,208,175,209,160,209,162,209,164,209,166,209,168,209,170,209,172,209,174,209,176,209,178,209,180,209,182,209,184,209,186,209,188,209,190,210,128,210,138,210,140,210,142,210,144,210,146,210,148,210,150,210,152,210,154,210,156,210,158,210,160,210,162,210,164,210,166,210,168,210,170,210,172,210,174,210,176,210,178,210,180,210,182,210,184,210,186,210,188,210,190,211,128,45,211,129,211,131,211,133,211,135,211,137,211,139,211,141,211,144,211,146,211,148,211,150,211,152,211,154,211,156,211,158,211,160,211,162,211,164,211,166,211,168,211,170,211,172,211,174,211,176,211,178,211,180,211,182,211,184,211,186,211,188,211,190,212,128,212,130,212,132,212,134,212,136,212,138,212,140,212,142,212,144,212,146,212,148,212,150,212,152,212,154,212,156,212,158,212,160,212,162,212,164,212,166,212,177,45,213,150,225,130,160,45,225,131,133,225,131,135,225,131,141,225,184,128,225,184,130,225,184,132,225,184,134,225,184,136,225,184,138,225,184,140,225,184,142,225,184,144,225,184,146,225,184,148,225,184,150,225,184,152,225,184,154,225,184,156,225,184,158,225,184,160,225,184,162,225,184,164,225,184,166,225,184,168,225,184,170,225,184,172,225,184,174,225,184,176,225,184,178,225,184,180,225,184,182,225,184,184,225,184,186,225,184,188,225,184,190,225,185,128,225,185,130,225,185,132,225,185,134,225,185,136,225,185,138,225,185,140,225,185,142,225,185,144,225,185,146,225,185,148,225,185,150,225,185,152,225,185,154,225,185,156,225,185,158,225,185,160,225,185,162,225,185,164,225,185,166,225,185,168,225,185,170,225,185,172,225,185,174,225,185,176,225,185,178,225,185,180,225,185,182,225,185,184,225,185,186,225,185,188,225,185,190,225,186,128,225,186,130,225,186,132,225,186,134,225,186,136,225,186,138,225,186,140,225,186,142,225,186,144,225,186,146,225,186,148,225,186,158,225,186,160,225,186,162,225,186,164,225,186,166,225,186,168,225,186,170,225,186,172,225,186,174,225,186,176,225,186,178,225,186,180,225,186,182,225,186,184,225,186,186,225,186,188,225,186,190,225,187,128,225,187,130,225,187,132,225,187,134,225,187,136,225,187,138,225,187,140,225,187,142,225,187,144,225,187,146,225,187,148,225,187,150,225,187,152,225,187,154,225,187,156,225,187,158,225,187,160,225,187,162,225,187,164,225,187,166,225,187,168,225,187,170,225,187,172,225,187,174,225,187,176,225,187,178,225,187,180,225,187,182,225,187,184,225,187,186,225,187,188,225,187,190,225,188,136,45,225,188,143,225,188,152,45,225,188,157,225,188,168,45,225,188,175,225,188,184,45,225,188,191,225,189,136,45,225,189,141,225,189,153,225,189,155,225,189,157,225,189,159,225,189,168,45,225,189,175,225,190,184,45,225,190,187,225,191,136,45,225,191,139,225,191,152,45,225,191,155,225,191,168,45,225,191,172,225,191,184,45,225,191,187,226,132,130,226,132,135,226,132,139,45,226,132,141,226,132,144,45,226,132,146,226,132,149,226,132,153,45,226,132,157,226,132,164,226,132,166,226,132,168,226,132,170,45,226,132,173,226,132,176,45,226,132,179,226,132,190,45,226,132,191,226,133,133,226,134,131,226,176,128,45,226,176,174,226,177,160,226,177,162,45,226,177,164,226,177,167,226,177,169,226,177,171,226,177,173,45,226,177,176,226,177,178,226,177,181,226,177,190,45,226,178,128,226,178,130,226,178,132,226,178,134,226,178,136,226,178,138,226,178,140,226,178,142,226,178,144,226,178,146,226,178,148,226,178,150,226,178,152,226,178,154,226,178,156,226,178,158,226,178,160,226,178,162,226,178,164,226,178,166,226,178,168,226,178,170,226,178,172,226,178,174,226,178,176,226,178,178,226,178,180,226,178,182,226,178,184,226,178,186,226,178,188,226,178,190,226,179,128,226,179,130,226,179,132,226,179,134,226,179,136,226,179,138,226,179,140,226,179,142,226,179,144,226,179,146,226,179,148,226,179,150,226,179,152,226,179,154,226,179,156,226,179,158,226,179,160,226,179,162,226,179,171,226,179,173,226,179,178,234,153,128,234,153,130,234,153,132,234,153,134,234,153,136,234,153,138,234,153,140,234,153,142,234,153,144,234,153,146,234,153,148,234,153,150,234,153,152,234,153,154,234,153,156,234,153,158,234,153,160,234,153,162,234,153,164,234,153,166,234,153,168,234,153,170,234,153,172,234,154,128,234,154,130,234,154,132,234,154,134,234,154,136,234,154,138,234,154,140,234,154,142,234,154,144,234,154,146,234,154,148,234,154,150,234,156,162,234,156,164,234,156,166,234,156,168,234,156,170,234,156,172,234,156,174,234,156,178,234,156,180,234,156,182,234,156,184,234,156,186,234,156,188,234,156,190,234,157,128,234,157,130,234,157,132,234,157,134,234,157,136,234,157,138,234,157,140,234,157,142,234,157,144,234,157,146,234,157,148,234,157,150,234,157,152,234,157,154,234,157,156,234,157,158,234,157,160,234,157,162,234,157,164,234,157,166,234,157,168,234,157,170,234,157,172,234,157,174,234,157,185,234,157,187,234,157,189,45,234,157,190,234,158,128,234,158,130,234,158,132,234,158,134,234,158,139,234,158,141,234,158,144,234,158,146,234,158,160,234,158,162,234,158,164,234,158,166,234,158,168,234,158,170,239,188,161,45,239,188,186,93>>, false))(N)
         end
       );
pegjs_rule('Mc', Node) -> 
  pegjs( Node
       , 'Mc'
       , fun(N) -> 
           (pegjs_combinator_regexp(<<94,91,224,164,131,224,164,187,224,164,190,45,224,165,128,224,165,137,45,224,165,140,224,165,142,45,224,165,143,224,166,130,45,224,166,131,224,166,190,45,224,167,128,224,167,135,45,224,167,136,224,167,139,45,224,167,140,224,167,151,224,168,131,224,168,190,45,224,169,128,224,170,131,224,170,190,45,224,171,128,224,171,137,224,171,139,45,224,171,140,224,172,130,45,224,172,131,224,172,190,224,173,128,224,173,135,45,224,173,136,224,173,139,45,224,173,140,224,173,151,224,174,190,45,224,174,191,224,175,129,45,224,175,130,224,175,134,45,224,175,136,224,175,138,45,224,175,140,224,175,151,224,176,129,45,224,176,131,224,177,129,45,224,177,132,224,178,130,45,224,178,131,224,178,190,224,179,128,45,224,179,132,224,179,135,45,224,179,136,224,179,138,45,224,179,139,224,179,149,45,224,179,150,224,180,130,45,224,180,131,224,180,190,45,224,181,128,224,181,134,45,224,181,136,224,181,138,45,224,181,140,224,181,151,224,182,130,45,224,182,131,224,183,143,45,224,183,145,224,183,152,45,224,183,159,224,183,178,45,224,183,179,224,188,190,45,224,188,191,224,189,191,225,128,171,45,225,128,172,225,128,177,225,128,184,225,128,187,45,225,128,188,225,129,150,45,225,129,151,225,129,162,45,225,129,164,225,129,167,45,225,129,173,225,130,131,45,225,130,132,225,130,135,45,225,130,140,225,130,143,225,130,154,45,225,130,156,225,158,182,225,158,190,45,225,159,133,225,159,135,45,225,159,136,225,164,163,45,225,164,166,225,164,169,45,225,164,171,225,164,176,45,225,164,177,225,164,179,45,225,164,184,225,166,176,45,225,167,128,225,167,136,45,225,167,137,225,168,153,45,225,168,154,225,169,149,225,169,151,225,169,161,225,169,163,45,225,169,164,225,169,173,45,225,169,178,225,172,132,225,172,181,225,172,187,225,172,189,45,225,173,129,225,173,131,45,225,173,132,225,174,130,225,174,161,225,174,166,45,225,174,167,225,174,170,225,174,172,45,225,174,173,225,175,167,225,175,170,45,225,175,172,225,175,174,225,175,178,45,225,175,179,225,176,164,45,225,176,171,225,176,180,45,225,176,181,225,179,161,225,179,178,45,225,179,179,227,128,174,45,227,128,175,234,160,163,45,234,160,164,234,160,167,234,162,128,45,234,162,129,234,162,180,45,234,163,131,234,165,146,45,234,165,147,234,166,131,234,166,180,45,234,166,181,234,166,186,45,234,166,187,234,166,189,45,234,167,128,234,168,175,45,234,168,176,234,168,179,45,234,168,180,234,169,141,234,169,187,234,171,171,234,171,174,45,234,171,175,234,171,181,234,175,163,45,234,175,164,234,175,166,45,234,175,167,234,175,169,45,234,175,170,234,175,172,93>>, false))(N)
         end
       );
pegjs_rule('Mn', Node) -> 
  pegjs( Node
       , 'Mn'
       , fun(N) -> 
           (pegjs_combinator_regexp(<<94,91,204,128,45,205,175,210,131,45,210,135,214,145,45,214,189,214,191,215,129,45,215,130,215,132,45,215,133,215,135,216,144,45,216,154,217,139,45,217,159,217,176,219,150,45,219,156,219,159,45,219,164,219,167,45,219,168,219,170,45,219,173,220,145,220,176,45,221,138,222,166,45,222,176,223,171,45,223,179,224,160,150,45,224,160,153,224,160,155,45,224,160,163,224,160,165,45,224,160,167,224,160,169,45,224,160,173,224,161,153,45,224,161,155,224,163,164,45,224,163,190,224,164,128,45,224,164,130,224,164,186,224,164,188,224,165,129,45,224,165,136,224,165,141,224,165,145,45,224,165,151,224,165,162,45,224,165,163,224,166,129,224,166,188,224,167,129,45,224,167,132,224,167,141,224,167,162,45,224,167,163,224,168,129,45,224,168,130,224,168,188,224,169,129,45,224,169,130,224,169,135,45,224,169,136,224,169,139,45,224,169,141,224,169,145,224,169,176,45,224,169,177,224,169,181,224,170,129,45,224,170,130,224,170,188,224,171,129,45,224,171,133,224,171,135,45,224,171,136,224,171,141,224,171,162,45,224,171,163,224,172,129,224,172,188,224,172,191,224,173,129,45,224,173,132,224,173,141,224,173,150,224,173,162,45,224,173,163,224,174,130,224,175,128,224,175,141,224,176,190,45,224,177,128,224,177,134,45,224,177,136,224,177,138,45,224,177,141,224,177,149,45,224,177,150,224,177,162,45,224,177,163,224,178,188,224,178,191,224,179,134,224,179,140,45,224,179,141,224,179,162,45,224,179,163,224,181,129,45,224,181,132,224,181,141,224,181,162,45,224,181,163,224,183,138,224,183,146,45,224,183,148,224,183,150,224,184,177,224,184,180,45,224,184,186,224,185,135,45,224,185,142,224,186,177,224,186,180,45,224,186,185,224,186,187,45,224,186,188,224,187,136,45,224,187,141,224,188,152,45,224,188,153,224,188,181,224,188,183,224,188,185,224,189,177,45,224,189,190,224,190,128,45,224,190,132,224,190,134,45,224,190,135,224,190,141,45,224,190,151,224,190,153,45,224,190,188,224,191,134,225,128,173,45,225,128,176,225,128,178,45,225,128,183,225,128,185,45,225,128,186,225,128,189,45,225,128,190,225,129,152,45,225,129,153,225,129,158,45,225,129,160,225,129,177,45,225,129,180,225,130,130,225,130,133,45,225,130,134,225,130,141,225,130,157,225,141,157,45,225,141,159,225,156,146,45,225,156,148,225,156,178,45,225,156,180,225,157,146,45,225,157,147,225,157,178,45,225,157,179,225,158,180,45,225,158,181,225,158,183,45,225,158,189,225,159,134,225,159,137,45,225,159,147,225,159,157,225,160,139,45,225,160,141,225,162,169,225,164,160,45,225,164,162,225,164,167,45,225,164,168,225,164,178,225,164,185,45,225,164,187,225,168,151,45,225,168,152,225,168,155,225,169,150,225,169,152,45,225,169,158,225,169,160,225,169,162,225,169,165,45,225,169,172,225,169,179,45,225,169,188,225,169,191,225,172,128,45,225,172,131,225,172,180,225,172,182,45,225,172,186,225,172,188,225,173,130,225,173,171,45,225,173,179,225,174,128,45,225,174,129,225,174,162,45,225,174,165,225,174,168,45,225,174,169,225,174,171,225,175,166,225,175,168,45,225,175,169,225,175,173,225,175,175,45,225,175,177,225,176,172,45,225,176,179,225,176,182,45,225,176,183,225,179,144,45,225,179,146,225,179,148,45,225,179,160,225,179,162,45,225,179,168,225,179,173,225,179,180,225,183,128,45,225,183,166,225,183,188,45,225,183,191,226,131,144,45,226,131,156,226,131,161,226,131,165,45,226,131,176,226,179,175,45,226,179,177,226,181,191,226,183,160,45,226,183,191,227,128,170,45,227,128,173,227,130,153,45,227,130,154,234,153,175,234,153,180,45,234,153,189,234,154,159,234,155,176,45,234,155,177,234,160,130,234,160,134,234,160,139,234,160,165,45,234,160,166,234,163,132,234,163,160,45,234,163,177,234,164,166,45,234,164,173,234,165,135,45,234,165,145,234,166,128,45,234,166,130,234,166,179,234,166,182,45,234,166,185,234,166,188,234,168,169,45,234,168,174,234,168,177,45,234,168,178,234,168,181,45,234,168,182,234,169,131,234,169,140,234,170,176,234,170,178,45,234,170,180,234,170,183,45,234,170,184,234,170,190,45,234,170,191,234,171,129,234,171,172,45,234,171,173,234,171,182,234,175,165,234,175,168,234,175,173,239,172,158,239,184,128,45,239,184,143,239,184,160,45,239,184,166,93>>, false))(N)
         end
       );
pegjs_rule('Nd', Node) -> 
  pegjs( Node
       , 'Nd'
       , fun(N) -> 
           (pegjs_combinator_regexp(<<94,91,48,45,57,217,160,45,217,169,219,176,45,219,185,223,128,45,223,137,224,165,166,45,224,165,175,224,167,166,45,224,167,175,224,169,166,45,224,169,175,224,171,166,45,224,171,175,224,173,166,45,224,173,175,224,175,166,45,224,175,175,224,177,166,45,224,177,175,224,179,166,45,224,179,175,224,181,166,45,224,181,175,224,185,144,45,224,185,153,224,187,144,45,224,187,153,224,188,160,45,224,188,169,225,129,128,45,225,129,137,225,130,144,45,225,130,153,225,159,160,45,225,159,169,225,160,144,45,225,160,153,225,165,134,45,225,165,143,225,167,144,45,225,167,153,225,170,128,45,225,170,137,225,170,144,45,225,170,153,225,173,144,45,225,173,153,225,174,176,45,225,174,185,225,177,128,45,225,177,137,225,177,144,45,225,177,153,234,152,160,45,234,152,169,234,163,144,45,234,163,153,234,164,128,45,234,164,137,234,167,144,45,234,167,153,234,169,144,45,234,169,153,234,175,176,45,234,175,185,239,188,144,45,239,188,153,93>>, false))(N)
         end
       );
pegjs_rule('Nl', Node) -> 
  pegjs( Node
       , 'Nl'
       , fun(N) -> 
           (pegjs_combinator_regexp(<<94,91,225,155,174,45,225,155,176,226,133,160,45,226,134,130,226,134,133,45,226,134,136,227,128,135,227,128,161,45,227,128,169,227,128,184,45,227,128,186,234,155,166,45,234,155,175,93>>, false))(N)
         end
       );
pegjs_rule('Pc', Node) -> 
  pegjs( Node
       , 'Pc'
       , fun(N) -> 
           (pegjs_combinator_regexp(<<94,91,95,226,128,191,45,226,129,128,226,129,148,239,184,179,45,239,184,180,239,185,141,45,239,185,143,239,188,191,93>>, false))(N)
         end
       );
pegjs_rule('Zs', Node) -> 
  pegjs( Node
       , 'Zs'
       , fun(N) -> 
           (pegjs_combinator_regexp(<<94,91,32,194,160,225,154,128,226,128,128,45,226,128,138,226,128,175,226,129,159,227,128,128,93>>, false))(N)
         end
       );
pegjs_rule('BreakToken', Node) -> 
  pegjs( Node
       , 'BreakToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<98,114,101,97,107>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('CaseToken', Node) -> 
  pegjs( Node
       , 'CaseToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<99,97,115,101>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('CatchToken', Node) -> 
  pegjs( Node
       , 'CatchToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<99,97,116,99,104>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('ClassToken', Node) -> 
  pegjs( Node
       , 'ClassToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<99,108,97,115,115>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('ConstToken', Node) -> 
  pegjs( Node
       , 'ConstToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<99,111,110,115,116>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('ContinueToken', Node) -> 
  pegjs( Node
       , 'ContinueToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<99,111,110,116,105,110,117,101>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('DebuggerToken', Node) -> 
  pegjs( Node
       , 'DebuggerToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<100,101,98,117,103,103,101,114>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('DefaultToken', Node) -> 
  pegjs( Node
       , 'DefaultToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<100,101,102,97,117,108,116>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('DeleteToken', Node) -> 
  pegjs( Node
       , 'DeleteToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<100,101,108,101,116,101>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('DoToken', Node) -> 
  pegjs( Node
       , 'DoToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<100,111>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('ElseToken', Node) -> 
  pegjs( Node
       , 'ElseToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<101,108,115,101>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('EnumToken', Node) -> 
  pegjs( Node
       , 'EnumToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<101,110,117,109>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('ExportToken', Node) -> 
  pegjs( Node
       , 'ExportToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<101,120,112,111,114,116>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('ExtendsToken', Node) -> 
  pegjs( Node
       , 'ExtendsToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<101,120,116,101,110,100,115>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('FalseToken', Node) -> 
  pegjs( Node
       , 'FalseToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<102,97,108,115,101>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('FinallyToken', Node) -> 
  pegjs( Node
       , 'FinallyToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<102,105,110,97,108,108,121>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('ForToken', Node) -> 
  pegjs( Node
       , 'ForToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<102,111,114>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('FunctionToken', Node) -> 
  pegjs( Node
       , 'FunctionToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<102,117,110,99,116,105,111,110>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('IfToken', Node) -> 
  pegjs( Node
       , 'IfToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<105,102>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('ImportToken', Node) -> 
  pegjs( Node
       , 'ImportToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<105,109,112,111,114,116>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('InstanceofToken', Node) -> 
  pegjs( Node
       , 'InstanceofToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<105,110,115,116,97,110,99,101,111,102>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('InToken', Node) -> 
  pegjs( Node
       , 'InToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<105,110>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('NewToken', Node) -> 
  pegjs( Node
       , 'NewToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<110,101,119>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('NullToken', Node) -> 
  pegjs( Node
       , 'NullToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<110,117,108,108>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('ReturnToken', Node) -> 
  pegjs( Node
       , 'ReturnToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<114,101,116,117,114,110>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('SuperToken', Node) -> 
  pegjs( Node
       , 'SuperToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<115,117,112,101,114>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('SwitchToken', Node) -> 
  pegjs( Node
       , 'SwitchToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<115,119,105,116,99,104>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('ThisToken', Node) -> 
  pegjs( Node
       , 'ThisToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<116,104,105,115>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('ThrowToken', Node) -> 
  pegjs( Node
       , 'ThrowToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<116,104,114,111,119>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('TrueToken', Node) -> 
  pegjs( Node
       , 'TrueToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<116,114,117,101>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('TryToken', Node) -> 
  pegjs( Node
       , 'TryToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<116,114,121>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('TypeofToken', Node) -> 
  pegjs( Node
       , 'TypeofToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<116,121,112,101,111,102>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('VarToken', Node) -> 
  pegjs( Node
       , 'VarToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<118,97,114>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('VoidToken', Node) -> 
  pegjs( Node
       , 'VoidToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<118,111,105,100>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('WhileToken', Node) -> 
  pegjs( Node
       , 'WhileToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<119,104,105,108,101>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('WithToken', Node) -> 
  pegjs( Node
       , 'WithToken'
       , fun(N) -> 
           (pegjs_combinator_sequence([pegjs_combinator_literal(<<119,105,116,104>>, false), pegjs_combinator_prefixed('simple_not', pegjs_combinator_rule_ref('IdentifierPart'))]))(N)
         end
       );
pegjs_rule('__', Node) -> 
  pegjs( Node
       , '__'
       , fun(N) -> 
           (pegjs_combinator_suffixed('zero_or_more', pegjs_combinator_choice([pegjs_combinator_rule_ref('WhiteSpace'), pegjs_combinator_rule_ref('LineTerminatorSequence'), pegjs_combinator_rule_ref('Comment')])))(N)
         end
       );
pegjs_rule('_', Node) -> 
  pegjs( Node
       , '_'
       , fun(N) -> 
           (pegjs_combinator_suffixed('zero_or_more', pegjs_combinator_choice([pegjs_combinator_rule_ref('WhiteSpace'), pegjs_combinator_rule_ref('MultiLineCommentNoLineTerminator')])))(N)
         end
       );
pegjs_rule('EOS', Node) -> 
  pegjs( Node
       , 'EOS'
       , fun(N) -> 
           (pegjs_combinator_choice([pegjs_combinator_sequence([pegjs_combinator_rule_ref('__'), pegjs_combinator_literal(<<59>>, false)]), pegjs_combinator_sequence([pegjs_combinator_rule_ref('_'), pegjs_combinator_suffixed('optional', pegjs_combinator_rule_ref('SingleLineComment')), pegjs_combinator_rule_ref('LineTerminatorSequence')]), pegjs_combinator_sequence([pegjs_combinator_rule_ref('__'), pegjs_combinator_rule_ref('EOF')])]))(N)
         end
       );
pegjs_rule('EOF', Node) -> 
  pegjs( Node
       , 'EOF'
       , fun(N) -> 
           (pegjs_combinator_prefixed('simple_not', pegjs_combinator_any()))(N)
         end
       ).

pegjs_combinator_all([], Node0, Accum, TransformFun) ->
  Result = lists:reverse(Accum),
  case TransformFun(Node0#pegjs_node{input = Result}) of
    {error, _} = E -> E;
    _ -> Node0#pegjs_node{match = Result}
  end;
pegjs_combinator_all([P|Parsers], Node0, Accum, TransformFun) ->
  case P(Node0) of
    {error, _} = Failure -> Failure;
    #pegjs_node{match = Result} = Node ->
      pegjs_combinator_all(Parsers, Node, [Result|Accum], TransformFun)
  end.

pegjs_combinator_attempt([], _, Failure) -> Failure;
pegjs_combinator_attempt([P|Parsers], Node, FirstFailure) ->
  case P(Node) of
    {error, _} = Failure ->
      case FirstFailure of
        none -> pegjs_combinator_attempt(Parsers, Node, Failure);
        _    -> pegjs_combinator_attempt(Parsers, Node, FirstFailure)
      end;
    Result -> Result
  end.

pegjs_combinator_prefixed(simple_not, P) ->
  fun(#pegjs_node{index = Index} = Node0)->
      case P(Node0) of
        {error,_} ->
          Node0#pegjs_node{match = []};
        Node      ->
          {error, {expected, {no_match, Node#pegjs_node.match}, Index}}
      end
  end;
pegjs_combinator_prefixed(simple_and, P) ->
  fun(Node) ->
    case P(Node) of
      {error, _} = Failure -> Failure;
      _ -> Node#pegjs_node{match = []}
    end
  end;

pegjs_combinator_prefixed(semantic_and, CustomFun) ->
  fun(Node0) ->
    case CustomFun(Node0) of
      {error, _} = Failure -> Failure;
      false                -> {error, {condition_failed, index(Node0)}};
      true                 -> Node0#pegjs_node{match = []};
      Node                 -> Node#pegjs_node{match = []}
    end
  end;

pegjs_combinator_prefixed(semantic_not, CustomFun) ->
  fun(Node0) ->
    case CustomFun(Node0) of
      {error, _}    -> Node0#pegjs_node{match = []};
      false         -> Node0#pegjs_node{match = []};
      true          -> {error, {condition_failed, index(Node0)}};
      #pegjs_node{} -> {error, {condition_failed, index(Node0)}}
    end
  end.

pegjs_combinator_regexp(Regexp, IsIgnoreCase) ->
    Options0 = [unicode, dotall, anchored],
    Options = case IsIgnoreCase of
                true  -> [caseless | Options0];
                false -> Options0
              end,
    {ok, RE} = re:compile(Regexp, Options),
    fun(#pegjs_node{input = Input, index = Index} = Node) ->
        case re:run(Input, RE) of
            {match, [{0, Length}|_]} ->
                {Head, Tail} = erlang:split_binary(Input, Length),
                Node#pegjs_node{ input = Tail
                               , index = p_advance_index(Head, Index)
                               , match = Head
                               };
            _ ->
              Modifier = case IsIgnoreCase of
                             true  -> case_insensitive;
                             false -> case_sensitive
                           end,
              {error, { expected
                      , {regexp, binary_to_list(Regexp), Modifier}
                      , Index}}
        end
    end.

pegjs_util_convert_to_iolist(MaybeIoList) ->
  pegjs_util_convert_to_iolist(MaybeIoList, []).

pegjs_util_convert_to_iolist([], Acc) ->
  lists:reverse(Acc);
pegjs_util_convert_to_iolist([H | T], Acc) ->
  pegjs_util_convert_to_iolist(T, [pegjs_util_convert_to_iolist(H) | Acc]);
pegjs_util_convert_to_iolist({Label, Value}, Acc) when is_binary(Label) ->
  [pegjs_util_convert_to_iolist(Value) | Acc];
pegjs_util_convert_to_iolist(Other, Acc) ->
  pegjs_util_convert_to_iolist([], [Other | Acc]).

pegjs_combinator_scan(_, #pegjs_node{input = []} = Node, Accum) ->
  Node#pegjs_node{match = lists:reverse(Accum)};
pegjs_combinator_scan(P, Node0, Accum) ->
  case P(Node0) of
    {error,_N} -> Node0#pegjs_node{match = lists:reverse(Accum)};
    #pegjs_node{match = Result} = Node ->
      pegjs_combinator_scan(P, Node, [Result | Accum])
  end.

pegjs_combinator_suffixed(zero_or_more, P) ->
  fun(Node) ->
      pegjs_combinator_scan(P, Node, [])
  end;
pegjs_combinator_suffixed(one_or_more, P) ->
  fun(#pegjs_node{index = Index} = Node)->
      Result = pegjs_combinator_scan(P, Node, []),
      case Result of
        #pegjs_node{match = [_|_]} ->
          Result;
        _ ->
          {error, {_, Failure, _}} = P(Node),
          {error, {expected, {at_least_one, Failure}, Index}}
      end
  end;
pegjs_combinator_suffixed(optional, P) ->
  fun(Node) ->
      case P(Node) of
        {error,_} -> Node#pegjs_node{match = []};
        Success -> Success
      end
  end.

pegjs_combinator_action(CustomFun, P) ->
  fun(Node0) ->
    case P(Node0) of
      {error, _} = E -> E;
      Node           ->
        case CustomFun(Node) of
          #pegjs_node{} = Match -> Match;
          {error, _} = E        -> E;
          Other                 -> Node#pegjs_node{match = Other}        end
    end
  end.

pegjs_combinator_any() ->
  fun(#pegjs_node{input = Input, index = Index} = Node) when is_binary(Input) ->
    case Input of
      <<>> ->
        {error, {expected, any_character, Index}};
      <<C/utf8, Rest/binary>> ->
        Node#pegjs_node{ input = Rest
                       , index = p_advance_index(<<C/utf8>>, Index)
                       , match = <<C/utf8>>
                       }
    end
  end.

pegjs_combinator_choice(Parsers) ->
  fun(Node) ->
      pegjs_combinator_attempt(Parsers, Node, none)
  end.

pegjs_combinator_labeled(Tag, P) ->
  fun(Node0) ->
      case P(Node0) of
        {error,_} = Failure ->
           Failure;
        #pegjs_node{match = Match} = Node ->
          case Tag of
            undefined -> Node;
            _         -> Node#pegjs_node{match = {Tag, Match}}
          end
      end
  end.

pegjs_combinator_literal(String, IsCaseInsensitive) ->
    Length = erlang:byte_size(String),
    fun(#pegjs_node{input = Input, index = Index} = Node) ->
      case Input of
        <<String:Length/binary, Rest/binary>> ->
          Node#pegjs_node{ input = Rest
                         , index = p_advance_index(String, Index)
                         , match = String
                         };
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
                  Node#pegjs_node{ input = Rest
                                 , index = p_advance_index(Result, Index)
                                 , match = Result
                                 };
                {match, _} ->
                  Node#pegjs_node{ input = <<>>
                                 , index = p_advance_index(String, Index)
                                 , match = Input
                                 };
                nomatch    ->
                  {error, {match_failed, {{String, Modifier}, Input}, Index}}
              end
          end
      end
    end.

pegjs_combinator_named(P) ->
  fun(Node) -> P(Node) end.

pegjs_combinator_rule_ref(Rule) ->
    fun(Node) ->
        pegjs_rule(Rule, Node)
    end.

pegjs_combinator_sequence(P) ->
  fun(Node) ->
      pegjs_combinator_all(P, Node, [], fun(N) -> N end)
  end.

pegjs_combinator_text(P) ->
  fun(Node0) ->
      Result = P(Node0),
      case Result of
        {error, _} = E -> E;
        #pegjs_node{match = MaybeIoList} = Node ->
          IoList = pegjs_util_convert_to_iolist(MaybeIoList),
          Node#pegjs_node{match = iolist_to_binary(IoList)}
      end
  end.

-spec pegjs_custom_fun_382_88(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_382_88(Node) -> 
 iolist_to_binary(value(<<"chars">>, Node)) .

-spec pegjs_custom_fun_93_6(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_93_6(Node) -> 

      Initializer = value(<<"initializer">>, Node),
      Rules       = value(<<"rules">>, Node),
      #entry{ type        = <<"grammar">>
            , initializer = case Initializer of [I, _] -> I; [] -> [] end
            , rules       = entries(Rules)
            , index       = index(Node)
            }
    .

-spec pegjs_custom_fun_230_6(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_230_6(Node) -> 

      #entry{ type  = <<"rule_ref">>
            , name  = value(<<"name">>, Node)
            , index = index(Node)
            }
    .

-spec pegjs_custom_fun_481_37(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_481_37(_Node) -> 
 #entry{type = <<"any">>} .

-spec pegjs_custom_fun_448_24(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_448_24(_Node) -> 
 <<"\b">>   .

-spec pegjs_custom_fun_427_66(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_427_66(Node) -> 
 text(Node) .

-spec pegjs_custom_fun_96_126(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_96_126(Node) -> 
 Code = value(<<"code">>, Node), #entry{type = <<"initializer">>, code = Code, index = index(Node)} .

-spec pegjs_custom_fun_484_50(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_484_50(Node) -> 
 value(<<"code">>, Node) .

-spec pegjs_custom_fun_472_6(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_472_6(Node) -> 

      hexstr_to_bin(value(<<"digits">>, Node))
    .

-spec pegjs_custom_fun_450_24(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_450_24(_Node) -> 
 <<"\n">>   .

-spec pegjs_custom_fun_424_6(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_424_6(Node) -> 

      Begin = value(<<"begin">>, Node),
      End   = value(<<"end">>, Node),
      case Begin > End of
        true ->
          error({invalid_character_range, {Begin, End, index(Node)}});
        false ->
          [Begin, End]
      end
    .

-spec pegjs_custom_fun_222_74(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_222_74(Node) -> 
 value(<<"expression">>, Node) .

-spec pegjs_custom_fun_170_6(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_170_6(Node) -> 

      First = value(<<"first">>, Node),
      Rest  = value(<<"rest">>, Node),
      case Rest of
        [] ->
          entries(First);
        _ ->
          #entry{ type     = <<"sequence">>
                , elements = entries([First | Rest])
                , index    = index(Node)
                }
      end
    .

-spec pegjs_custom_fun_452_24(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_452_24(_Node) -> 
 <<"\t">>   .

-spec pegjs_custom_fun_390_66(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_390_66(Node) -> 
 text(Node) .

-spec pegjs_custom_fun_378_6(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_378_6(Node) -> 

      #entry{ type        = <<"literal">>
            , value       = value(<<"value">>, Node)
            , ignore_case = value(<<"ignoreCase">>, Node) /= []
            , index       = index(Node)
            }
    .

-spec pegjs_custom_fun_467_6(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_467_6(Node) -> 

      hexstr_to_bin(value(<<"digits">>, Node))
    .

-spec pegjs_custom_fun_456_71(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_456_71(Node) -> 
 text(Node) .

-spec pegjs_custom_fun_436_35(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_436_35(_Node) -> 
 <<"\0">> .

-spec pegjs_custom_fun_381_88(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_381_88(Node) -> 
 iolist_to_binary(value(<<"chars">>, Node)) .

-spec pegjs_custom_fun_412_6(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_412_6(Node) -> 

      Inverted   = value(<<"inverted">>, Node),
      IgnoreCase = value(<<"ignoreCase">>, Node),
      Parts0     = value(<<"parts">>, Node),
      Parts      = filter_empty_strings(Parts0),
      #entry{ type        = <<"class">>
            , parts       = Parts
            , inverted    = Inverted /= []
            , ignore_case = IgnoreCase /= []
            , raw_text    = text(Node)
            , index       = index(Node)
            }
    .

-spec pegjs_custom_fun_181_6(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_181_6(Node) -> 

      Label      = value(<<"label">>, Node),
      Expression = value(<<"expression">>, Node),
      #entry{ type       = <<"labeled">>
            , label      = Label
            , expression = entries(Expression)
            , index      = index(Node)
            }
    .

-spec pegjs_custom_fun_385_66(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_385_66(Node) -> 
 text(Node) .

-spec pegjs_custom_fun_449_24(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_449_24(_Node) -> 
 <<"\f">>   .

-spec pegjs_custom_fun_432_43(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_432_43(_Node) -> 
 <<"">> .

-spec pegjs_custom_fun_429_36(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_429_36(Node) -> 
 text(Node) .

-spec pegjs_custom_fun_240_6(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_240_6(Node) -> 

      Operator = value(<<"operator">>, Node),
      Code     = value(<<"code">>, Node),
      #entry{ type  = ops_to_semantic_predicate_types(Operator)
            , code  = Code
            , index = index(Node)
            }
    .

-spec pegjs_custom_fun_208_6(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_208_6(Node) -> 

      Operator   = value(<<"operator">>, Node),
      Expression = value(<<"expression">>, Node),
      #entry{ type       = ops_to_suffixed_types(Operator)
            , expression = entries(Expression)
            , index      = index(Node)
            }
    .

-spec pegjs_custom_fun_192_6(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_192_6(Node) -> 

      Operator   = value(<<"operator">>, Node),
      Expression = value(<<"expression">>, Node),
      #entry{ type       = ops_to_prefixed_types(Operator)
            , expression = entries(Expression)
            , index      = index(Node)
            }
    .

-spec pegjs_custom_fun_391_65(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_391_65(Node) -> 
 value(<<"sequence">>, Node) .

-spec pegjs_custom_fun_287_121(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_287_121(Node) -> 
 iolist_to_binary([value(<<"first">>, Node), value(<<"rest">>, Node)]) .

-spec pegjs_custom_fun_284_66(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_284_66(Node) -> 
 value(<<"name">>, Node) .

-spec pegjs_custom_fun_121_6(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_121_6(Node) -> 

      Name         = value(<<"name">>, Node),
      DisplayName  = value(<<"displayName">>, Node),
      Expression   = value(<<"expression">>, Node),
      #entry{ type = <<"rule">>
            , name = Name
            , expression = case DisplayName of
                              [String, _] ->
                                #entry{ type       = <<"named">>
                                      , name       = String
                                      , expression = entries(Expression)
                                      , index      = index(Node)
                                      };
                              [] ->
                                entries(Expression)
                           end
            , index = index(Node)
            }
    .

-spec pegjs_custom_fun_451_24(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_451_24(_Node) -> 
 <<"\r">>   .

-spec pegjs_custom_fun_155_6(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_155_6(Node) -> 

      Expression = value(<<"expression">>, Node),
      Code = value(<<"code">>, Node),
      case Code of
        [] ->
          entries(Expression);
        [_, C]  ->
          #entry{ type       = <<"action">>
                , expression = entries(Expression)
                , code       = C
                , index      = index(Node)
                }
      end
    .

-spec pegjs_custom_fun_139_6(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_139_6(Node) -> 

      First = value(<<"first">>, Node),
      Rest  = value(<<"rest">>, Node),
      case Rest of
        [] ->
          entries(First);
        _ ->
          #entry{ type         = <<"choice">>
                , alternatives = entries([First | Rest])
                , index       = index(Node)
                }
      end
    .

-spec pegjs_custom_fun_453_24(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_453_24(_Node) -> 
 <<"\v">>   .

-spec pegjs_custom_fun_428_83(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_428_83(Node) -> 
 iolist_to_binary(value(<<"sequence">>, Node)) .

-spec pegjs_custom_fun_386_65(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_386_65(Node) -> 
 value(<<"sequence">>, Node) .

-spec pegjs_custom_fun_293_72(#pegjs_node{}) -> #pegjs_node{} | {error, term()}.
pegjs_custom_fun_293_72(Node) -> 
 value(<<"sequence">>, Node) .

