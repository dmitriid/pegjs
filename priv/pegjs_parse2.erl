-module(pegjs_parse2).
-export([parse/1,file/1]).
-define(p_anything,true).
-define(p_charclass,true).
-define(p_choose,true).
-define(p_label,true).
-define(p_not,true).
-define(p_one_or_more,true).
-define(p_optional,true).
-define(p_regexp,true).
-define(p_scan,true).
-define(p_seq,true).
-define(p_string,true).
-define(p_zero_or_more,true).



-spec file(file:name()) -> any().
file(Filename) -> case file:read_file(Filename) of {ok,Bin} -> parse(Bin); Err -> Err end.

-spec parse(binary() | list()) -> any().
parse(List) when is_list(List) -> parse(list_to_binary(List));
parse(Input) when is_binary(Input) ->
  setup_memo(),
  Result = case 'Grammar'(Input,{{line,1},{column,1}}) of
             {AST, <<>>, _Index} -> AST;
             Any -> Any
           end,
  release_memo(), Result.

-spec 'Grammar'(input(), index()) -> parse_result().
'Grammar'(Input, Index) ->
  p(Input, Index, 'Grammar', fun(I,D) -> (p_seq([fun '__'/2, p_label('initializer', p_optional(p_seq([fun 'Initializer'/2, fun '__'/2]))), p_label('rules', p_one_or_more(p_seq([fun 'Rule'/2, fun '__'/2])))]))(I,D) end, fun(Node, Idx) ->transform('Grammar', Node, Idx) end).

-spec 'Initializer'(input(), index()) -> parse_result().
'Initializer'(Input, Index) ->
  p(Input, Index, 'Initializer', fun(I,D) -> (p_seq([p_label('code', fun 'CodeBlock'/2), fun 'EOS'/2]))(I,D) end, fun(Node, Idx) ->transform('Initializer', Node, Idx) end).

-spec 'Rule'(input(), index()) -> parse_result().
'Rule'(Input, Index) ->
  p(Input, Index, 'Rule', fun(I,D) -> (p_seq([p_label('name', fun 'IdentifierName'/2), fun '__'/2, p_label('displayName', p_optional(p_seq([fun 'StringLiteral'/2, fun '__'/2]))), p_string(<<"=">>), fun '__'/2, p_label('expression', fun 'Expression'/2), fun 'EOS'/2]))(I,D) end, fun(Node, Idx) ->transform('Rule', Node, Idx) end).

-spec 'Expression'(input(), index()) -> parse_result().
'Expression'(Input, Index) ->
  p(Input, Index, 'Expression', fun(I,D) -> (fun 'ChoiceExpression'/2)(I,D) end, fun(Node, Idx) ->transform('Expression', Node, Idx) end).

-spec 'ChoiceExpression'(input(), index()) -> parse_result().
'ChoiceExpression'(Input, Index) ->
  p(Input, Index, 'ChoiceExpression', fun(I,D) -> (p_seq([p_label('first', fun 'ActionExpression'/2), p_label('rest', p_zero_or_more(p_seq([fun '__'/2, p_string(<<"\/">>), fun '__'/2, fun 'ActionExpression'/2])))]))(I,D) end, fun(Node, Idx) ->transform('ChoiceExpression', Node, Idx) end).

-spec 'ActionExpression'(input(), index()) -> parse_result().
'ActionExpression'(Input, Index) ->
  p(Input, Index, 'ActionExpression', fun(I,D) -> (p_seq([p_label('expression', fun 'SequenceExpression'/2), p_label('code', p_optional(p_seq([fun '__'/2, fun 'CodeBlock'/2])))]))(I,D) end, fun(Node, Idx) ->transform('ActionExpression', Node, Idx) end).

-spec 'SequenceExpression'(input(), index()) -> parse_result().
'SequenceExpression'(Input, Index) ->
  p(Input, Index, 'SequenceExpression', fun(I,D) -> (p_seq([p_label('first', fun 'LabeledExpression'/2), p_label('rest', p_zero_or_more(p_seq([fun '__'/2, fun 'LabeledExpression'/2])))]))(I,D) end, fun(Node, Idx) ->transform('SequenceExpression', Node, Idx) end).

-spec 'LabeledExpression'(input(), index()) -> parse_result().
'LabeledExpression'(Input, Index) ->
  p(Input, Index, 'LabeledExpression', fun(I,D) -> (p_seq([p_label('label', fun 'Identifier'/2), fun '__'/2, p_string(<<":">>), fun '__'/2, p_label('expression', fun 'PrefixedExpression'/2)]))(I,D) end, fun(Node, Idx) ->transform('LabeledExpression', Node, Idx) end).

-spec 'PrefixedExpression'(input(), index()) -> parse_result().
'PrefixedExpression'(Input, Index) ->
  p(Input, Index, 'PrefixedExpression', fun(I,D) -> (p_seq([p_label('operator', fun 'PrefixedOperator'/2), fun '__'/2, p_label('expression', fun 'SuffixedExpression'/2)]))(I,D) end, fun(Node, Idx) ->transform('PrefixedExpression', Node, Idx) end).

-spec 'PrefixedOperator'(input(), index()) -> parse_result().
'PrefixedOperator'(Input, Index) ->
  p(Input, Index, 'PrefixedOperator', fun(I,D) -> (p_choose([p_string(<<"$">>), p_string(<<"&">>), p_string(<<"!">>)]))(I,D) end, fun(Node, Idx) ->transform('PrefixedOperator', Node, Idx) end).

-spec 'SuffixedExpression'(input(), index()) -> parse_result().
'SuffixedExpression'(Input, Index) ->
  p(Input, Index, 'SuffixedExpression', fun(I,D) -> (p_seq([p_label('expression', fun 'PrimaryExpression'/2), fun '__'/2, p_label('operator', fun 'SuffixedOperator'/2)]))(I,D) end, fun(Node, Idx) ->transform('SuffixedExpression', Node, Idx) end).

-spec 'SuffixedOperator'(input(), index()) -> parse_result().
'SuffixedOperator'(Input, Index) ->
  p(Input, Index, 'SuffixedOperator', fun(I,D) -> (p_choose([p_string(<<"?">>), p_string(<<"*">>), p_string(<<"+">>)]))(I,D) end, fun(Node, Idx) ->transform('SuffixedOperator', Node, Idx) end).

-spec 'PrimaryExpression'(input(), index()) -> parse_result().
'PrimaryExpression'(Input, Index) ->
  p(Input, Index, 'PrimaryExpression', fun(I,D) -> (p_choose([fun 'LiteralMatcher'/2, fun 'CharacterClassMatcher'/2, fun 'AnyMatcher'/2, fun 'RuleReferenceExpression'/2, fun 'SemanticPredicateExpression'/2, p_seq([p_string(<<"(">>), fun '__'/2, p_label('expression', fun 'Expression'/2), fun '__'/2, p_string(<<")">>)])]))(I,D) end, fun(Node, Idx) ->transform('PrimaryExpression', Node, Idx) end).

-spec 'RuleReferenceExpression'(input(), index()) -> parse_result().
'RuleReferenceExpression'(Input, Index) ->
  p(Input, Index, 'RuleReferenceExpression', fun(I,D) -> (p_seq([p_label('name', fun 'IdentifierName'/2), p_not(p_seq([fun '__'/2, p_optional(p_seq([fun 'StringLiteral'/2, fun '__'/2])), p_string(<<"=">>)]))]))(I,D) end, fun(Node, Idx) ->transform('RuleReferenceExpression', Node, Idx) end).

-spec 'SemanticPredicateExpression'(input(), index()) -> parse_result().
'SemanticPredicateExpression'(Input, Index) ->
  p(Input, Index, 'SemanticPredicateExpression', fun(I,D) -> (p_seq([p_label('operator', fun 'SemanticPredicateOperator'/2), fun '__'/2, p_label('code', fun 'CodeBlock'/2)]))(I,D) end, fun(Node, Idx) ->transform('SemanticPredicateExpression', Node, Idx) end).

-spec 'SemanticPredicateOperator'(input(), index()) -> parse_result().
'SemanticPredicateOperator'(Input, Index) ->
  p(Input, Index, 'SemanticPredicateOperator', fun(I,D) -> (p_choose([p_string(<<"&">>), p_string(<<"!">>)]))(I,D) end, fun(Node, Idx) ->transform('SemanticPredicateOperator', Node, Idx) end).

-spec 'SourceCharacter'(input(), index()) -> parse_result().
'SourceCharacter'(Input, Index) ->
  p(Input, Index, 'SourceCharacter', fun(I,D) -> (p_anything())(I,D) end, fun(Node, Idx) ->transform('SourceCharacter', Node, Idx) end).

-spec 'WhiteSpace'(input(), index()) -> parse_result().
'WhiteSpace'(Input, Index) ->
  p(Input, Index, 'WhiteSpace', fun(I,D) -> (p_choose([p_string(<<"\t">>), p_string(<<"\v">>), p_string(<<"\f">>), p_string(<<"\s">>), p_string(<<"\u00A0">>), p_string(<<"\uFEFF">>), fun 'Zs'/2]))(I,D) end, fun(Node, Idx) ->transform('WhiteSpace', Node, Idx) end).

-spec 'LineTerminator'(input(), index()) -> parse_result().
'LineTerminator'(Input, Index) ->
  p(Input, Index, 'LineTerminator', fun(I,D) -> (p_charclass(<<"[\n\r\u2028\u2029]">>))(I,D) end, fun(Node, Idx) ->transform('LineTerminator', Node, Idx) end).

-spec 'LineTerminatorSequence'(input(), index()) -> parse_result().
'LineTerminatorSequence'(Input, Index) ->
  p(Input, Index, 'LineTerminatorSequence', fun(I,D) -> (p_choose([p_string(<<"\n">>), p_string(<<"\r\n">>), p_string(<<"\r">>), p_string(<<"\u2028">>), p_string(<<"\u2029">>)]))(I,D) end, fun(Node, Idx) ->transform('LineTerminatorSequence', Node, Idx) end).

-spec 'Comment'(input(), index()) -> parse_result().
'Comment'(Input, Index) ->
  p(Input, Index, 'Comment', fun(I,D) -> (p_choose([fun 'MultiLineComment'/2, fun 'SingleLineComment'/2]))(I,D) end, fun(Node, Idx) ->transform('Comment', Node, Idx) end).

-spec 'MultiLineComment'(input(), index()) -> parse_result().
'MultiLineComment'(Input, Index) ->
  p(Input, Index, 'MultiLineComment', fun(I,D) -> (p_seq([p_string(<<"\/*">>), p_zero_or_more(p_seq([p_not(p_string(<<"*\/">>)), fun 'SourceCharacter'/2])), p_string(<<"*\/">>)]))(I,D) end, fun(Node, Idx) ->transform('MultiLineComment', Node, Idx) end).

-spec 'MultiLineCommentNoLineTerminator'(input(), index()) -> parse_result().
'MultiLineCommentNoLineTerminator'(Input, Index) ->
  p(Input, Index, 'MultiLineCommentNoLineTerminator', fun(I,D) -> (p_seq([p_string(<<"\/*">>), p_zero_or_more(p_seq([p_not(p_choose([p_string(<<"*\/">>), fun 'LineTerminator'/2])), fun 'SourceCharacter'/2])), p_string(<<"*\/">>)]))(I,D) end, fun(Node, Idx) ->transform('MultiLineCommentNoLineTerminator', Node, Idx) end).

-spec 'SingleLineComment'(input(), index()) -> parse_result().
'SingleLineComment'(Input, Index) ->
  p(Input, Index, 'SingleLineComment', fun(I,D) -> (p_seq([p_string(<<"\/\/">>), p_zero_or_more(p_seq([p_not(fun 'LineTerminator'/2), fun 'SourceCharacter'/2]))]))(I,D) end, fun(Node, Idx) ->transform('SingleLineComment', Node, Idx) end).

-spec 'Identifier'(input(), index()) -> parse_result().
'Identifier'(Input, Index) ->
  p(Input, Index, 'Identifier', fun(I,D) -> (p_seq([p_not(fun 'ReservedWord'/2), p_label('name', fun 'IdentifierName'/2)]))(I,D) end, fun(Node, Idx) ->transform('Identifier', Node, Idx) end).

-spec 'IdentifierName'(input(), index()) -> parse_result().
'IdentifierName'(Input, Index) ->
  p(Input, Index, 'IdentifierName', fun(I,D) -> (p_seq([p_label('first', p_one_or_more(fun 'IdentifierStart'/2)), p_label('rest', p_zero_or_more(fun 'IdentifierPart'/2))]))(I,D) end, fun(Node, _Idx) ->
    io:format("~p~n", [Node]),
    First = proplists:get_value(first, Node),
    Rest = proplists:get_value(rest, Node),
    iolist_to_binary([First, Rest])
   end).

-spec 'IdentifierStart'(input(), index()) -> parse_result().
'IdentifierStart'(Input, Index) ->
  p(Input, Index, 'IdentifierStart', fun(I,D) -> (p_choose([p_regexp(<<"[a-zA-Z]">>), fun 'UnicodeLetter'/2, p_string(<<"$">>), p_string(<<"_">>), p_seq([p_string(<<"\\">>), p_label('sequence', fun 'UnicodeEscapeSequence'/2)])]))(I,D) end, fun(Node, Idx) ->transform('IdentifierStart', Node, Idx) end).

-spec 'IdentifierPart'(input(), index()) -> parse_result().
'IdentifierPart'(Input, Index) ->
  p(Input, Index, 'IdentifierPart', fun(I,D) -> (p_choose([fun 'IdentifierStart'/2, fun 'UnicodeCombiningMark'/2, fun 'UnicodeDigit'/2, fun 'UnicodeConnectorPunctuation'/2, p_string(<<"\u200C">>), p_string(<<"\u200D">>)]))(I,D) end, fun(Node, Idx) ->transform('IdentifierPart', Node, Idx) end).

-spec 'UnicodeLetter'(input(), index()) -> parse_result().
'UnicodeLetter'(Input, Index) ->
  p(Input, Index, 'UnicodeLetter', fun(I,D) -> (p_choose([fun 'Lu'/2, fun 'Ll'/2, fun 'Lt'/2, fun 'Lm'/2, fun 'Lo'/2, fun 'Nl'/2]))(I,D) end, fun(Node, Idx) ->transform('UnicodeLetter', Node, Idx) end).

-spec 'UnicodeCombiningMark'(input(), index()) -> parse_result().
'UnicodeCombiningMark'(Input, Index) ->
  p(Input, Index, 'UnicodeCombiningMark', fun(I,D) -> (p_choose([fun 'Mn'/2, fun 'Mc'/2]))(I,D) end, fun(Node, Idx) ->transform('UnicodeCombiningMark', Node, Idx) end).

-spec 'UnicodeDigit'(input(), index()) -> parse_result().
'UnicodeDigit'(Input, Index) ->
  p(Input, Index, 'UnicodeDigit', fun(I,D) -> (fun 'Nd'/2)(I,D) end, fun(Node, Idx) ->transform('UnicodeDigit', Node, Idx) end).

-spec 'UnicodeConnectorPunctuation'(input(), index()) -> parse_result().
'UnicodeConnectorPunctuation'(Input, Index) ->
  p(Input, Index, 'UnicodeConnectorPunctuation', fun(I,D) -> (fun 'Pc'/2)(I,D) end, fun(Node, Idx) ->transform('UnicodeConnectorPunctuation', Node, Idx) end).

-spec 'ReservedWord'(input(), index()) -> parse_result().
'ReservedWord'(Input, Index) ->
  p(Input, Index, 'ReservedWord', fun(I,D) -> (p_choose([fun 'Keyword'/2, fun 'FutureReservedWord'/2, fun 'NullLiteral'/2, fun 'BooleanLiteral'/2]))(I,D) end, fun(Node, Idx) ->transform('ReservedWord', Node, Idx) end).

-spec 'Keyword'(input(), index()) -> parse_result().
'Keyword'(Input, Index) ->
  p(Input, Index, 'Keyword', fun(I,D) -> (p_choose([fun 'BreakToken'/2, fun 'CaseToken'/2, fun 'CatchToken'/2, fun 'ContinueToken'/2, fun 'DebuggerToken'/2, fun 'DefaultToken'/2, fun 'DeleteToken'/2, fun 'DoToken'/2, fun 'ElseToken'/2, fun 'FinallyToken'/2, fun 'ForToken'/2, fun 'FunctionToken'/2, fun 'IfToken'/2, fun 'InstanceofToken'/2, fun 'InToken'/2, fun 'NewToken'/2, fun 'ReturnToken'/2, fun 'SwitchToken'/2, fun 'ThisToken'/2, fun 'ThrowToken'/2, fun 'TryToken'/2, fun 'TypeofToken'/2, fun 'VarToken'/2, fun 'VoidToken'/2, fun 'WhileToken'/2, fun 'WithToken'/2]))(I,D) end, fun(Node, Idx) ->transform('Keyword', Node, Idx) end).

-spec 'FutureReservedWord'(input(), index()) -> parse_result().
'FutureReservedWord'(Input, Index) ->
  p(Input, Index, 'FutureReservedWord', fun(I,D) -> (p_choose([fun 'ClassToken'/2, fun 'ConstToken'/2, fun 'EnumToken'/2, fun 'ExportToken'/2, fun 'ExtendsToken'/2, fun 'ImportToken'/2, fun 'SuperToken'/2]))(I,D) end, fun(Node, Idx) ->transform('FutureReservedWord', Node, Idx) end).

-spec 'NullLiteral'(input(), index()) -> parse_result().
'NullLiteral'(Input, Index) ->
  p(Input, Index, 'NullLiteral', fun(I,D) -> (fun 'NullToken'/2)(I,D) end, fun(Node, Idx) ->transform('NullLiteral', Node, Idx) end).

-spec 'BooleanLiteral'(input(), index()) -> parse_result().
'BooleanLiteral'(Input, Index) ->
  p(Input, Index, 'BooleanLiteral', fun(I,D) -> (p_choose([fun 'TrueToken'/2, fun 'FalseToken'/2]))(I,D) end, fun(Node, Idx) ->transform('BooleanLiteral', Node, Idx) end).

-spec 'LiteralMatcher'(input(), index()) -> parse_result().
'LiteralMatcher'(Input, Index) ->
  p(Input, Index, 'LiteralMatcher', fun(I,D) -> (p_seq([p_label('value', fun 'StringLiteral'/2), p_label('ignoreCase', p_optional(p_string(<<"i">>)))]))(I,D) end, fun(Node, Idx) ->transform('LiteralMatcher', Node, Idx) end).

-spec 'StringLiteral'(input(), index()) -> parse_result().
'StringLiteral'(Input, Index) ->
  p(Input, Index, 'StringLiteral', fun(I,D) -> (p_choose([p_seq([p_string(<<"\"">>), p_label('chars', p_zero_or_more(fun 'DoubleStringCharacter'/2)), p_string(<<"\"">>)]), p_seq([p_string(<<"\'">>), p_label('chars', p_zero_or_more(fun 'SingleStringCharacter'/2)), p_string(<<"\'">>)])]))(I,D) end, fun(Node, Idx) ->transform('StringLiteral', Node, Idx) end).

-spec 'DoubleStringCharacter'(input(), index()) -> parse_result().
'DoubleStringCharacter'(Input, Index) ->
  p(Input, Index, 'DoubleStringCharacter', fun(I,D) -> (p_choose([p_seq([p_not(p_choose([p_string(<<"\"">>), p_string(<<"\\">>), fun 'LineTerminator'/2])), fun 'SourceCharacter'/2]), p_seq([p_string(<<"\\">>), p_label('sequence', fun 'EscapeSequence'/2)]), fun 'LineContinuation'/2]))(I,D) end, fun(Node, Idx) ->transform('DoubleStringCharacter', Node, Idx) end).

-spec 'SingleStringCharacter'(input(), index()) -> parse_result().
'SingleStringCharacter'(Input, Index) ->
  p(Input, Index, 'SingleStringCharacter', fun(I,D) -> (p_choose([p_seq([p_not(p_choose([p_string(<<"\'">>), p_string(<<"\\">>), fun 'LineTerminator'/2])), fun 'SourceCharacter'/2]), p_seq([p_string(<<"\\">>), p_label('sequence', fun 'EscapeSequence'/2)]), fun 'LineContinuation'/2]))(I,D) end, fun(Node, Idx) ->transform('SingleStringCharacter', Node, Idx) end).

-spec 'CharacterClassMatcher'(input(), index()) -> parse_result().
'CharacterClassMatcher'(Input, Index) ->
  p(Input, Index, 'CharacterClassMatcher', fun(I,D) -> (p_seq([p_string(<<"[">>), p_label('inverted', p_optional(p_string(<<"^">>))), p_label('parts', p_zero_or_more(p_choose([fun 'ClassCharacterRange'/2, fun 'ClassCharacter'/2]))), p_string(<<"]">>), p_label('ignoreCase', p_optional(p_string(<<"i">>)))]))(I,D) end, fun(Node, Idx) ->transform('CharacterClassMatcher', Node, Idx) end).

-spec 'ClassCharacterRange'(input(), index()) -> parse_result().
'ClassCharacterRange'(Input, Index) ->
  p(Input, Index, 'ClassCharacterRange', fun(I,D) -> (p_seq([p_label('begin', fun 'ClassCharacter'/2), p_string(<<"-">>), p_label('end', fun 'ClassCharacter'/2)]))(I,D) end, fun(Node, Idx) ->transform('ClassCharacterRange', Node, Idx) end).

-spec 'ClassCharacter'(input(), index()) -> parse_result().
'ClassCharacter'(Input, Index) ->
  p(Input, Index, 'ClassCharacter', fun(I,D) -> (p_choose([p_seq([p_not(p_choose([p_string(<<"]">>), p_string(<<"\\">>), fun 'LineTerminator'/2])), fun 'SourceCharacter'/2]), p_seq([p_string(<<"\\">>), p_label('sequence', fun 'EscapeSequence'/2)]), fun 'LineContinuation'/2]))(I,D) end, fun(Node, Idx) ->transform('ClassCharacter', Node, Idx) end).

-spec 'LineContinuation'(input(), index()) -> parse_result().
'LineContinuation'(Input, Index) ->
  p(Input, Index, 'LineContinuation', fun(I,D) -> (p_seq([p_string(<<"\\">>), fun 'LineTerminatorSequence'/2]))(I,D) end, fun(Node, Idx) ->transform('LineContinuation', Node, Idx) end).

-spec 'EscapeSequence'(input(), index()) -> parse_result().
'EscapeSequence'(Input, Index) ->
  p(Input, Index, 'EscapeSequence', fun(I,D) -> (p_choose([fun 'CharacterEscapeSequence'/2, p_seq([p_string(<<"0">>), p_not(fun 'DecimalDigit'/2)]), fun 'HexEscapeSequence'/2, fun 'UnicodeEscapeSequence'/2]))(I,D) end, fun(Node, Idx) ->transform('EscapeSequence', Node, Idx) end).

-spec 'CharacterEscapeSequence'(input(), index()) -> parse_result().
'CharacterEscapeSequence'(Input, Index) ->
  p(Input, Index, 'CharacterEscapeSequence', fun(I,D) -> (p_choose([fun 'SingleEscapeCharacter'/2, fun 'NonEscapeCharacter'/2]))(I,D) end, fun(Node, Idx) ->transform('CharacterEscapeSequence', Node, Idx) end).

-spec 'SingleEscapeCharacter'(input(), index()) -> parse_result().
'SingleEscapeCharacter'(Input, Index) ->
  p(Input, Index, 'SingleEscapeCharacter', fun(I,D) -> (p_choose([p_string(<<"\'">>), p_string(<<"\"">>), p_string(<<"\\">>), p_string(<<"b">>), p_string(<<"f">>), p_string(<<"n">>), p_string(<<"r">>), p_string(<<"t">>), p_string(<<"v">>)]))(I,D) end, fun(Node, Idx) ->transform('SingleEscapeCharacter', Node, Idx) end).

-spec 'NonEscapeCharacter'(input(), index()) -> parse_result().
'NonEscapeCharacter'(Input, Index) ->
  p(Input, Index, 'NonEscapeCharacter', fun(I,D) -> (p_seq([p_not(p_choose([fun 'EscapeCharacter'/2, fun 'LineTerminator'/2])), fun 'SourceCharacter'/2]))(I,D) end, fun(Node, Idx) ->transform('NonEscapeCharacter', Node, Idx) end).

-spec 'EscapeCharacter'(input(), index()) -> parse_result().
'EscapeCharacter'(Input, Index) ->
  p(Input, Index, 'EscapeCharacter', fun(I,D) -> (p_choose([fun 'SingleEscapeCharacter'/2, fun 'DecimalDigit'/2, p_string(<<"x">>), p_string(<<"u">>)]))(I,D) end, fun(Node, Idx) ->transform('EscapeCharacter', Node, Idx) end).

-spec 'HexEscapeSequence'(input(), index()) -> parse_result().
'HexEscapeSequence'(Input, Index) ->
  p(Input, Index, 'HexEscapeSequence', fun(I,D) -> (p_seq([p_string(<<"x">>), p_label('digits', p_seq([fun 'HexDigit'/2, fun 'HexDigit'/2]))]))(I,D) end, fun(Node, Idx) ->transform('HexEscapeSequence', Node, Idx) end).

-spec 'UnicodeEscapeSequence'(input(), index()) -> parse_result().
'UnicodeEscapeSequence'(Input, Index) ->
  p(Input, Index, 'UnicodeEscapeSequence', fun(I,D) -> (p_seq([p_string(<<"u">>), p_label('digits', fun 'HexDigit'/2), fun 'HexDigit'/2, fun 'HexDigit'/2, fun 'HexDigit'/2]))(I,D) end, fun(Node, Idx) ->transform('UnicodeEscapeSequence', Node, Idx) end).

-spec 'DecimalDigit'(input(), index()) -> parse_result().
'DecimalDigit'(Input, Index) ->
  p(Input, Index, 'DecimalDigit', fun(I,D) -> (p_charclass(<<"[0-9]">>))(I,D) end, fun(Node, Idx) ->transform('DecimalDigit', Node, Idx) end).

-spec 'HexDigit'(input(), index()) -> parse_result().
'HexDigit'(Input, Index) ->
  p(Input, Index, 'HexDigit', fun(I,D) -> (p_charclass(<<"[0-9a-fA-F]">>))(I,D) end, fun(Node, Idx) ->transform('HexDigit', Node, Idx) end).

-spec 'AnyMatcher'(input(), index()) -> parse_result().
'AnyMatcher'(Input, Index) ->
  p(Input, Index, 'AnyMatcher', fun(I,D) -> (p_string(<<".">>))(I,D) end, fun(Node, Idx) ->transform('AnyMatcher', Node, Idx) end).

-spec 'CodeBlock'(input(), index()) -> parse_result().
'CodeBlock'(Input, Index) ->
  p(Input, Index, 'CodeBlock', fun(I,D) -> (p_seq([p_string(<<"{">>), p_label('code', fun 'Code'/2), p_string(<<"}">>)]))(I,D) end, fun(Node, Idx) ->transform('CodeBlock', Node, Idx) end).

-spec 'Code'(input(), index()) -> parse_result().
'Code'(Input, Index) ->
  p(Input, Index, 'Code', fun(I,D) -> (p_zero_or_more(p_choose([p_one_or_more(p_seq([p_not(p_charclass(<<"[{}]">>)), fun 'SourceCharacter'/2])), p_seq([p_string(<<"{">>), fun 'Code'/2, p_string(<<"}">>)])])))(I,D) end, fun(Node, Idx) ->transform('Code', Node, Idx) end).

-spec 'Ll'(input(), index()) -> parse_result().
'Ll'(Input, Index) ->
  p(Input, Index, 'Ll', fun(I,D) -> (p_charclass(<<"[\u0061-\u007A\u00B5\u00DF-\u00F6\u00F8-\u00FF\u0101\u0103\u0105\u0107\u0109\u010B\u010D\u010F\u0111\u0113\u0115\u0117\u0119\u011B\u011D\u011F\u0121\u0123\u0125\u0127\u0129\u012B\u012D\u012F\u0131\u0133\u0135\u0137-\u0138\u013A\u013C\u013E\u0140\u0142\u0144\u0146\u0148-\u0149\u014B\u014D\u014F\u0151\u0153\u0155\u0157\u0159\u015B\u015D\u015F\u0161\u0163\u0165\u0167\u0169\u016B\u016D\u016F\u0171\u0173\u0175\u0177\u017A\u017C\u017E-\u0180\u0183\u0185\u0188\u018C-\u018D\u0192\u0195\u0199-\u019B\u019E\u01A1\u01A3\u01A5\u01A8\u01AA-\u01AB\u01AD\u01B0\u01B4\u01B6\u01B9-\u01BA\u01BD-\u01BF\u01C6\u01C9\u01CC\u01CE\u01D0\u01D2\u01D4\u01D6\u01D8\u01DA\u01DC-\u01DD\u01DF\u01E1\u01E3\u01E5\u01E7\u01E9\u01EB\u01ED\u01EF-\u01F0\u01F3\u01F5\u01F9\u01FB\u01FD\u01FF\u0201\u0203\u0205\u0207\u0209\u020B\u020D\u020F\u0211\u0213\u0215\u0217\u0219\u021B\u021D\u021F\u0221\u0223\u0225\u0227\u0229\u022B\u022D\u022F\u0231\u0233-\u0239\u023C\u023F-\u0240\u0242\u0247\u0249\u024B\u024D\u024F-\u0293\u0295-\u02AF\u0371\u0373\u0377\u037B-\u037D\u0390\u03AC-\u03CE\u03D0-\u03D1\u03D5-\u03D7\u03D9\u03DB\u03DD\u03DF\u03E1\u03E3\u03E5\u03E7\u03E9\u03EB\u03ED\u03EF-\u03F3\u03F5\u03F8\u03FB-\u03FC\u0430-\u045F\u0461\u0463\u0465\u0467\u0469\u046B\u046D\u046F\u0471\u0473\u0475\u0477\u0479\u047B\u047D\u047F\u0481\u048B\u048D\u048F\u0491\u0493\u0495\u0497\u0499\u049B\u049D\u049F\u04A1\u04A3\u04A5\u04A7\u04A9\u04AB\u04AD\u04AF\u04B1\u04B3\u04B5\u04B7\u04B9\u04BB\u04BD\u04BF\u04C2\u04C4\u04C6\u04C8\u04CA\u04CC\u04CE-\u04CF\u04D1\u04D3\u04D5\u04D7\u04D9\u04DB\u04DD\u04DF\u04E1\u04E3\u04E5\u04E7\u04E9\u04EB\u04ED\u04EF\u04F1\u04F3\u04F5\u04F7\u04F9\u04FB\u04FD\u04FF\u0501\u0503\u0505\u0507\u0509\u050B\u050D\u050F\u0511\u0513\u0515\u0517\u0519\u051B\u051D\u051F\u0521\u0523\u0525\u0527\u0561-\u0587\u1D00-\u1D2B\u1D6B-\u1D77\u1D79-\u1D9A\u1E01\u1E03\u1E05\u1E07\u1E09\u1E0B\u1E0D\u1E0F\u1E11\u1E13\u1E15\u1E17\u1E19\u1E1B\u1E1D\u1E1F\u1E21\u1E23\u1E25\u1E27\u1E29\u1E2B\u1E2D\u1E2F\u1E31\u1E33\u1E35\u1E37\u1E39\u1E3B\u1E3D\u1E3F\u1E41\u1E43\u1E45\u1E47\u1E49\u1E4B\u1E4D\u1E4F\u1E51\u1E53\u1E55\u1E57\u1E59\u1E5B\u1E5D\u1E5F\u1E61\u1E63\u1E65\u1E67\u1E69\u1E6B\u1E6D\u1E6F\u1E71\u1E73\u1E75\u1E77\u1E79\u1E7B\u1E7D\u1E7F\u1E81\u1E83\u1E85\u1E87\u1E89\u1E8B\u1E8D\u1E8F\u1E91\u1E93\u1E95-\u1E9D\u1E9F\u1EA1\u1EA3\u1EA5\u1EA7\u1EA9\u1EAB\u1EAD\u1EAF\u1EB1\u1EB3\u1EB5\u1EB7\u1EB9\u1EBB\u1EBD\u1EBF\u1EC1\u1EC3\u1EC5\u1EC7\u1EC9\u1ECB\u1ECD\u1ECF\u1ED1\u1ED3\u1ED5\u1ED7\u1ED9\u1EDB\u1EDD\u1EDF\u1EE1\u1EE3\u1EE5\u1EE7\u1EE9\u1EEB\u1EED\u1EEF\u1EF1\u1EF3\u1EF5\u1EF7\u1EF9\u1EFB\u1EFD\u1EFF-\u1F07\u1F10-\u1F15\u1F20-\u1F27\u1F30-\u1F37\u1F40-\u1F45\u1F50-\u1F57\u1F60-\u1F67\u1F70-\u1F7D\u1F80-\u1F87\u1F90-\u1F97\u1FA0-\u1FA7\u1FB0-\u1FB4\u1FB6-\u1FB7\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FC7\u1FD0-\u1FD3\u1FD6-\u1FD7\u1FE0-\u1FE7\u1FF2-\u1FF4\u1FF6-\u1FF7\u210A\u210E-\u210F\u2113\u212F\u2134\u2139\u213C-\u213D\u2146-\u2149\u214E\u2184\u2C30-\u2C5E\u2C61\u2C65-\u2C66\u2C68\u2C6A\u2C6C\u2C71\u2C73-\u2C74\u2C76-\u2C7B\u2C81\u2C83\u2C85\u2C87\u2C89\u2C8B\u2C8D\u2C8F\u2C91\u2C93\u2C95\u2C97\u2C99\u2C9B\u2C9D\u2C9F\u2CA1\u2CA3\u2CA5\u2CA7\u2CA9\u2CAB\u2CAD\u2CAF\u2CB1\u2CB3\u2CB5\u2CB7\u2CB9\u2CBB\u2CBD\u2CBF\u2CC1\u2CC3\u2CC5\u2CC7\u2CC9\u2CCB\u2CCD\u2CCF\u2CD1\u2CD3\u2CD5\u2CD7\u2CD9\u2CDB\u2CDD\u2CDF\u2CE1\u2CE3-\u2CE4\u2CEC\u2CEE\u2CF3\u2D00-\u2D25\u2D27\u2D2D\uA641\uA643\uA645\uA647\uA649\uA64B\uA64D\uA64F\uA651\uA653\uA655\uA657\uA659\uA65B\uA65D\uA65F\uA661\uA663\uA665\uA667\uA669\uA66B\uA66D\uA681\uA683\uA685\uA687\uA689\uA68B\uA68D\uA68F\uA691\uA693\uA695\uA697\uA723\uA725\uA727\uA729\uA72B\uA72D\uA72F-\uA731\uA733\uA735\uA737\uA739\uA73B\uA73D\uA73F\uA741\uA743\uA745\uA747\uA749\uA74B\uA74D\uA74F\uA751\uA753\uA755\uA757\uA759\uA75B\uA75D\uA75F\uA761\uA763\uA765\uA767\uA769\uA76B\uA76D\uA76F\uA771-\uA778\uA77A\uA77C\uA77F\uA781\uA783\uA785\uA787\uA78C\uA78E\uA791\uA793\uA7A1\uA7A3\uA7A5\uA7A7\uA7A9\uA7FA\uFB00-\uFB06\uFB13-\uFB17\uFF41-\uFF5A]">>))(I,D) end, fun(Node, Idx) ->transform('Ll', Node, Idx) end).

-spec 'Lm'(input(), index()) -> parse_result().
'Lm'(Input, Index) ->
  p(Input, Index, 'Lm', fun(I,D) -> (p_charclass(<<"[\u02B0-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0374\u037A\u0559\u0640\u06E5-\u06E6\u07F4-\u07F5\u07FA\u081A\u0824\u0828\u0971\u0E46\u0EC6\u10FC\u17D7\u1843\u1AA7\u1C78-\u1C7D\u1D2C-\u1D6A\u1D78\u1D9B-\u1DBF\u2071\u207F\u2090-\u209C\u2C7C-\u2C7D\u2D6F\u2E2F\u3005\u3031-\u3035\u303B\u309D-\u309E\u30FC-\u30FE\uA015\uA4F8-\uA4FD\uA60C\uA67F\uA717-\uA71F\uA770\uA788\uA7F8-\uA7F9\uA9CF\uAA70\uAADD\uAAF3-\uAAF4\uFF70\uFF9E-\uFF9F]">>))(I,D) end, fun(Node, Idx) ->transform('Lm', Node, Idx) end).

-spec 'Lo'(input(), index()) -> parse_result().
'Lo'(Input, Index) ->
  p(Input, Index, 'Lo', fun(I,D) -> (p_charclass(<<"[\u00AA\u00BA\u01BB\u01C0-\u01C3\u0294\u05D0-\u05EA\u05F0-\u05F2\u0620-\u063F\u0641-\u064A\u066E-\u066F\u0671-\u06D3\u06D5\u06EE-\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u0800-\u0815\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0972-\u0977\u0979-\u097F\u0985-\u098C\u098F-\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC-\u09DD\u09DF-\u09E1\u09F0-\u09F1\u0A05-\u0A0A\u0A0F-\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32-\u0A33\u0A35-\u0A36\u0A38-\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2-\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0-\u0AE1\u0B05-\u0B0C\u0B0F-\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32-\u0B33\u0B35-\u0B39\u0B3D\u0B5C-\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99-\u0B9A\u0B9C\u0B9E-\u0B9F\u0BA3-\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58-\u0C59\u0C60-\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0-\u0CE1\u0CF1-\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60-\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32-\u0E33\u0E40-\u0E45\u0E81-\u0E82\u0E84\u0E87-\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA-\u0EAB\u0EAD-\u0EB0\u0EB2-\u0EB3\u0EBD\u0EC0-\u0EC4\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065-\u1066\u106E-\u1070\u1075-\u1081\u108E\u10D0-\u10FA\u10FD-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17DC\u1820-\u1842\u1844-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE-\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C77\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5-\u1CF6\u2135-\u2138\u2D30-\u2D67\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u3006\u303C\u3041-\u3096\u309F\u30A1-\u30FA\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA014\uA016-\uA48C\uA4D0-\uA4F7\uA500-\uA60B\uA610-\uA61F\uA62A-\uA62B\uA66E\uA6A0-\uA6E5\uA7FB-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA6F\uAA71-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5-\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADC\uAAE0-\uAAEA\uAAF2\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40-\uFB41\uFB43-\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF66-\uFF6F\uFF71-\uFF9D\uFFA0-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]">>))(I,D) end, fun(Node, Idx) ->transform('Lo', Node, Idx) end).

-spec 'Lt'(input(), index()) -> parse_result().
'Lt'(Input, Index) ->
  p(Input, Index, 'Lt', fun(I,D) -> (p_charclass(<<"[\u01C5\u01C8\u01CB\u01F2\u1F88-\u1F8F\u1F98-\u1F9F\u1FA8-\u1FAF\u1FBC\u1FCC\u1FFC]">>))(I,D) end, fun(Node, Idx) ->transform('Lt', Node, Idx) end).

-spec 'Lu'(input(), index()) -> parse_result().
'Lu'(Input, Index) ->
  p(Input, Index, 'Lu', fun(I,D) -> (p_charclass(<<"[\u0041-\u005A\u00C0-\u00D6\u00D8-\u00DE\u0100\u0102\u0104\u0106\u0108\u010A\u010C\u010E\u0110\u0112\u0114\u0116\u0118\u011A\u011C\u011E\u0120\u0122\u0124\u0126\u0128\u012A\u012C\u012E\u0130\u0132\u0134\u0136\u0139\u013B\u013D\u013F\u0141\u0143\u0145\u0147\u014A\u014C\u014E\u0150\u0152\u0154\u0156\u0158\u015A\u015C\u015E\u0160\u0162\u0164\u0166\u0168\u016A\u016C\u016E\u0170\u0172\u0174\u0176\u0178-\u0179\u017B\u017D\u0181-\u0182\u0184\u0186-\u0187\u0189-\u018B\u018E-\u0191\u0193-\u0194\u0196-\u0198\u019C-\u019D\u019F-\u01A0\u01A2\u01A4\u01A6-\u01A7\u01A9\u01AC\u01AE-\u01AF\u01B1-\u01B3\u01B5\u01B7-\u01B8\u01BC\u01C4\u01C7\u01CA\u01CD\u01CF\u01D1\u01D3\u01D5\u01D7\u01D9\u01DB\u01DE\u01E0\u01E2\u01E4\u01E6\u01E8\u01EA\u01EC\u01EE\u01F1\u01F4\u01F6-\u01F8\u01FA\u01FC\u01FE\u0200\u0202\u0204\u0206\u0208\u020A\u020C\u020E\u0210\u0212\u0214\u0216\u0218\u021A\u021C\u021E\u0220\u0222\u0224\u0226\u0228\u022A\u022C\u022E\u0230\u0232\u023A-\u023B\u023D-\u023E\u0241\u0243-\u0246\u0248\u024A\u024C\u024E\u0370\u0372\u0376\u0386\u0388-\u038A\u038C\u038E-\u038F\u0391-\u03A1\u03A3-\u03AB\u03CF\u03D2-\u03D4\u03D8\u03DA\u03DC\u03DE\u03E0\u03E2\u03E4\u03E6\u03E8\u03EA\u03EC\u03EE\u03F4\u03F7\u03F9-\u03FA\u03FD-\u042F\u0460\u0462\u0464\u0466\u0468\u046A\u046C\u046E\u0470\u0472\u0474\u0476\u0478\u047A\u047C\u047E\u0480\u048A\u048C\u048E\u0490\u0492\u0494\u0496\u0498\u049A\u049C\u049E\u04A0\u04A2\u04A4\u04A6\u04A8\u04AA\u04AC\u04AE\u04B0\u04B2\u04B4\u04B6\u04B8\u04BA\u04BC\u04BE\u04C0-\u04C1\u04C3\u04C5\u04C7\u04C9\u04CB\u04CD\u04D0\u04D2\u04D4\u04D6\u04D8\u04DA\u04DC\u04DE\u04E0\u04E2\u04E4\u04E6\u04E8\u04EA\u04EC\u04EE\u04F0\u04F2\u04F4\u04F6\u04F8\u04FA\u04FC\u04FE\u0500\u0502\u0504\u0506\u0508\u050A\u050C\u050E\u0510\u0512\u0514\u0516\u0518\u051A\u051C\u051E\u0520\u0522\u0524\u0526\u0531-\u0556\u10A0-\u10C5\u10C7\u10CD\u1E00\u1E02\u1E04\u1E06\u1E08\u1E0A\u1E0C\u1E0E\u1E10\u1E12\u1E14\u1E16\u1E18\u1E1A\u1E1C\u1E1E\u1E20\u1E22\u1E24\u1E26\u1E28\u1E2A\u1E2C\u1E2E\u1E30\u1E32\u1E34\u1E36\u1E38\u1E3A\u1E3C\u1E3E\u1E40\u1E42\u1E44\u1E46\u1E48\u1E4A\u1E4C\u1E4E\u1E50\u1E52\u1E54\u1E56\u1E58\u1E5A\u1E5C\u1E5E\u1E60\u1E62\u1E64\u1E66\u1E68\u1E6A\u1E6C\u1E6E\u1E70\u1E72\u1E74\u1E76\u1E78\u1E7A\u1E7C\u1E7E\u1E80\u1E82\u1E84\u1E86\u1E88\u1E8A\u1E8C\u1E8E\u1E90\u1E92\u1E94\u1E9E\u1EA0\u1EA2\u1EA4\u1EA6\u1EA8\u1EAA\u1EAC\u1EAE\u1EB0\u1EB2\u1EB4\u1EB6\u1EB8\u1EBA\u1EBC\u1EBE\u1EC0\u1EC2\u1EC4\u1EC6\u1EC8\u1ECA\u1ECC\u1ECE\u1ED0\u1ED2\u1ED4\u1ED6\u1ED8\u1EDA\u1EDC\u1EDE\u1EE0\u1EE2\u1EE4\u1EE6\u1EE8\u1EEA\u1EEC\u1EEE\u1EF0\u1EF2\u1EF4\u1EF6\u1EF8\u1EFA\u1EFC\u1EFE\u1F08-\u1F0F\u1F18-\u1F1D\u1F28-\u1F2F\u1F38-\u1F3F\u1F48-\u1F4D\u1F59\u1F5B\u1F5D\u1F5F\u1F68-\u1F6F\u1FB8-\u1FBB\u1FC8-\u1FCB\u1FD8-\u1FDB\u1FE8-\u1FEC\u1FF8-\u1FFB\u2102\u2107\u210B-\u210D\u2110-\u2112\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u2130-\u2133\u213E-\u213F\u2145\u2183\u2C00-\u2C2E\u2C60\u2C62-\u2C64\u2C67\u2C69\u2C6B\u2C6D-\u2C70\u2C72\u2C75\u2C7E-\u2C80\u2C82\u2C84\u2C86\u2C88\u2C8A\u2C8C\u2C8E\u2C90\u2C92\u2C94\u2C96\u2C98\u2C9A\u2C9C\u2C9E\u2CA0\u2CA2\u2CA4\u2CA6\u2CA8\u2CAA\u2CAC\u2CAE\u2CB0\u2CB2\u2CB4\u2CB6\u2CB8\u2CBA\u2CBC\u2CBE\u2CC0\u2CC2\u2CC4\u2CC6\u2CC8\u2CCA\u2CCC\u2CCE\u2CD0\u2CD2\u2CD4\u2CD6\u2CD8\u2CDA\u2CDC\u2CDE\u2CE0\u2CE2\u2CEB\u2CED\u2CF2\uA640\uA642\uA644\uA646\uA648\uA64A\uA64C\uA64E\uA650\uA652\uA654\uA656\uA658\uA65A\uA65C\uA65E\uA660\uA662\uA664\uA666\uA668\uA66A\uA66C\uA680\uA682\uA684\uA686\uA688\uA68A\uA68C\uA68E\uA690\uA692\uA694\uA696\uA722\uA724\uA726\uA728\uA72A\uA72C\uA72E\uA732\uA734\uA736\uA738\uA73A\uA73C\uA73E\uA740\uA742\uA744\uA746\uA748\uA74A\uA74C\uA74E\uA750\uA752\uA754\uA756\uA758\uA75A\uA75C\uA75E\uA760\uA762\uA764\uA766\uA768\uA76A\uA76C\uA76E\uA779\uA77B\uA77D-\uA77E\uA780\uA782\uA784\uA786\uA78B\uA78D\uA790\uA792\uA7A0\uA7A2\uA7A4\uA7A6\uA7A8\uA7AA\uFF21-\uFF3A]">>))(I,D) end, fun(Node, Idx) ->transform('Lu', Node, Idx) end).

-spec 'Mc'(input(), index()) -> parse_result().
'Mc'(Input, Index) ->
  p(Input, Index, 'Mc', fun(I,D) -> (p_charclass(<<"[\u0903\u093B\u093E-\u0940\u0949-\u094C\u094E-\u094F\u0982-\u0983\u09BE-\u09C0\u09C7-\u09C8\u09CB-\u09CC\u09D7\u0A03\u0A3E-\u0A40\u0A83\u0ABE-\u0AC0\u0AC9\u0ACB-\u0ACC\u0B02-\u0B03\u0B3E\u0B40\u0B47-\u0B48\u0B4B-\u0B4C\u0B57\u0BBE-\u0BBF\u0BC1-\u0BC2\u0BC6-\u0BC8\u0BCA-\u0BCC\u0BD7\u0C01-\u0C03\u0C41-\u0C44\u0C82-\u0C83\u0CBE\u0CC0-\u0CC4\u0CC7-\u0CC8\u0CCA-\u0CCB\u0CD5-\u0CD6\u0D02-\u0D03\u0D3E-\u0D40\u0D46-\u0D48\u0D4A-\u0D4C\u0D57\u0D82-\u0D83\u0DCF-\u0DD1\u0DD8-\u0DDF\u0DF2-\u0DF3\u0F3E-\u0F3F\u0F7F\u102B-\u102C\u1031\u1038\u103B-\u103C\u1056-\u1057\u1062-\u1064\u1067-\u106D\u1083-\u1084\u1087-\u108C\u108F\u109A-\u109C\u17B6\u17BE-\u17C5\u17C7-\u17C8\u1923-\u1926\u1929-\u192B\u1930-\u1931\u1933-\u1938\u19B0-\u19C0\u19C8-\u19C9\u1A19-\u1A1A\u1A55\u1A57\u1A61\u1A63-\u1A64\u1A6D-\u1A72\u1B04\u1B35\u1B3B\u1B3D-\u1B41\u1B43-\u1B44\u1B82\u1BA1\u1BA6-\u1BA7\u1BAA\u1BAC-\u1BAD\u1BE7\u1BEA-\u1BEC\u1BEE\u1BF2-\u1BF3\u1C24-\u1C2B\u1C34-\u1C35\u1CE1\u1CF2-\u1CF3\u302E-\u302F\uA823-\uA824\uA827\uA880-\uA881\uA8B4-\uA8C3\uA952-\uA953\uA983\uA9B4-\uA9B5\uA9BA-\uA9BB\uA9BD-\uA9C0\uAA2F-\uAA30\uAA33-\uAA34\uAA4D\uAA7B\uAAEB\uAAEE-\uAAEF\uAAF5\uABE3-\uABE4\uABE6-\uABE7\uABE9-\uABEA\uABEC]">>))(I,D) end, fun(Node, Idx) ->transform('Mc', Node, Idx) end).

-spec 'Mn'(input(), index()) -> parse_result().
'Mn'(Input, Index) ->
  p(Input, Index, 'Mn', fun(I,D) -> (p_charclass(<<"[\u0300-\u036F\u0483-\u0487\u0591-\u05BD\u05BF\u05C1-\u05C2\u05C4-\u05C5\u05C7\u0610-\u061A\u064B-\u065F\u0670\u06D6-\u06DC\u06DF-\u06E4\u06E7-\u06E8\u06EA-\u06ED\u0711\u0730-\u074A\u07A6-\u07B0\u07EB-\u07F3\u0816-\u0819\u081B-\u0823\u0825-\u0827\u0829-\u082D\u0859-\u085B\u08E4-\u08FE\u0900-\u0902\u093A\u093C\u0941-\u0948\u094D\u0951-\u0957\u0962-\u0963\u0981\u09BC\u09C1-\u09C4\u09CD\u09E2-\u09E3\u0A01-\u0A02\u0A3C\u0A41-\u0A42\u0A47-\u0A48\u0A4B-\u0A4D\u0A51\u0A70-\u0A71\u0A75\u0A81-\u0A82\u0ABC\u0AC1-\u0AC5\u0AC7-\u0AC8\u0ACD\u0AE2-\u0AE3\u0B01\u0B3C\u0B3F\u0B41-\u0B44\u0B4D\u0B56\u0B62-\u0B63\u0B82\u0BC0\u0BCD\u0C3E-\u0C40\u0C46-\u0C48\u0C4A-\u0C4D\u0C55-\u0C56\u0C62-\u0C63\u0CBC\u0CBF\u0CC6\u0CCC-\u0CCD\u0CE2-\u0CE3\u0D41-\u0D44\u0D4D\u0D62-\u0D63\u0DCA\u0DD2-\u0DD4\u0DD6\u0E31\u0E34-\u0E3A\u0E47-\u0E4E\u0EB1\u0EB4-\u0EB9\u0EBB-\u0EBC\u0EC8-\u0ECD\u0F18-\u0F19\u0F35\u0F37\u0F39\u0F71-\u0F7E\u0F80-\u0F84\u0F86-\u0F87\u0F8D-\u0F97\u0F99-\u0FBC\u0FC6\u102D-\u1030\u1032-\u1037\u1039-\u103A\u103D-\u103E\u1058-\u1059\u105E-\u1060\u1071-\u1074\u1082\u1085-\u1086\u108D\u109D\u135D-\u135F\u1712-\u1714\u1732-\u1734\u1752-\u1753\u1772-\u1773\u17B4-\u17B5\u17B7-\u17BD\u17C6\u17C9-\u17D3\u17DD\u180B-\u180D\u18A9\u1920-\u1922\u1927-\u1928\u1932\u1939-\u193B\u1A17-\u1A18\u1A1B\u1A56\u1A58-\u1A5E\u1A60\u1A62\u1A65-\u1A6C\u1A73-\u1A7C\u1A7F\u1B00-\u1B03\u1B34\u1B36-\u1B3A\u1B3C\u1B42\u1B6B-\u1B73\u1B80-\u1B81\u1BA2-\u1BA5\u1BA8-\u1BA9\u1BAB\u1BE6\u1BE8-\u1BE9\u1BED\u1BEF-\u1BF1\u1C2C-\u1C33\u1C36-\u1C37\u1CD0-\u1CD2\u1CD4-\u1CE0\u1CE2-\u1CE8\u1CED\u1CF4\u1DC0-\u1DE6\u1DFC-\u1DFF\u20D0-\u20DC\u20E1\u20E5-\u20F0\u2CEF-\u2CF1\u2D7F\u2DE0-\u2DFF\u302A-\u302D\u3099-\u309A\uA66F\uA674-\uA67D\uA69F\uA6F0-\uA6F1\uA802\uA806\uA80B\uA825-\uA826\uA8C4\uA8E0-\uA8F1\uA926-\uA92D\uA947-\uA951\uA980-\uA982\uA9B3\uA9B6-\uA9B9\uA9BC\uAA29-\uAA2E\uAA31-\uAA32\uAA35-\uAA36\uAA43\uAA4C\uAAB0\uAAB2-\uAAB4\uAAB7-\uAAB8\uAABE-\uAABF\uAAC1\uAAEC-\uAAED\uAAF6\uABE5\uABE8\uABED\uFB1E\uFE00-\uFE0F\uFE20-\uFE26]">>))(I,D) end, fun(Node, Idx) ->transform('Mn', Node, Idx) end).

-spec 'Nd'(input(), index()) -> parse_result().
'Nd'(Input, Index) ->
  p(Input, Index, 'Nd', fun(I,D) -> (p_charclass(<<"[\u0030-\u0039\u0660-\u0669\u06F0-\u06F9\u07C0-\u07C9\u0966-\u096F\u09E6-\u09EF\u0A66-\u0A6F\u0AE6-\u0AEF\u0B66-\u0B6F\u0BE6-\u0BEF\u0C66-\u0C6F\u0CE6-\u0CEF\u0D66-\u0D6F\u0E50-\u0E59\u0ED0-\u0ED9\u0F20-\u0F29\u1040-\u1049\u1090-\u1099\u17E0-\u17E9\u1810-\u1819\u1946-\u194F\u19D0-\u19D9\u1A80-\u1A89\u1A90-\u1A99\u1B50-\u1B59\u1BB0-\u1BB9\u1C40-\u1C49\u1C50-\u1C59\uA620-\uA629\uA8D0-\uA8D9\uA900-\uA909\uA9D0-\uA9D9\uAA50-\uAA59\uABF0-\uABF9\uFF10-\uFF19]">>))(I,D) end, fun(Node, Idx) ->transform('Nd', Node, Idx) end).

-spec 'Nl'(input(), index()) -> parse_result().
'Nl'(Input, Index) ->
  p(Input, Index, 'Nl', fun(I,D) -> (p_charclass(<<"[\u16EE-\u16F0\u2160-\u2182\u2185-\u2188\u3007\u3021-\u3029\u3038-\u303A\uA6E6-\uA6EF]">>))(I,D) end, fun(Node, Idx) ->transform('Nl', Node, Idx) end).

-spec 'Pc'(input(), index()) -> parse_result().
'Pc'(Input, Index) ->
  p(Input, Index, 'Pc', fun(I,D) -> (p_charclass(<<"[\u005F\u203F-\u2040\u2054\uFE33-\uFE34\uFE4D-\uFE4F\uFF3F]">>))(I,D) end, fun(Node, Idx) ->transform('Pc', Node, Idx) end).

-spec 'Zs'(input(), index()) -> parse_result().
'Zs'(Input, Index) ->
  p(Input, Index, 'Zs', fun(I,D) -> (p_charclass(<<"[\u0020\u00A0\u1680\u2000-\u200A\u202F\u205F\u3000]">>))(I,D) end, fun(Node, Idx) ->transform('Zs', Node, Idx) end).

-spec 'BreakToken'(input(), index()) -> parse_result().
'BreakToken'(Input, Index) ->
  p(Input, Index, 'BreakToken', fun(I,D) -> (p_seq([p_string(<<"break">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('BreakToken', Node, Idx) end).

-spec 'CaseToken'(input(), index()) -> parse_result().
'CaseToken'(Input, Index) ->
  p(Input, Index, 'CaseToken', fun(I,D) -> (p_seq([p_string(<<"case">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('CaseToken', Node, Idx) end).

-spec 'CatchToken'(input(), index()) -> parse_result().
'CatchToken'(Input, Index) ->
  p(Input, Index, 'CatchToken', fun(I,D) -> (p_seq([p_string(<<"catch">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('CatchToken', Node, Idx) end).

-spec 'ClassToken'(input(), index()) -> parse_result().
'ClassToken'(Input, Index) ->
  p(Input, Index, 'ClassToken', fun(I,D) -> (p_seq([p_string(<<"class">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('ClassToken', Node, Idx) end).

-spec 'ConstToken'(input(), index()) -> parse_result().
'ConstToken'(Input, Index) ->
  p(Input, Index, 'ConstToken', fun(I,D) -> (p_seq([p_string(<<"const">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('ConstToken', Node, Idx) end).

-spec 'ContinueToken'(input(), index()) -> parse_result().
'ContinueToken'(Input, Index) ->
  p(Input, Index, 'ContinueToken', fun(I,D) -> (p_seq([p_string(<<"continue">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('ContinueToken', Node, Idx) end).

-spec 'DebuggerToken'(input(), index()) -> parse_result().
'DebuggerToken'(Input, Index) ->
  p(Input, Index, 'DebuggerToken', fun(I,D) -> (p_seq([p_string(<<"debugger">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('DebuggerToken', Node, Idx) end).

-spec 'DefaultToken'(input(), index()) -> parse_result().
'DefaultToken'(Input, Index) ->
  p(Input, Index, 'DefaultToken', fun(I,D) -> (p_seq([p_string(<<"default">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('DefaultToken', Node, Idx) end).

-spec 'DeleteToken'(input(), index()) -> parse_result().
'DeleteToken'(Input, Index) ->
  p(Input, Index, 'DeleteToken', fun(I,D) -> (p_seq([p_string(<<"delete">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('DeleteToken', Node, Idx) end).

-spec 'DoToken'(input(), index()) -> parse_result().
'DoToken'(Input, Index) ->
  p(Input, Index, 'DoToken', fun(I,D) -> (p_seq([p_string(<<"do">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('DoToken', Node, Idx) end).

-spec 'ElseToken'(input(), index()) -> parse_result().
'ElseToken'(Input, Index) ->
  p(Input, Index, 'ElseToken', fun(I,D) -> (p_seq([p_string(<<"else">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('ElseToken', Node, Idx) end).

-spec 'EnumToken'(input(), index()) -> parse_result().
'EnumToken'(Input, Index) ->
  p(Input, Index, 'EnumToken', fun(I,D) -> (p_seq([p_string(<<"enum">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('EnumToken', Node, Idx) end).

-spec 'ExportToken'(input(), index()) -> parse_result().
'ExportToken'(Input, Index) ->
  p(Input, Index, 'ExportToken', fun(I,D) -> (p_seq([p_string(<<"export">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('ExportToken', Node, Idx) end).

-spec 'ExtendsToken'(input(), index()) -> parse_result().
'ExtendsToken'(Input, Index) ->
  p(Input, Index, 'ExtendsToken', fun(I,D) -> (p_seq([p_string(<<"extends">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('ExtendsToken', Node, Idx) end).

-spec 'FalseToken'(input(), index()) -> parse_result().
'FalseToken'(Input, Index) ->
  p(Input, Index, 'FalseToken', fun(I,D) -> (p_seq([p_string(<<"false">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('FalseToken', Node, Idx) end).

-spec 'FinallyToken'(input(), index()) -> parse_result().
'FinallyToken'(Input, Index) ->
  p(Input, Index, 'FinallyToken', fun(I,D) -> (p_seq([p_string(<<"finally">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('FinallyToken', Node, Idx) end).

-spec 'ForToken'(input(), index()) -> parse_result().
'ForToken'(Input, Index) ->
  p(Input, Index, 'ForToken', fun(I,D) -> (p_seq([p_string(<<"for">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('ForToken', Node, Idx) end).

-spec 'FunctionToken'(input(), index()) -> parse_result().
'FunctionToken'(Input, Index) ->
  p(Input, Index, 'FunctionToken', fun(I,D) -> (p_seq([p_string(<<"function">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('FunctionToken', Node, Idx) end).

-spec 'IfToken'(input(), index()) -> parse_result().
'IfToken'(Input, Index) ->
  p(Input, Index, 'IfToken', fun(I,D) -> (p_seq([p_string(<<"if">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('IfToken', Node, Idx) end).

-spec 'ImportToken'(input(), index()) -> parse_result().
'ImportToken'(Input, Index) ->
  p(Input, Index, 'ImportToken', fun(I,D) -> (p_seq([p_string(<<"import">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('ImportToken', Node, Idx) end).

-spec 'InstanceofToken'(input(), index()) -> parse_result().
'InstanceofToken'(Input, Index) ->
  p(Input, Index, 'InstanceofToken', fun(I,D) -> (p_seq([p_string(<<"instanceof">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('InstanceofToken', Node, Idx) end).

-spec 'InToken'(input(), index()) -> parse_result().
'InToken'(Input, Index) ->
  p(Input, Index, 'InToken', fun(I,D) -> (p_seq([p_string(<<"in">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('InToken', Node, Idx) end).

-spec 'NewToken'(input(), index()) -> parse_result().
'NewToken'(Input, Index) ->
  p(Input, Index, 'NewToken', fun(I,D) -> (p_seq([p_string(<<"new">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('NewToken', Node, Idx) end).

-spec 'NullToken'(input(), index()) -> parse_result().
'NullToken'(Input, Index) ->
  p(Input, Index, 'NullToken', fun(I,D) -> (p_seq([p_string(<<"null">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('NullToken', Node, Idx) end).

-spec 'ReturnToken'(input(), index()) -> parse_result().
'ReturnToken'(Input, Index) ->
  p(Input, Index, 'ReturnToken', fun(I,D) -> (p_seq([p_string(<<"return">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('ReturnToken', Node, Idx) end).

-spec 'SuperToken'(input(), index()) -> parse_result().
'SuperToken'(Input, Index) ->
  p(Input, Index, 'SuperToken', fun(I,D) -> (p_seq([p_string(<<"super">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('SuperToken', Node, Idx) end).

-spec 'SwitchToken'(input(), index()) -> parse_result().
'SwitchToken'(Input, Index) ->
  p(Input, Index, 'SwitchToken', fun(I,D) -> (p_seq([p_string(<<"switch">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('SwitchToken', Node, Idx) end).

-spec 'ThisToken'(input(), index()) -> parse_result().
'ThisToken'(Input, Index) ->
  p(Input, Index, 'ThisToken', fun(I,D) -> (p_seq([p_string(<<"this">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('ThisToken', Node, Idx) end).

-spec 'ThrowToken'(input(), index()) -> parse_result().
'ThrowToken'(Input, Index) ->
  p(Input, Index, 'ThrowToken', fun(I,D) -> (p_seq([p_string(<<"throw">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('ThrowToken', Node, Idx) end).

-spec 'TrueToken'(input(), index()) -> parse_result().
'TrueToken'(Input, Index) ->
  p(Input, Index, 'TrueToken', fun(I,D) -> (p_seq([p_string(<<"true">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('TrueToken', Node, Idx) end).

-spec 'TryToken'(input(), index()) -> parse_result().
'TryToken'(Input, Index) ->
  p(Input, Index, 'TryToken', fun(I,D) -> (p_seq([p_string(<<"try">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('TryToken', Node, Idx) end).

-spec 'TypeofToken'(input(), index()) -> parse_result().
'TypeofToken'(Input, Index) ->
  p(Input, Index, 'TypeofToken', fun(I,D) -> (p_seq([p_string(<<"typeof">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('TypeofToken', Node, Idx) end).

-spec 'VarToken'(input(), index()) -> parse_result().
'VarToken'(Input, Index) ->
  p(Input, Index, 'VarToken', fun(I,D) -> (p_seq([p_string(<<"var">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('VarToken', Node, Idx) end).

-spec 'VoidToken'(input(), index()) -> parse_result().
'VoidToken'(Input, Index) ->
  p(Input, Index, 'VoidToken', fun(I,D) -> (p_seq([p_string(<<"void">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('VoidToken', Node, Idx) end).

-spec 'WhileToken'(input(), index()) -> parse_result().
'WhileToken'(Input, Index) ->
  p(Input, Index, 'WhileToken', fun(I,D) -> (p_seq([p_string(<<"while">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('WhileToken', Node, Idx) end).

-spec 'WithToken'(input(), index()) -> parse_result().
'WithToken'(Input, Index) ->
  p(Input, Index, 'WithToken', fun(I,D) -> (p_seq([p_string(<<"with">>), p_not(fun 'IdentifierPart'/2)]))(I,D) end, fun(Node, Idx) ->transform('WithToken', Node, Idx) end).

-spec '__'(input(), index()) -> parse_result().
'__'(Input, Index) ->
  p(Input, Index, '__', fun(I,D) -> (p_zero_or_more(p_choose([fun 'WhiteSpace'/2, fun 'LineTerminatorSequence'/2, fun 'Comment'/2])))(I,D) end, fun(Node, Idx) ->transform('__', Node, Idx) end).

-spec '_'(input(), index()) -> parse_result().
'_'(Input, Index) ->
  p(Input, Index, '_', fun(I,D) -> (p_zero_or_more(p_choose([fun 'WhiteSpace'/2, fun 'MultiLineCommentNoLineTerminator'/2])))(I,D) end, fun(Node, Idx) ->transform('_', Node, Idx) end).

-spec 'EOS'(input(), index()) -> parse_result().
'EOS'(Input, Index) ->
  p(Input, Index, 'EOS', fun(I,D) -> (p_choose([p_seq([fun '__'/2, p_string(<<";">>)]), p_seq([fun '_'/2, p_optional(fun 'SingleLineComment'/2), fun 'LineTerminatorSequence'/2]), p_seq([fun '__'/2, fun 'EOF'/2])]))(I,D) end, fun(Node, Idx) ->transform('EOS', Node, Idx) end).

-spec 'EOF'(input(), index()) -> parse_result().
'EOF'(Input, Index) ->
  p(Input, Index, 'EOF', fun(I,D) -> (p_not(p_anything()))(I,D) end, fun(Node, Idx) ->transform('EOF', Node, Idx) end).


transform(_,Node,_Index) -> Node.
-file("peg_includes.hrl", 1).
%% -type index() :: {{line, pos_integer()}, {column, pos_integer()}}.
-type input() :: binary().
-type parse_failure() :: {fail, term()}.
-type parse_success() :: {term(), input(), index()}.
-type parse_result() :: parse_failure() | parse_success().
-type parse_fun() :: fun((input(), index()) -> parse_result()).
-type xform_fun() :: fun((input(), index()) -> term()).

-spec p(input(), index(), atom(), parse_fun(), xform_fun()) -> parse_result().
p(Inp, StartIndex, Name, ParseFun, TransformFun) ->
  case get_memo(StartIndex, Name) of      % See if the current reduction is memoized
    {ok, Memo} -> %Memo;                     % If it is, return the stored result
      Memo;
    _ ->                                        % If not, attempt to parse
      Result = case ParseFun(Inp, StartIndex) of
        {fail,_} = Failure ->                       % If it fails, memoize the failure
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

-ifdef(p_eof).
-spec p_eof() -> parse_fun().
p_eof() ->
  fun(<<>>, Index) -> {eof, [], Index};
     (_, Index) -> {fail, {expected, eof, Index}} end.
-endif.

-ifdef(p_optional).
-spec p_optional(parse_fun()) -> parse_fun().
p_optional(P) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {fail,_} -> {[], Input, Index};
        {_, _, _} = Success -> Success
      end
  end.
-endif.

-ifdef(p_not).
-spec p_not(parse_fun()) -> parse_fun().
p_not(P) ->
  fun(Input, Index)->
      case P(Input,Index) of
        {fail,_} ->
          {[], Input, Index};
        {Result, _, _} -> {fail, {expected, {no_match, Result},Index}}
      end
  end.
-endif.

-ifdef(p_assert).
-spec p_assert(parse_fun()) -> parse_fun().
p_assert(P) ->
  fun(Input,Index) ->
      case P(Input,Index) of
        {fail,_} = Failure-> Failure;
        _ -> {[], Input, Index}
      end
  end.
-endif.

-ifdef(p_seq).
-spec p_seq([parse_fun()]) -> parse_fun().
p_seq(P) ->
  fun(Input, Index) ->
      p_all(P, Input, Index, [])
  end.

-spec p_all([parse_fun()], input(), index(), [term()]) -> parse_result().
p_all([], Inp, Index, Accum ) -> {lists:reverse( Accum ), Inp, Index};
p_all([P|Parsers], Inp, Index, Accum) ->
  case P(Inp, Index) of
    {fail, _} = Failure -> Failure;
    {Result, InpRem, NewIndex} -> p_all(Parsers, InpRem, NewIndex, [Result|Accum])
  end.
-endif.

-ifdef(p_choose).
-spec p_choose([parse_fun()]) -> parse_fun().
p_choose(Parsers) ->
  fun(Input, Index) ->
      p_attempt(Parsers, Input, Index, none)
  end.

-spec p_attempt([parse_fun()], input(), index(), none | parse_failure()) -> parse_result().
p_attempt([], _Input, _Index, Failure) -> Failure;
p_attempt([P|Parsers], Input, Index, FirstFailure)->
  case P(Input, Index) of
    {fail, _} = Failure ->
      case FirstFailure of
        none -> p_attempt(Parsers, Input, Index, Failure);
        _ -> p_attempt(Parsers, Input, Index, FirstFailure)
      end;
    Result -> Result
  end.
-endif.

-ifdef(p_zero_or_more).
-spec p_zero_or_more(parse_fun()) -> parse_fun().
p_zero_or_more(P) ->
  fun(Input, Index) ->
      p_scan(P, Input, Index, [])
  end.
-endif.

-ifdef(p_one_or_more).
-spec p_one_or_more(parse_fun()) -> parse_fun().
p_one_or_more(P) ->
  fun(Input, Index)->
      Result = p_scan(P, Input, Index, []),
      case Result of
        {[_|_], _, _} ->
          Result;
        _ ->
          {fail, {expected, Failure, _}} = P(Input,Index),
          {fail, {expected, {at_least_one, Failure}, Index}}
      end
  end.
-endif.

-ifdef(p_label).
-spec p_label(atom(), parse_fun()) -> parse_fun().
p_label(Tag, P) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {fail,_} = Failure ->
           Failure;
        {Result, InpRem, NewIndex} ->
          {{Tag, Result}, InpRem, NewIndex}
      end
  end.
-endif.

-ifdef(p_scan).
-spec p_scan(parse_fun(), input(), index(), [term()]) -> parse_result().
p_scan(_, [], Index, Accum) -> {lists:reverse( Accum ), [], Index};
p_scan(P, Inp, Index, Accum) ->
  case P(Inp, Index) of
    {fail,_} -> {lists:reverse(Accum), Inp, Index};
    {Result, InpRem, NewIndex} -> p_scan(P, InpRem, NewIndex, [Result | Accum])
  end.
-endif.

-ifdef(p_string).
-spec p_string(binary()) -> parse_fun().
p_string(S) ->
    Length = erlang:byte_size(S),
    fun(Input, Index) ->
      try
          <<S:Length/binary, Rest/binary>> = Input,
          {S, Rest, p_advance_index(S, Index)}
      catch
          error:{badmatch,_} -> {fail, {expected, {string, S}, Index}}
      end
    end.
-endif.

-ifdef(p_anything).
-spec p_anything() -> parse_fun().
p_anything() ->
  fun(<<>>, Index) -> {fail, {expected, any_character, Index}};
     (Input, Index) when is_binary(Input) ->
          <<C/utf8, Rest/binary>> = Input,
          {<<C/utf8>>, Rest, p_advance_index(<<C/utf8>>, Index)}
  end.
-endif.

-ifdef(p_charclass).
-spec p_charclass(string() | binary()) -> parse_fun().
p_charclass(Class) ->
    {ok, RE} = re:compile(Class, [unicode, dotall]),
    fun(Inp, Index) ->
            case re:run(Inp, RE, [anchored]) of
                {match, [{0, Length}|_]} ->
                    {Head, Tail} = erlang:split_binary(Inp, Length),
                    {Head, Tail, p_advance_index(Head, Index)};
                _ -> {fail, {expected, {character_class, binary_to_list(Class)}, Index}}
            end
    end.
-endif.

-ifdef(p_regexp).
-spec p_regexp(binary()) -> parse_fun().
p_regexp(Regexp) ->
    {ok, RE} = re:compile(Regexp, [unicode, dotall, anchored]),
    fun(Inp, Index) ->
        case re:run(Inp, RE) of
            {match, [{0, Length}|_]} ->
                {Head, Tail} = erlang:split_binary(Inp, Length),
                {Head, Tail, p_advance_index(Head, Index)};
            _ -> {fail, {expected, {regexp, binary_to_list(Regexp)}, Index}}
        end
    end.
-endif.

-ifdef(line).
-spec line(index() | term()) -> pos_integer() | undefined.
line({{line,L},_}) -> L;
line(_) -> undefined.
-endif.

-ifdef(column).
-spec column(index() | term()) -> pos_integer() | undefined.
column({_,{column,C}}) -> C;
column(_) -> undefined.
-endif.

-type index() ::tuple().
-spec p_advance_index(input() | unicode:charlist() | pos_integer(), index()) -> index().
p_advance_index(MatchedInput, Index) when is_list(MatchedInput) orelse is_binary(MatchedInput)-> % strings
  lists:foldl(fun p_advance_index/2, Index, unicode:characters_to_list(MatchedInput));
p_advance_index(MatchedInput, Index) when is_integer(MatchedInput) -> % single characters
  {{line, Line}, {column, Col}} = Index,
  case MatchedInput of
    $\n -> {{line, Line+1}, {column, 1}};
    _ -> {{line, Line}, {column, Col+1}}
  end.
