%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for the bytecode generator
%%%      Direct port of https://github.com/pegjs/pegjs/blob/master/spec/unit/parser.spec.js
%%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%%%
%%%-----------------------------------------------------------------------------
-module(pegjs2_parser_tests).
-author(<<"dmitriid">>).

%%%_* Includes =================================================================
-include_lib("eunit/include/eunit.hrl").
-include_lib("../src/pegjs2.hrl").
-include("pegjs_test.hrl").

%%%_* Defines ==================================================================
-define(FAIL(X), fun(Y) -> ?assertMatch(X, Y) end).

%%%_* Tests ====================================================================

parser_test_() ->
  [
  %% Canonical Grammar is "a = \"abcd\"; b = \"efgh\"; c = \"ijkl\";"
    run_test( "parses Grammar"
            , "\na = \"abcd\";\n"
            , #entry{type = <<"grammar">>, initializer = [], rules = [ruleA()]}
            )
  , run_test( "parses Grammar"
            , "\na = \"abcd\";\nb = \"efgh\";\nc = \"ijkl\";\n"
            , #entry{type = <<"grammar">>, initializer = [], rules = [ruleA(), ruleB(), ruleC()]}
            )
  , run_test( "parses Grammar"
            , "\n{ code };\na = \"abcd\";\n"
            , #entry{type = <<"grammar">>, initializer = initializer(), rules = [ruleA()]}
            )
  %% Canonical Initializer is "{ code }"
  , run_test( "parses Initializer"
            , "{ code };start = \"abcd\""
            , #entry{type = <<"grammar">>, initializer = initializer(), rules = [ruleStart()]}
            )
%%   %% Canonical Rule is "a = \"abcd\";"
  , run_test( "parses Rule"
            , "start\n=\n\"abcd\";"
            , oneRuleGrammar(literalAbcd())
            )
  , run_test( "parses Rule"
            , "start\n\"start rule\"\n=\n\"abcd\";"
            , oneRuleGrammar(named())
            )
  %% Canonical Expression is "\"abcd\""
  , run_test( "parses Expression"
            , "start = \"abcd\" / \"efgh\" / \"ijkl\""
            , oneRuleGrammar(choice())
            )
  %% Canonical ChoiceExpression is "\"abcd\" / \"efgh\" / \"ijkl\""
  , run_test( "parses ChoiceExpression"
            , "start = \"abcd\" { code }"
            , oneRuleGrammar(actionAbcd())
            )
  , run_test( "parses ChoiceExpression"
            , "start = \"abcd\" { code }\n/\n\"efgh\" { code }"
            , oneRuleGrammar(choice2())
            )
  , run_test( "parses ChoiceExpression"
            , "start = \"abcd\" { code }\n/\n\"efgh\" { code }\n/\n\"ijkl\" { code }\n/\n\"mnop\" { code }"
            , oneRuleGrammar(choice4())
            )
  %% Canonical ActionExpression is "\"abcd\" { code }"
  , run_test( "parses ActionExpression"
            , "start = \"abcd\" \"efgh\" \"ijkl\""
            , oneRuleGrammar(sequence())
            )
  , run_test( "parses ActionExpression"
            , "start = \"abcd\" \"efgh\" \"ijkl\"\n{ code }"
            , oneRuleGrammar(actionSequence())
            )
  %% Canonical SequenceExpression is "\"abcd\" \"efgh\" \"ijkl\""
  , run_test( "parses SequenceExpression"
            , "start = a:\"abcd\""
            , oneRuleGrammar(labeledAbcd())
            )
  , run_test( "parses SequenceExpression"
            , "start = a:\"abcd\"\nb:\"efgh\""
            , oneRuleGrammar(sequence2())
            )
  , run_test( "parses SequenceExpression"
            , "start = a:\"abcd\"\nb:\"efgh\"\nc:\"ijkl\"\nd:\"mnop\""
            , oneRuleGrammar(sequence4())
            )
  %% Canonical LabeledExpression is "a:\"abcd\""
  , run_test( "parses LabeledExpression"
            , "start = a\n:\n!\"abcd\""
            , oneRuleGrammar(labeledSimpleNot())
            )
  , run_test( "parses LabeledExpression"
            , "start = !\"abcd\""
            , oneRuleGrammar(simpleNotAbcd())
            )
  %% Canonical PrefixedExpression is "!\"abcd\""
  , run_test( "parses PrefixedExpression"
            , "start = !\n\"abcd\"?"
            , oneRuleGrammar(simpleNotOptional())
            )
  , run_test( "parses PrefixedExpression"
            , "start = \"abcd\"?"
            , oneRuleGrammar(optional())
            )
  %% Canonical PrefixedOperator is "!"
  , run_test( "parses PrefixedOperator"
            , "start = $\"abcd\"?"
            , oneRuleGrammar(textOptional())
            )
  , run_test( "parses PrefixedOperator"
            , "start = &\"abcd\"?"
            , oneRuleGrammar(simpleAndOptional())
            )
  , run_test( "parses PrefixedOperator"
            , "start = !\"abcd\"?"
            , oneRuleGrammar(simpleNotOptional())
            )
  %% Canonical SuffixedExpression is "\"ebcd\"?"
  , run_test( "parses SuffixedExpression"
            , "start = \"abcd\"\n?"
            , oneRuleGrammar(optional())
            )
  , run_test( "parses SuffixedExpression"
            , "start = \"abcd\""
            , oneRuleGrammar(literalAbcd())
            )
  %% Canonical SuffixedOperator is "?"
  , run_test( "parses SuffixedExpression"
            , "start = \"abcd\"?"
            , oneRuleGrammar(optional())
            )
  , run_test( "parses SuffixedExpression"
            , "start = \"abcd\"*"
            , oneRuleGrammar(zeroOrMore())
            )
  , run_test( "parses SuffixedExpression"
            , "start = \"abcd\"+"
            , oneRuleGrammar(oneOrMore())
            )
  %% Canonical PrimaryExpression is "\"abcd\""
  , run_test( "parses SuffixedExpression"
            , "start = \"abcd\""
            , trivialGrammar()
            )
%% TODO: garmmar should return proper parts
%%   , run_test( "parses SuffixedExpression"
%%             , "start = [a-d]"
%%             , classGrammar([[<<"a">>, <<"d">>]], false, false, <<"[a-d]">>)
%%             )
  , run_test( "parses SuffixedExpression"
            , "start = ."
            , anyGrammar()
            )
  , run_test( "parses SuffixedExpression"
            , "start = a"
            , ruleRefGrammar(<<"a">>)
            )
  , run_test( "parses SuffixedExpression"
            , "start = &{ code }"
            , oneRuleGrammar(semanticAnd())
            )
  , run_test( "parses SuffixedExpression"
            , "start = (\n\"abcd\"\n)"
            , trivialGrammar()
            )
  %% Canonical RuleReferenceExpression is "a"
  , run_test( "parses RuleReferenceExpression"
            , "start = a"
            , ruleRefGrammar(<<"a">>)
            )
  , run_test( "parses RuleReferenceExpression"
            , "start = a\n="
            , ?FAIL({error, _})
            )
  , run_test( "parses RuleReferenceExpression"
            , "start = a\n\"abcd\"\n="
            , ?FAIL({error, _})
            )
  %% Canonical SemanticPredicateExpression is "!{ code }"
  , run_test( "parses SemanticPredicateExpression"
            , "start = !\n{ code }"
            , oneRuleGrammar(semanticNot())
            )
  %% Canonical SemanticPredicateOperator is "!"
  , run_test( "parses SemanticPredicateExpression"
            , "start = &{ code }"
            , oneRuleGrammar(semanticAnd())
            )
  , run_test( "parses SemanticPredicateExpression"
            , "start = !{ code }"
            , oneRuleGrammar(semanticNot())
            )

  %% The SourceCharacter rule is not tested

  %% Canonical WhiteSpace is " "
  , run_test("parses SemanticPredicateExpression"
            , "start =\t\"abcd\""
            , trivialGrammar()
            )
  , run_test("parses SemanticPredicateExpression"
            , <<"start =", 16#0B, "\"abcd\"">>
            , trivialGrammar()
            )
  , run_test("parses SemanticPredicateExpression"
            , "start =\v\"abcd\""                 %% same as above, not supported
            , trivialGrammar()                    %%  in original peg.js
            )
  , run_test("parses SemanticPredicateExpression"
            , "start =\f\"abcd\""
            , trivialGrammar()
            )
  , run_test("parses SemanticPredicateExpression"
            , "start = \"abcd\""
            , trivialGrammar()
            )
  , run_test("parses SemanticPredicateExpression"
            , "start = \x{A0}\"abcd\""
            , trivialGrammar()
            )
  , run_test("parses SemanticPredicateExpression"
            , "start =\x{FEFF}\"abcd\""
            , trivialGrammar()
            )
  , run_test( "parses SemanticPredicateExpression"
            , "start = \x{1680}\"abcd\""
            , trivialGrammar()
            )
  %% Canonical LineTerminator is "\n"
  , run_test( "parses LineTerminator"
            , "start = \"\n\""
            , ?FAIL({error, _})
            )
  , run_test( "parses LineTerminator"
            , "start = \"\r\""
            , ?FAIL({error, _})
            )
  , run_test( "parses LineTerminator"
            , "start = \x{2028}"
            , ?FAIL({error, _})
            )
  , run_test( "parses LineTerminator"
            , "start = \x{2029}"
            , ?FAIL({error, _})
            )
  %% Canonical LineTerminatorSequence is "\r\n"
  , run_test("parses LineTerminatorSequence"
            , "start =\n\"abcd\""
            , trivialGrammar()
            )
  , run_test( "parses LineTerminatorSequence"
            , "start =\r\n\"abcd\""
            , trivialGrammar()
            )
  , run_test("parses LineTerminatorSequence"
            , "start =\r\"abcd\""
            , trivialGrammar()
            )
  , run_test("parses LineTerminatorSequence"
            , "start =\x{2028}\"abcd\""
            , trivialGrammar()
            )
  , run_test("parses LineTerminatorSequence"
            , "start =\x{2029}\"abcd\""
            , trivialGrammar()
            )
  %% Canonical Comment is "/* comment */"
  , run_test("parses Comment"
            , "start =// comment\n\"abcd\""
            , trivialGrammar()
            )
  , run_test("parses Comment"
            , "start =/* comment */\"abcd\""
            , trivialGrammar()
            )
  %% Canonical MultiLineComment is "/* comment */
  , run_test("parses MultiLineComment"
            , "start =/**/\"abcd\""
            , trivialGrammar()
            )
  , run_test("parses MultiLineComment"
            , "start =/*a*/\"abcd\""
            , trivialGrammar()
            )
  , run_test("parses MultiLineComment"
            , "start =/*abc*/\"abcd\""
            , trivialGrammar()
            )
  , run_test("parses MultiLineComment"
            , "start =/**/*/\"abcd\""
            , ?FAIL({error, _})
            )
  %% Canonical MultiLineCommentNoLineTerminator is "/* comment */
  , run_test("parses MultiLineCommentNoLineTerminator"
            , "a = \"abcd\"/**/\r\nb = \"efgh\""
            , twoRuleGrammar()
            )
  , run_test("parses MultiLineCommentNoLineTerminator"
            , "a = \"abcd\"/*a*/\r\nb = \"efgh\""
            , twoRuleGrammar()
            )
  , run_test("parses MultiLineCommentNoLineTerminator"
            , "a = \"abcd\"/*abc*/\r\nb = \"efgh\""
            , twoRuleGrammar()
            )
  , run_test("parses MultiLineCommentNoLineTerminator"
            , "a = \"abcd\"/**/*/\r\nb = \"efgh\""
            , ?FAIL({error, _})
            )
  , run_test("parses MultiLineCommentNoLineTerminator"
            , "a = \"abcd\"/*\n*/\r\nb = \"efgh\""
            , ?FAIL({error, _})
            )
  %% Canonical SingleLineComment is "// comment"
  , run_test("parses SingleLineComment"
            , "start =//\n\"abcd\""
            , trivialGrammar()
            )
  , run_test("parses SingleLineComment"
            , "start =//a\n\"abcd\""
            , trivialGrammar()
            )
  , run_test("parses SingleLineComment"
            , "start =//abc\n\"abcd\""
            , trivialGrammar()
            )
  , run_test("parses SingleLineComment"
            , "start =//\n@\n\"abcd\""
            , ?FAIL({error, _})
            )
  %% Canonical Identifier is "a".
  , run_test("parses Identifier"
            , "start = a:\"abcd\""
            , oneRuleGrammar(labeledAbcd())
            )
  %% Canonical IdentifierName is "a".
  , run_test("parses IdentifierName"
            , "start = a"
            , ruleRefGrammar(<<"a">>)
            )
  , run_test("parses IdentifierName"
            , "start = ab"
            , ruleRefGrammar(<<"ab">>)
            )
  , run_test("parses IdentifierName"
            , "start = abcd"
            , ruleRefGrammar(<<"abcd">>)
            )
  %% Canonical IdentifierStart is "a".
  , run_test("parses IdentifierStart"
            , "start = a"
            , ruleRefGrammar(<<"a">>)
            )
  , run_test("parses IdentifierStart"
            , "start = $"
            , ruleRefGrammar(<<"$">>)
            )
  , run_test("parses IdentifierStart"
            , "start = _"
            , ruleRefGrammar(<<"_">>)
            )
  , run_test("parses IdentifierStart"
            , "start = \\u0061"
            , ruleRefGrammar(<<"a">>)
            )
  %% Canonical IdentifierPart is "a".
  , run_test("parses IdentifierPart"
            , "start = aa"
            , ruleRefGrammar(<<"aa">>)
            )
  , run_test("parses IdentifierPart"
            , "start = a\\u0300"
            , ruleRefGrammar(unicode:characters_to_binary("a\x{0300}"))
            )
  , run_test("parses IdentifierPart"
            , "start = a0"
            , ruleRefGrammar(<<"a0">>)
            )
  , run_test("parses IdentifierPart"
            , "start = a\\u203F"
            , ruleRefGrammar(unicode:characters_to_binary("a\x{203F}"))
            )
  , run_test("parses IdentifierPart"
            , "start = a\\u200C"
            , ruleRefGrammar(unicode:characters_to_binary("a\x{200C}"))
            )
  , run_test("parses IdentifierPart"
            , "start = a\\u200D"
            , ruleRefGrammar(unicode:characters_to_binary("a\x{200D}"))
            )

  %% Unicode rules and reserved word rules are not tested.

  %% Canonical LiteralMatcher is "\"abcd\""
  , run_test("parses LiteralMatcher"
            , "start = \"abcd\""
            , literalGrammar(<<"abcd">>, false)
            )
  , run_test("parses LiteralMatcher"
            , "start = \"abcd\"i"
            , literalGrammar(<<"abcd">>, true)
            )
  %% Canonical StringLiteral is "\"abcd\""
  , run_test("parses StringLiteral"
            , "start = \"\""
            , literalGrammar(<<"">>, false)
            )
  , run_test("parses StringLiteral"
            , "start = \"a\""
            , literalGrammar(<<"a">>, false)
            )
  , run_test("parses StringLiteral"
            , "start = \"abc\""
            , literalGrammar(<<"abc">>, false)
            )
  , run_test("parses StringLiteral"
            , "start = ''"
            , literalGrammar(<<"">>, false)
            )
  , run_test("parses StringLiteral"
            , "start = 'a'"
            , literalGrammar(<<"a">>, false)
            )
  , run_test("parses StringLiteral"
            , "start = 'abc'"
            , literalGrammar(<<"abc">>, false)
            )
  %% Canonical DoubleStringCharacter is "a"
  , run_test("parses DoubleStringCharacter"
            , "start = \"a\""
            , literalGrammar(<<"a">>, false)
            )
  , run_test("parses DoubleStringCharacter"
            , "start = \"\\n\""
            , literalGrammar(<<"\n">>, false)
            )
  , run_test("parses DoubleStringCharacter"
            , "start = \"\\\n\""
            , literalGrammar(<<"">>, false)
            )
  , run_test("parses DoubleStringCharacter"
            , "start = \"\"\""
            , ?FAIL({error, _})
            )
  , run_test("parses DoubleStringCharacter"
            , "start = \"\\\""
            , ?FAIL({error, _})
            )
  , run_test("parses DoubleStringCharacter"
            , "start = \"\n\""
            , ?FAIL({error, _})
            )
  %% Canonical SingleStringCharacter is "a"
  , run_test("parses SingleStringCharacter"
            , "start = 'a'"
            , literalGrammar(<<"a">>, false)
            )
  , run_test("parses SingleStringCharacter"
            , "start = '\\n\'"
            , literalGrammar(<<"\n">>, false)
            )
  , run_test("parses SingleStringCharacter"
            , "start = '\\\n'"
            , literalGrammar(<<"">>, false)
            )
  , run_test("parses SingleStringCharacter"
            , "start = '''"
            , ?FAIL({error, _})
            )
  , run_test("parses SingleStringCharacter"
            , "start = '\\'"
            , ?FAIL({error, _})
            )
  , run_test("parses SingleStringCharacter"
            , "start = '\n'"
            , ?FAIL({error, _})
            )
  %% Canonical CharacterClassMatcher is "[a-d]"
  , run_test("parses CharacterClassMatcher"
            , "start = []"
            , classGrammar([], false, false, <<"[]">>)
            )
%% TODO: Fix parts in regexps
%%   , run_test("parses CharacterClassMatcher"
%%             , "start = [a-d]"
%%             , classGrammar([[<<"a">>, <<"d">>]], false, false, <<"[a-d]">>)
%%             )
  , run_test("parses CharacterClassMatcher"
            , "start = [a]"
            , classGrammar([<<"a">>], false, false, <<"[a]">>)
            )
%% TODO: Fix parts in regexps
%%   , run_test("parses CharacterClassMatcher"
%%             , "start = [a-de-hi-l]"
%%             , classGrammar( [[<<"a">>, <<"d">>], [<<"e">>, <<"h">>], [<<"i">>, <<"l">>]]
%%                           , false
%%                           , false
%%                           , <<"[a-de-hi-l]">>
%%                           )
%%             )
%%   , run_test("parses CharacterClassMatcher"
%%             , "start = [^a-d]"
%%             , classGrammar([[<<"a">>, <<"d">>]], true, false, <<"[^a-d]">>)
%%             )
%%   , run_test("parses CharacterClassMatcher"
%%             , "start = [a-d]i"
%%             , classGrammar([[<<"a">>, <<"d">>]], false, true, <<"[a-d]i">>)
%%             )
%% TODO: Fix empty classes
%%   , run_test("parses CharacterClassMatcher"
%%             , "start = [\\\n]"
%%             , classGrammar([], false, true, <<"[\\\n]">>)
%%             )
  %% Canonical ClassCharacterRange is "a-d"
%% TODO: Fix parts in regexps
%%   , run_test("parses ClassCharacterRange"
%%             , "start = [a-d]"
%%             , classGrammar([[<<"a">>, <<"d">>]], false, false, <<"[a-d]">>)
%%             )
%%   , run_test("parses ClassCharacterRange"
%%             , "start = [a-a]"
%%             , classGrammar([[<<"a">>, <<"a">>]], false, false, <<"[a-a]">>)
%%             )
%% TODO: Fix range checks
%%   , run_test("parses ClassCharacterRange"
%%             , "start = [b-a]"
%%             , ?FAIL({error, _})
%%             )
  %% Canonical ClassCharacter is "a"
  , run_test("parses ClassCharacter"
            , "start = [a]"
            , classGrammar([<<"a">>], false, false, <<"[a]">>)
            )
%% TODO: Fix parts in regexps
%%   , run_test("parses ClassCharacter"
%%             , "start = [\\n]"
%%             , classGrammar([<<"\n">>], false, false, <<"[\\n]">>)
%%             )
%% TODO: Remove empty character ranges
%%   , run_test("parses ClassCharacter"
%%             , "start = [\\\n]"
%%             , classGrammar([], false, false, <<"[a]">>)
%%             )
  , run_test("parses ClassCharacter"
            , "start = []]"
            , ?FAIL({error, _})
            )
  , run_test("parses ClassCharacter"
            , "start = [\\]"
            , ?FAIL({error, _})
            )
  , run_test("parses ClassCharacter"
            , "start = [\n]"
            , ?FAIL({error, _})
            )
  %% Canonical LineContinuation is "\\\n"
  , run_test("parses LineContinuation"
            , "start = \"\\\r\n\""
            , literalGrammar(<<"">>, false)
            )
  %% Canonical EscapeSequence is "n"
  , run_test("parses EscapeSequence"
            , "start = \"\\n\""
            , literalGrammar(<<"\n">>, false)
            )
  , run_test("parses EscapeSequence"
            , "start = \"\\0\""
            , literalGrammar(unicode:characters_to_binary("\x{00}"), false)
            )
  , run_test("parses EscapeSequence"
            , "start = \"\\xFF\""
            , literalGrammar(unicode:characters_to_binary("\x{FF}"), false)
            )
  , run_test("parses EscapeSequence"
            , "start = \"\\uFFFF\""
            , literalGrammar(unicode:characters_to_binary([16#FFFF]), false)
            )
  , run_test("parses EscapeSequence"
            , "start = \"\\09\""
            , ?FAIL({error, _})
            )
  %% Canonical CharacterEscapeSequence is "n"
  , run_test("parses EscapeSequence"
            , "start = \"\\n\""
            , literalGrammar(<<"\n">>, false)
            )
  , run_test("parses EscapeSequence"
            , "start = \"\\a\""
            , literalGrammar(<<"a">>, false)
            )
  %% Canonical SingleEscapeCharacter is "n"
  , run_test("parses SingleEscapeCharacter"
            , "start = \"\\'\""
            , literalGrammar(<<"'">>, false)
            )
  , run_test("parses SingleEscapeCharacter"
            , "start = \"\\\"\""
            , literalGrammar(<<"\"">>, false)
            )
  , run_test("parses SingleEscapeCharacter"
            , "start = \"\\\\\""
            , literalGrammar(<<"\\">>, false)
            )
  , run_test("parses SingleEscapeCharacter"
            , "start = \"\\b\""
            , literalGrammar(<<"\b">>, false)
            )
  , run_test("parses SingleEscapeCharacter"
            , "start = \"\\f\""
            , literalGrammar(<<"\f">>, false)
            )
  , run_test("parses SingleEscapeCharacter"
            , "start = \"\\n\""
            , literalGrammar(<<"\n">>, false)
            )
  , run_test("parses SingleEscapeCharacter"
            , "start = \"\\r\""
            , literalGrammar(<<"\r">>, false)
            )
  , run_test("parses SingleEscapeCharacter"
            , "start = \"\\t\""
            , literalGrammar(<<"\t">>, false)
            )
  , run_test("parses SingleEscapeCharacter"
            , "start = \"\\v\""
            , literalGrammar(<<"\v">>, false)
            )
  %% Canonical NonEscapeCharacter is "a"
  , run_test("parses NonEscapeCharacter"
            , "start = \"\\a\""
            , literalGrammar(<<"a">>, false)
            )

  %% The negative predicate is impossible to test with PEG.js grammar structure.

  %% The EscapeCharacter rule is impossible to test with PEG.js grammar structure.

  %% Canonical HexEscapeSequence is "xFF"
  , run_test("parses HexEscapeSequence"
            , "start = \"\\xFF\""
            , literalGrammar(unicode:characters_to_binary([16#FF]), false)
            )
  %% Canonical UnicodeEscapeSequence is "uFFFF"
  , run_test("parses UnicodeEscapeSequence"
            , "start = \"\\uFFFF\""
            , literalGrammar(unicode:characters_to_binary([16#FFFF]), false)
            )

  %% Digit rules are not tested

  %% Canonical AnyMatcher is "."
  , run_test("parses AnyMatcher"
            , "start = ."
            , anyGrammar()
            )
  %% Canonical CodeBlock is "{ code }"
  , run_test("parses CodeBlock"
            , "start = \"abcd\" {a}"
            , actionGrammar(<<"a">>)
            )
  %% Canonical Code is " code "
  , run_test("parses Code"
            , "start = \"abcd\" {abc}"
            , actionGrammar(<<"abc">>)
            )
  , run_test("parses Code"
            , "start = \"abcd\" {{a}}"
            , actionGrammar(<<"{a}">>)
            )
%% TODO: Fix multiple brackets
%%   , run_test("parses Code"
%%             , "start = \"abcd\" {{a}{b}{c}}"
%%             , actionGrammar(<<"{a}{b}{c}">>)
%%             )
  , run_test("parses Code"
            , "start = \"abcd\" {{}"
            , ?FAIL({error, _})
            )
  , run_test("parses Code"
            , "start = \"abcd\" {}}"
            , ?FAIL({error, _})
            )

  %% Unicode character category rules and token rules are not tested

  %% Canonical __ is "\n"
  , run_test("parses __"
            , "start =\"abcd\""
            , trivialGrammar()
            )
  , run_test("parses __"
            , "start = \"abcd\""
            , trivialGrammar()
            )
  , run_test("parses __"
            , "start =\r\n\"abcd\""
            , trivialGrammar()
            )
  , run_test("parses __"
            , "start =/* comment */\"abcd\""
            , trivialGrammar()
            )
  %% Canonical _ is " "
  , run_test("parses _"
            , "a = \"abcd\"\r\nb = \"efgh\""
            , twoRuleGrammar()
            )
  , run_test("parses _"
            , "a = \"abcd\" \r\nb = \"efgh\""
            , twoRuleGrammar()
            )
  , run_test("parses _"
            , "a = \"abcd\"/* comment */\r\nb = \"efgh\""
            , twoRuleGrammar()
            )
  %% Canonical EOS is ";"
  , run_test("parses EOS"
            , "a = \"abcd\";\nb = \"efgh\""
            , twoRuleGrammar()
            )
  , run_test("parses EOS"
            , "a = \"abcd\" \r\nb = \"efgh\""
            , twoRuleGrammar()
            )
  , run_test("parses EOS"
            , "a = \"abcd\" // comment\r\nb = \"efgh\""
            , twoRuleGrammar()
            )
  , run_test("parses EOS"
            , "a = \"abcd\"\nb = \"efgh\""
            , twoRuleGrammar()
            )
  %% Canonical EOF is the end of input
  , run_test("parses EOS"
            , "start = \"abcd\"\n"
            , trivialGrammar()
            )
  ].

%%%_* Internal =================================================================

run_test(Description, Grammar0, Expected) ->
  Grammar = unicode:characters_to_binary(Grammar0),
  FullDescription = << (list_to_binary(Description))/binary
                    , "\n"
                    , Grammar/binary
                    , "\n\n">>,
  { Description
  , fun() ->
      io:format(user, "~ts~n", [FullDescription]),
      Result = pegjs_util:chain( [ fun pegjs2_analyze:analyze/1
                                 ]
                               , [{input, Grammar}]),
      do_run_test(Result, Expected)
    end
  }.

do_run_test(Result, F) when is_function(F) ->
  ?whenFail(io:format(user, "Expected condition failed ~n"
                            "Got result ~n~p~n", [Result])
           , F(Result)
           );
do_run_test(Result, Expected) ->
  ActualGrammar0 = Result#analysis.grammar,
  ActualGrammar = reset_indices(ActualGrammar0),
  ?whenFail(io:format(user, "Expected result ~n~p~n"
                            "Got result ~n~p~n", [Expected, ActualGrammar])
           , ?assertEqual(Expected, ActualGrammar)
           ).

%%%_* Helpers/definitions ======================================================

literalAbcd() -> 
  #entry{type = <<"literal">>, value = <<"abcd">>, ignore_case = false}.


literalEfgh() -> 
  #entry{type = <<"literal">>, value = <<"efgh">>, ignore_case = false}.


literalIjkl() -> 
  #entry{type = <<"literal">>, value = <<"ijkl">>, ignore_case = false}.


literalMnop() -> 
  #entry{type = <<"literal">>, value = <<"mnop">>, ignore_case = false}.


semanticAnd() -> 
  #entry{type = <<"semantic_and">>, code = <<" code ">>}.


semanticNot() -> 
  #entry{type = <<"semantic_not">>, code = <<" code ">>}.


optional() -> 
  #entry{type = <<"optional">>, expression = literalAbcd()}.


zeroOrMore() -> 
  #entry{type = <<"zero_or_more">>, expression = literalAbcd()}.


oneOrMore() -> 
  #entry{type = <<"one_or_more">>, expression = literalAbcd()}.


textOptional() -> 
  #entry{type = <<"text">>, expression = optional()}.


simpleNotAbcd() -> 
  #entry{type = <<"simple_not">>, expression = literalAbcd()}.


simpleAndOptional() -> 
  #entry{type = <<"simple_and">>, expression = optional()}.


simpleNotOptional() -> 
  #entry{type = <<"simple_not">>, expression = optional()}.


labeledAbcd() -> 
  #entry{type = <<"labeled">>, label = <<"a">>, expression = literalAbcd()}.


labeledEfgh() -> 
  #entry{type = <<"labeled">>, label = <<"b">>, expression = literalEfgh()}.


labeledIjkl() -> 
  #entry{type = <<"labeled">>, label = <<"c">>, expression = literalIjkl()}.


labeledMnop() -> 
  #entry{type = <<"labeled">>, label = <<"d">>, expression = literalMnop()}.


labeledSimpleNot() -> 
  #entry{type = <<"labeled">>, label = <<"a">>, expression = simpleNotAbcd()}.



sequence() ->
  #entry{ type = <<"sequence">>
        , elements = [literalAbcd(), literalEfgh(), literalIjkl()]
        }.

sequence2() ->
  #entry{ type = <<"sequence">>
        , elements = [labeledAbcd(), labeledEfgh()]
        }.

sequence4() ->
  #entry{ type = <<"sequence">>
        , elements = [labeledAbcd(), labeledEfgh(), labeledIjkl(), labeledMnop()]
        }.

actionAbcd() ->
  #entry{type = <<"action">>, expression = literalAbcd(), code = <<" code ">>}.


actionEfgh() ->
  #entry{type = <<"action">>, expression = literalEfgh(), code = <<" code ">>}.


actionIjkl() ->
  #entry{type = <<"action">>, expression = literalIjkl(), code = <<" code ">>}.


actionMnop() ->
  #entry{type = <<"action">>, expression = literalMnop(), code = <<" code ">>}.


actionSequence() ->
  #entry{type = <<"action">>, expression = sequence(), code = <<" code ">>}.


choice() ->
  #entry{ type = <<"choice">>
        , alternatives = [literalAbcd(), literalEfgh(), literalIjkl()]
        }.

choice2() ->
  #entry{ type = <<"choice">>
        , alternatives = [actionAbcd(), actionEfgh()]
        }.

choice4() ->
  #entry{ type = <<"choice">>
        , alternatives = [actionAbcd(), actionEfgh(), actionIjkl(), actionMnop()]
        }.

named() ->
  #entry{type = <<"named">>, name = <<"start rule">>, expression = literalAbcd()}.


ruleA() ->
  #entry{type = <<"rule">>, name = <<"a">>, expression = literalAbcd()}.


ruleB() ->
  #entry{type = <<"rule">>, name = <<"b">>, expression = literalEfgh()}.


ruleC() ->
  #entry{type = <<"rule">>, name = <<"c">>, expression = literalIjkl()}.


ruleStart() ->
  #entry{type = <<"rule">>, name = <<"start">>, expression = literalAbcd()}.


initializer() ->
  #entry{type = <<"initializer">>, code = <<" code ">>}.

oneRuleGrammar(Expression) ->
  #entry{ type = <<"grammar">>
        , rules = [#entry{ type = <<"rule">>
                         , name = <<"start">>
                         , expression = Expression
                         }]
        , initializer = []
        }.

actionGrammar(Code) ->
  oneRuleGrammar(#entry{ type = <<"action">>
                       , expression = literalAbcd()
                       , code = Code
                       }
                ).

literalGrammar(Value, IgnoreCase) ->
  oneRuleGrammar(#entry{type = <<"literal">>
                       , value = Value
                       , ignore_case = IgnoreCase
                       }
                ).

classGrammar(Parts, Inverted, IgnoreCase, RawText) ->
    oneRuleGrammar(#entry{type = <<"class">>
                         , parts = Parts
                         , inverted = Inverted
                         , ignore_case = IgnoreCase
                         , raw_text = RawText
                         }
                  ).

anyGrammar() ->
    oneRuleGrammar(#entry{type = <<"any">>}).

ruleRefGrammar(Name) ->
    oneRuleGrammar(#entry{type = <<"rule_ref">>, name = Name}).

trivialGrammar() ->
  literalGrammar(<<"abcd">>, false).

twoRuleGrammar() ->
  #entry{ type = <<"grammar">>
        , rules = [ruleA(), ruleB()]
        , initializer = []
        }.


%% Produced grammar will have correct indices which will interfere with
%% ?assertEquals. We reset all indices here, without changing the structure
reset_indices(#entry{ alternatives = A
                    , elements     = Els
                    , expression   = E
                    , initializer  = I
                    , rules        = R} = Entry) ->
  Entry#entry{ alternatives = reset_indices(A)
             , elements     = reset_indices(Els)
             , expression   = reset_indices(E)
             , initializer  = reset_indices(I)
             , rules        = reset_indices(R)
             , index        = undefined
             };
reset_indices(List) when is_list(List) ->
  [reset_indices(E) || E <- List];
reset_indices(Other) ->
  Other.
