{
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
}


grammar
  = __ initializer:initializer? rules:rule+
  {
    [_, {_, Initializer}, {_, Rules}] = Node,
    I = case Initializer of
          [] -> #code{};
          _  -> #code{ code = Initializer, index = Idx }
        end,
    #grammar{ initializer = I
            , rules       = Rules
            , index       = Idx
            }
  }

initializer
  = code:action semicolon?
  {
    [{_, Code}, _] = Node,
    Code
  }

rule
  = name:identifier displayName:string? equals expression:expression semicolon?
  {
    [{_, Name}, {_, DisplayName}, _, {_, Expression}, _] = Node,
    #rule{ name         = Name
         , display_name = DisplayName
         , expression   = Expression
         , index        = Idx
         }
  }

expression
  = choice

choice
  = head:sequence tail:(slash sequence)*
  {
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
  }

sequence
  = elements:labeled* code:action
    {
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
    }
  / elements:labeled*
  {
    [{_, Elements}] = Node,
    Expression = case Elements of
                   [OneElement] -> OneElement;
                   _ ->
                    Elements
                 end,
    #sequence{ elements = Expression
             , index    = Idx
             }
  }

labeled
  = label:identifier colon expression:prefixed
    {
      [{_, Label}, _, {_, Expression}] = Node,
      #labeled{ label      = Label
              , expression = Expression
              , index      = Idx
              }
    }
  / prefixed
    {
      #labeled{ expression = Node
              , index      = Idx
              }
    }

prefixed
  = dollar:dollar expression:suffixed
    {
      [_, {_, Expression}] = Node,
      #text{ expression = Expression
           , index      = Idx
           }
    }
  / and:and code:action
    {
      [_, {_, Code}] = Node,
      #prefixed{ type  = semantic_and
               , code  = #code{ code  = Code
                              , index = Idx
                              }
               , index = Idx
               }
    }
  / and:and expression:suffixed
    {
      [_, {_, Expression}] = Node,
      #prefixed{ type       = simple_and
               , expression = Expression
               , index      = Idx
               }
    }
  / not:not code:action
    {
      [_, {_, Code}] = Node,
      #prefixed{ type  = semantic_not
               , code  = #code{ code  = Code
                              , index = Idx
                              }
               , index = Idx
               }
    }
  / not:not expression:suffixed
    {
      [_, {_, Expression}] = Node,
      #prefixed{ type       = simple_not
               , expression = Expression
               , index      = Idx
               }
    }
  / suffixed

suffixed
  = expression:primary suffix:question
    {
      [{_, Expression}, _] = Node,
      #suffixed{ type       = optional
               , expression = Expression
               , index      = Idx
               }
    }
  / expression:primary suffix:star
    {
      [{_, Expression}, _] = Node,
      #suffixed{ type       = zero_or_more
               , expression = Expression
               , index      = Idx
               }
    }
  / expression:primary suffix:plus
    {
      [{_, Expression}, _] = Node,
      #suffixed{ type       = one_or_more
               , expression = Expression
               , index      = Idx
               }
    }
  / primary

primary
  = name:identifier !(string? equals)
    {
      [{_, Name}, _] = Node,
      #rule_ref{ name  = Name
               , index = Idx
               }
    }
  / literal
  / class
  / dot
    {
      #anything{ index = Idx }
    }
  / lparen expression:expression rparen
    {
      [_, {_, Expression}, _] = Node,
      Expression
    }

/* "Lexical" elements */

action
  = braced:braced __
  {
    [{_, Braced}, _] = Node,
    binary:part(Braced, 1, size(Braced) - 2)
  }

braced
  =
  ("{" (braced / nonBraceCharacters)* "}")
  {
    iolist_to_binary(Node)
  }

nonBraceCharacters
  = nonBraceCharacter+

nonBraceCharacter
  = [^{}]

equals    = "=" __ { <<"=">> }
colon     = ":" __ { <<":">> }
semicolon = ";" __ { <<";">> }
slash     = "/" __ { <<"/">> }
and       = "&" __ { <<"&">> }
not       = "!" __ { <<"!">> }
dollar    = "$" __ { <<"$">> }
question  = "?" __ { <<"?">> }
star      = "*" __ { <<"*">> }
plus      = "+" __ { <<"+">> }
lparen    = "(" __ { <<"(">> }
rparen    = ")" __ { <<")">> }
dot       = "." __ { <<".">> }

/*
 * Modeled after ECMA-262, 5th ed., 7.6, but much simplified:
 *
 * * no Unicode escape sequences
 *
 * * "Unicode combining marks" and "Unicode connection punctuation" can't be
 *   part of the identifier
 *
 * * only [a-zA-Z] is considered a "Unicode letter"
 *
 * * only [0-9] is considered a "Unicode digit"
 *
 * The simplifications were made just to make the implementation little bit
 * easier, there is no "philosophical" reason behind them.
 *
 * Contrary to ECMA 262, the "$" character is not valid because it serves other
 * purpose in the grammar.
 */

identifier
  = chars:((letter / "_") (letter / digit / "_")*) __
  {
    [{_, Chars}, _] = Node,
    iolist_to_binary(do_convert_to_iolist(Chars))
  }

/*
 * Modeled after ECMA-262, 5th ed., 7.8.4. (syntax & semantics, rules only
 * vaguely).
 */
literal
  = value:(doubleQuotedString / singleQuotedString) flags:"i"? __
  {
    [{_, [Value]}, {_, Flags}, _] = Node,
    #literal{ value       = escape(lists:flatten(Value))
            , ignore_case = Flags == <<"i">>
            }
  }

string
  = string:(doubleQuotedString / singleQuotedString) __
  {
    [{_, String}, _] = Node,
    String
  };

doubleQuotedString
  = '"' chars:doubleQuotedCharacter* '"'
  {
    [_, {_, Chars}, _] = Node,
    Chars
  }

doubleQuotedCharacter
  = simpleDoubleQuotedCharacter
  / simpleEscapeSequence
  / zeroEscapeSequence
  / hexEscapeSequence
  / unicodeEscapeSequence
  / eolEscapeSequence

simpleDoubleQuotedCharacter
  = !('"' / "\\" / eolChar) char_:.
  {
    [_, {_, Char_}] = Node,
    Char_
  }

singleQuotedString
  = "'" chars:singleQuotedCharacter* "'"
  {
    [_, {_, Chars}, _] = Node,
    Chars
  }

singleQuotedCharacter
  = simpleSingleQuotedCharacter
  / simpleEscapeSequence
  / zeroEscapeSequence
  / hexEscapeSequence
  / unicodeEscapeSequence
  / eolEscapeSequence

simpleSingleQuotedCharacter
  = !("'" / "\\" / eolChar) char_:.
  {
    [_, {_, Char_}] = Node,
    Char_
  }

class
  = "[" inverted:"^"? parts:(classCharacterRange / classCharacter)* "]" flags:"i"? __
  {
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
  }

classCharacterRange
  = begin:classCharacter "-" end:classCharacter
  {
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
  }

classCharacter
  = bracketDelimitedCharacter
  {
    [[Data]] = Node,
    #charclass{ data     = Data
              , raw_text = quote_for_regexp_class(Data)
              , index    = Idx
              }
  }

bracketDelimitedCharacter
  = simpleBracketDelimitedCharacter
  / simpleEscapeSequence
  / zeroEscapeSequence
  / hexEscapeSequence
  / unicodeEscapeSequence
  / eolEscapeSequence

simpleBracketDelimitedCharacter
  = !("]" / "\\" / eolChar) char_:.
  {
    [_, {_, Char_}] = Node,
    Char_
  }

simpleEscapeSequence
  = "\\" !(digit / "x" / "u" / eolChar) char_:.
  {
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
  }

zeroEscapeSequence
  = "\\0" !digit
  {
    "\x{00}"
  }

hexEscapeSequence
  = "\\x" digits:(hexDigit hexDigit)
  {
    [_, {_, Digits0}] = Node,
    Digits = lists:foldl( fun(D, Acc) -> <<Acc/binary, D/binary>> end
                        , <<>>, lists:flatten(Digits0)),
    <<"\\x{", Digits/binary, "}">>
  }

unicodeEscapeSequence
  = "\\u" digits:(hexDigit hexDigit hexDigit hexDigit)
  {
    [_, {_, Digits0}] = Node,
    Digits = lists:foldl( fun(D, Acc) -> <<Acc/binary, D/binary>> end
                        , <<>>, lists:flatten(Digits0)),
    <<"\\x{", Digits/binary, "}">>
  }

eolEscapeSequence
  = "\\" eol:eol
  {
    [_, {_, Eol}] = Node,
    Eol
  };

digit
  = [0-9]

hexDigit
  = [0-9a-fA-F]

letter
  = lowerCaseLetter
  / upperCaseLetter

lowerCaseLetter
  = [a-z]

upperCaseLetter
  = [A-Z]

__ = (whitespace / eol / comment)*

/* Modeled after ECMA-262, 5th ed., 7.4. */
comment
  = singleLineComment
  / multiLineComment

singleLineComment
  = "//" (!eolChar .)*

multiLineComment
  = "/*" (!"*/" .)* "*/"

/* Modeled after ECMA-262, 5th ed., 7.3. */
eol
  = "\n"
  / "\r\n"
  / "\r"
  / [\x2028]
  / [\x2029]

eolChar
  = [\n\r\x2028\x2029]

/* Modeled after ECMA-262, 5th ed., 7.2. */
whitespace
  = [ \t\v\f\x00A0\xFEFF\x1680\x180E\x2000-\x200A\x202F\x205F\x3000]
