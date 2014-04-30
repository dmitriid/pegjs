-module(pegjs_parse).
-export([parse/1,file/1]).
-define(p_anything,true).
-define(p_charclass,true).
-define(p_choose,true).
-define(p_label,true).
-define(p_not,true).
-define(p_one_or_more,true).
-define(p_optional,true).
-define(p_scan,true).
-define(p_seq,true).
-define(p_string,true).
-define(p_zero_or_more,true).



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
  [C] = binary_to_list(C0),
  <<$\\, $x, (to_hex(C))/binary>>;
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

stringify(AST) when is_list(AST) ->
  stringify(iolist_to_binary(AST));
stringify(AST) when is_binary(AST) ->
  escape(AST).

escape(<<>>) -> <<>>;
escape(<<$\">>) -> <<"\\\"">>;
escape(<<$\n>>) -> <<"\\n">>;
escape(<<$\r>>) -> <<"\\r">>;
escape(<<$\f>>) -> <<"\\f">>;
escape(<<$\t>>) -> <<"\\t">>;
escape(<<$\\>>) -> <<"\\\\">>;
escape(<<B:1/binary>>) -> B;
escape(<<B:1/binary, Rest/binary>>) -> << (escape(B))/binary
                                        , (escape(Rest))/binary>>.

-spec file(file:name()) -> any().
file(Filename) -> case file:read_file(Filename) of {ok,Bin} -> parse(Bin); Err -> Err end.

-spec parse(binary() | list()) -> any().
parse(List) when is_list(List) -> parse(list_to_binary(List));
parse(Input) when is_binary(Input) ->
  setup_memo(),
  Result = case 'grammar'(Input,{{line,1},{column,1}}) of
             {AST, <<>>, _Index} -> AST;
             Any -> Any
           end,
  release_memo(), Result.

-spec 'grammar'(input(), index()) -> parse_result().
'grammar'(Input, Index) ->
  p(Input, Index, 'grammar', fun(I,D) -> (p_seq([fun '__'/2, p_label('initializer', p_optional(fun 'initializer'/2)), p_label('rules', p_one_or_more(fun 'rule'/2))]))(I,D) end, fun(Node, _Idx) ->
    Initializer = get_value('initializer', Node),
    Rules = get_value('rules', Node),
    #grammar{ initializer = Initializer
            , rules       = Rules
            , index       = Index
            }
   end).

-spec 'initializer'(input(), index()) -> parse_result().
'initializer'(Input, Index) ->
  p(Input, Index, 'initializer', fun(I,D) -> (p_seq([p_label('code', fun 'action'/2), p_optional(fun 'semicolon'/2)]))(I,D) end, fun(Node, _Idx) ->
    Code = get_value('code', Node),
    #code{ code  = Code
         , index = Index
         }
   end).

-spec 'rule'(input(), index()) -> parse_result().
'rule'(Input, Index) ->
  p(Input, Index, 'rule', fun(I,D) -> (p_seq([p_label('name', fun 'identifier'/2), p_label('displayName', p_optional(fun 'string'/2)), fun 'equals'/2, p_label('expression', fun 'expression'/2), p_optional(fun 'semicolon'/2)]))(I,D) end, fun(Node, _Idx) ->
    Expression  = get_value(expression, Node),
    Name        = get_value(name, Node),
    DisplayName = get_value(displayName, Node),
    #rule{ name         = Name
         , display_name = DisplayName
         , expression   = Expression
         , index        = Index
         }
   end).

-spec 'expression'(input(), index()) -> parse_result().
'expression'(Input, Index) ->
  p(Input, Index, 'expression', fun(I,D) -> (fun 'choice'/2)(I,D) end, fun(Node, Idx) ->transform('expression', Node, Idx) end).

-spec 'choice'(input(), index()) -> parse_result().
'choice'(Input, Index) ->
  p(Input, Index, 'choice', fun(I,D) -> (p_seq([p_label('head', fun 'sequence'/2), p_label('tail', p_zero_or_more(p_seq([fun 'slash'/2, fun 'sequence'/2])))]))(I,D) end, fun(Node, _Idx) ->
    Head = get_value(head, Node),
    Tail = get_value(tail, Node),
    case Tail of
      [] ->
        Head;
      _List ->
        Alternatives0 = lists:foldl(
                          fun(E, Acc) ->
                            [_, Sequence] = E,
                            [Sequence | Acc]
                          end,
                          [Head],
                          Tail
                        ),
        Alternatives = lists:reverse(Alternatives0),
        #choice{ alternatives = Alternatives
               , index        = Index
               }
    end
   end).

-spec 'sequence'(input(), index()) -> parse_result().
'sequence'(Input, Index) ->
  p(Input, Index, 'sequence', fun(I,D) -> (p_choose([p_seq([p_label('elements', p_zero_or_more(fun 'labeled'/2)), p_label('code', fun 'action'/2)]), p_label('elements', p_zero_or_more(fun 'labeled'/2))]))(I,D) end, fun(Node, _Idx) ->
    Node0 = case Node of
              N when is_list(N) -> N;
              _ -> [Node]
            end,
    Elements = get_value('elements', Node0),
    Code     = get_value('code', Node0),
    Expression = case Elements of
                   [OneElement] -> OneElement;
                   _ ->
                    Elements
                 end,
    #sequence{ elements = Expression
             , code     = #code{ code  = Code
                               , index = Index
                               }
             , index    = Index
             }
   end).

-spec 'labeled'(input(), index()) -> parse_result().
'labeled'(Input, Index) ->
  p(Input, Index, 'labeled', fun(I,D) -> (p_choose([p_seq([p_label('label', fun 'identifier'/2), fun 'colon'/2, p_label('expression', fun 'prefixed'/2)]), fun 'prefixed'/2]))(I,D) end, fun(Node, _Idx) ->
    case Node of
      [{_, Label}, _, {_, Expression}] ->
        #labeled{ label      = Label
                , expression = Expression
                , index      = Index
                };
      Expression ->
        #labeled{ expression = Expression
                , index      = Index
                }
    end
   end).

-spec 'prefixed'(input(), index()) -> parse_result().
'prefixed'(Input, Index) ->
  p(Input, Index, 'prefixed', fun(I,D) -> (p_choose([p_seq([p_label('dollar', fun 'dollar'/2), p_label('expression', fun 'suffixed'/2)]), p_seq([p_label('and', fun 'and'/2), p_label('code', fun 'action'/2)]), p_seq([p_label('and', fun 'and'/2), p_label('expression', fun 'suffixed'/2)]), p_seq([p_label('not', fun 'not'/2), p_label('code', fun 'action'/2)]), p_seq([p_label('not', fun 'not'/2), p_label('expression', fun 'suffixed'/2)]), fun 'suffixed'/2]))(I,D) end, fun(Node, _Idx) ->
    And        = get_value('and', Node),
    Code       = get_value('code', Node),
    Dollar     = get_value('dollar', Node),
    Expression = get_value('expression', Node),
    Not        = get_value('not', Node),

    case Dollar of
      [] ->
        case And of
          [] ->
            case Not of
              [] -> Node; %% suffixed
              _  ->
                case Code of
                  [] -> % not:not expression:suffixed
                    #prefixed{ type       = simple_not
                             , expression = Expression
                             , index      = Index
                             };
                  _  -> % not:not code:action
                    #prefixed{ type  = semantic_not
                             , code  = Code
                             , index = Index
                             }
                end
            end;
          _  ->
            case Code of
              [] -> % and:and expression:suffixed
                #prefixed{ type       = simple_and
                         , expression = Expression
                         , index      = Index
                         };
              _  -> % and:and code:action
                 #prefixed{ type  = semantic_and
                          , code  = Code
                          , index = Index
                          }
            end
        end;
      _ ->  % dollar:dollar expression:suffixed
        #text{ expression = Expression
             , index      = Index
             }
    end
   end).

-spec 'suffixed'(input(), index()) -> parse_result().
'suffixed'(Input, Index) ->
  p(Input, Index, 'suffixed', fun(I,D) -> (p_choose([p_seq([p_label('expression', fun 'primary'/2), p_label('suffix', fun 'question'/2)]), p_seq([p_label('expression', fun 'primary'/2), p_label('suffix', fun 'star'/2)]), p_seq([p_label('expression', fun 'primary'/2), p_label('suffix', fun 'plus'/2)]), fun 'primary'/2]))(I,D) end, fun(Node, _Idx) ->
    Expression = get_value('expression', Node),
    Suffix     = get_value('suffix', Node),
    case Expression of
      [] -> Node;
      _  ->
        case Suffix of
          <<"?">> ->
            #suffixed{ type       = optional
                     , expression = Expression
                     , index      = Index
                     };
          <<"*">> ->
            #suffixed{ type       = zero_or_more
                     , expression = Expression
                     , index      = Index
                     };
          <<"+">> ->
            #suffixed{ type       = one_or_more
                     , expression = Expression
                     , index      = Index
                     }
        end
    end
   end).

-spec 'primary'(input(), index()) -> parse_result().
'primary'(Input, Index) ->
  p(Input, Index, 'primary', fun(I,D) -> (p_choose([p_seq([p_label('name', fun 'identifier'/2), p_not(p_seq([p_optional(fun 'string'/2), fun 'equals'/2]))]), fun 'literal'/2, fun 'class'/2, fun 'dot'/2, p_seq([fun 'lparen'/2, p_label('expression', fun 'expression'/2), fun 'rparen'/2])]))(I,D) end, fun(Node, _Idx) ->
    case Node of
      <<".">> ->
        #anything{ index = Index };
      [_, _] ->
        #rule_ref{ name  = get_value('name', Node)
                 , index = Index
                 };
      [_, _, _] ->
        get_value('expression', Node);
      _ ->
        Node
    end
   end).

-spec 'action'(input(), index()) -> parse_result().
'action'(Input, Index) ->
  p(Input, Index, 'action', fun(I,D) -> (p_seq([p_label('braced', fun 'braced'/2), fun '__'/2]))(I,D) end, fun(Node, _Idx) ->
    Braced = get_value(braced, Node),
    binary:part(Braced, 1, size(Braced) - 2)
   end).

-spec 'braced'(input(), index()) -> parse_result().
'braced'(Input, Index) ->
  p(Input, Index, 'braced', fun(I,D) -> (p_seq([p_string(<<"{">>), p_zero_or_more(p_choose([fun 'braced'/2, fun 'nonBraceCharacters'/2])), p_string(<<"}">>)]))(I,D) end, fun(Node, _Idx) ->
    stringify(Node)
   end).

-spec 'nonBraceCharacters'(input(), index()) -> parse_result().
'nonBraceCharacters'(Input, Index) ->
  p(Input, Index, 'nonBraceCharacters', fun(I,D) -> (p_one_or_more(fun 'nonBraceCharacter'/2))(I,D) end, fun(Node, Idx) ->transform('nonBraceCharacters', Node, Idx) end).

-spec 'nonBraceCharacter'(input(), index()) -> parse_result().
'nonBraceCharacter'(Input, Index) ->
  p(Input, Index, 'nonBraceCharacter', fun(I,D) -> (p_charclass(<<"[^{}]">>))(I,D) end, fun(Node, Idx) ->transform('nonBraceCharacter', Node, Idx) end).

-spec 'equals'(input(), index()) -> parse_result().
'equals'(Input, Index) ->
  p(Input, Index, 'equals', fun(I,D) -> (p_seq([p_string(<<"=">>), fun '__'/2]))(I,D) end, fun(_Node, _Idx) -> <<"=">>  end).

-spec 'colon'(input(), index()) -> parse_result().
'colon'(Input, Index) ->
  p(Input, Index, 'colon', fun(I,D) -> (p_seq([p_string(<<":">>), fun '__'/2]))(I,D) end, fun(_Node, _Idx) -> <<":">>  end).

-spec 'semicolon'(input(), index()) -> parse_result().
'semicolon'(Input, Index) ->
  p(Input, Index, 'semicolon', fun(I,D) -> (p_seq([p_string(<<";">>), fun '__'/2]))(I,D) end, fun(_Node, _Idx) -> <<";">>  end).

-spec 'slash'(input(), index()) -> parse_result().
'slash'(Input, Index) ->
  p(Input, Index, 'slash', fun(I,D) -> (p_seq([p_string(<<"\/">>), fun '__'/2]))(I,D) end, fun(_Node, _Idx) -> <<"/">>  end).

-spec 'and'(input(), index()) -> parse_result().
'and'(Input, Index) ->
  p(Input, Index, 'and', fun(I,D) -> (p_seq([p_string(<<"&">>), fun '__'/2]))(I,D) end, fun(_Node, _Idx) -> <<"&">>  end).

-spec 'not'(input(), index()) -> parse_result().
'not'(Input, Index) ->
  p(Input, Index, 'not', fun(I,D) -> (p_seq([p_string(<<"!">>), fun '__'/2]))(I,D) end, fun(_Node, _Idx) -> <<"!">>  end).

-spec 'dollar'(input(), index()) -> parse_result().
'dollar'(Input, Index) ->
  p(Input, Index, 'dollar', fun(I,D) -> (p_seq([p_string(<<"$">>), fun '__'/2]))(I,D) end, fun(_Node, _Idx) -> <<"$">>  end).

-spec 'question'(input(), index()) -> parse_result().
'question'(Input, Index) ->
  p(Input, Index, 'question', fun(I,D) -> (p_seq([p_string(<<"?">>), fun '__'/2]))(I,D) end, fun(_Node, _Idx) -> <<"?">>  end).

-spec 'star'(input(), index()) -> parse_result().
'star'(Input, Index) ->
  p(Input, Index, 'star', fun(I,D) -> (p_seq([p_string(<<"*">>), fun '__'/2]))(I,D) end, fun(_Node, _Idx) -> <<"*">>  end).

-spec 'plus'(input(), index()) -> parse_result().
'plus'(Input, Index) ->
  p(Input, Index, 'plus', fun(I,D) -> (p_seq([p_string(<<"+">>), fun '__'/2]))(I,D) end, fun(_Node, _Idx) -> <<"+">>  end).

-spec 'lparen'(input(), index()) -> parse_result().
'lparen'(Input, Index) ->
  p(Input, Index, 'lparen', fun(I,D) -> (p_seq([p_string(<<"(">>), fun '__'/2]))(I,D) end, fun(_Node, _Idx) -> <<"(">>  end).

-spec 'rparen'(input(), index()) -> parse_result().
'rparen'(Input, Index) ->
  p(Input, Index, 'rparen', fun(I,D) -> (p_seq([p_string(<<")">>), fun '__'/2]))(I,D) end, fun(_Node, _Idx) -> <<")">>  end).

-spec 'dot'(input(), index()) -> parse_result().
'dot'(Input, Index) ->
  p(Input, Index, 'dot', fun(I,D) -> (p_seq([p_string(<<".">>), fun '__'/2]))(I,D) end, fun(_Node, _Idx) -> <<".">>  end).

-spec 'identifier'(input(), index()) -> parse_result().
'identifier'(Input, Index) ->
  p(Input, Index, 'identifier', fun(I,D) -> (p_seq([p_label('chars', p_seq([p_choose([fun 'letter'/2, p_string(<<"_">>)]), p_zero_or_more(p_choose([fun 'letter'/2, fun 'digit'/2, p_string(<<"_">>)]))])), fun '__'/2]))(I,D) end, fun(Node, _Idx) ->
    stringify(get_value(chars, Node))
   end).

-spec 'literal'(input(), index()) -> parse_result().
'literal'(Input, Index) ->
  p(Input, Index, 'literal', fun(I,D) -> (p_seq([p_label('value', p_choose([fun 'doubleQuotedString'/2, fun 'singleQuotedString'/2])), p_label('flags', p_optional(p_string(<<"i">>))), fun '__'/2]))(I,D) end, fun(Node, _Idx) ->
  #literal{ value       = get_value('value', Node)
          , ignore_case = get_value('flags', Node) == <<"i">>
          }
   end).

-spec 'string'(input(), index()) -> parse_result().
'string'(Input, Index) ->
  p(Input, Index, 'string', fun(I,D) -> (p_seq([p_label('string', p_choose([fun 'doubleQuotedString'/2, fun 'singleQuotedString'/2])), fun '__'/2]))(I,D) end, fun(Node, _Idx) ->
    get_value('string', Node)
   end).

-spec 'doubleQuotedString'(input(), index()) -> parse_result().
'doubleQuotedString'(Input, Index) ->
  p(Input, Index, 'doubleQuotedString', fun(I,D) -> (p_seq([p_string(<<"\"">>), p_label('chars', p_zero_or_more(fun 'doubleQuotedCharacter'/2)), p_string(<<"\"">>)]))(I,D) end, fun(Node, _Idx) ->
    stringify(get_value(chars, Node))
   end).

-spec 'doubleQuotedCharacter'(input(), index()) -> parse_result().
'doubleQuotedCharacter'(Input, Index) ->
  p(Input, Index, 'doubleQuotedCharacter', fun(I,D) -> (p_choose([fun 'simpleDoubleQuotedCharacter'/2, fun 'simpleEscapeSequence'/2, fun 'zeroEscapeSequence'/2, fun 'hexEscapeSequence'/2, fun 'unicodeEscapeSequence'/2, fun 'eolEscapeSequence'/2]))(I,D) end, fun(Node, Idx) ->transform('doubleQuotedCharacter', Node, Idx) end).

-spec 'simpleDoubleQuotedCharacter'(input(), index()) -> parse_result().
'simpleDoubleQuotedCharacter'(Input, Index) ->
  p(Input, Index, 'simpleDoubleQuotedCharacter', fun(I,D) -> (p_seq([p_not(p_choose([p_string(<<"\"">>), p_string(<<"\\">>), fun 'eolChar'/2])), p_label('char_', p_anything())]))(I,D) end, fun(Node, _Idx) ->
    get_value('char_', Node)
   end).

-spec 'singleQuotedString'(input(), index()) -> parse_result().
'singleQuotedString'(Input, Index) ->
  p(Input, Index, 'singleQuotedString', fun(I,D) -> (p_seq([p_string(<<"\'">>), p_label('chars', p_zero_or_more(fun 'singleQuotedCharacter'/2)), p_string(<<"\'">>)]))(I,D) end, fun(Node, _Idx) ->
    stringify(get_value(chars, Node))
   end).

-spec 'singleQuotedCharacter'(input(), index()) -> parse_result().
'singleQuotedCharacter'(Input, Index) ->
  p(Input, Index, 'singleQuotedCharacter', fun(I,D) -> (p_choose([fun 'simpleSingleQuotedCharacter'/2, fun 'simpleEscapeSequence'/2, fun 'zeroEscapeSequence'/2, fun 'hexEscapeSequence'/2, fun 'unicodeEscapeSequence'/2, fun 'eolEscapeSequence'/2]))(I,D) end, fun(Node, Idx) ->transform('singleQuotedCharacter', Node, Idx) end).

-spec 'simpleSingleQuotedCharacter'(input(), index()) -> parse_result().
'simpleSingleQuotedCharacter'(Input, Index) ->
  p(Input, Index, 'simpleSingleQuotedCharacter', fun(I,D) -> (p_seq([p_not(p_choose([p_string(<<"\'">>), p_string(<<"\\">>), fun 'eolChar'/2])), p_label('char_', p_anything())]))(I,D) end, fun(Node, _Idx) ->
    get_value('char_', Node)
   end).

-spec 'class'(input(), index()) -> parse_result().
'class'(Input, Index) ->
  p(Input, Index, 'class', fun(I,D) -> (p_seq([p_string(<<"[">>), p_label('inverted', p_optional(p_string(<<"^">>))), p_label('parts', p_zero_or_more(p_choose([fun 'classCharacterRange'/2, fun 'classCharacter'/2]))), p_string(<<"]">>), p_label('flags', p_optional(p_string(<<"i">>))), fun '__'/2]))(I,D) end, fun(Node, _Idx) ->
    Inverted = case get_value(inverted, Node) of [] -> <<>>; I -> I end,
    Parts = get_value(parts, Node),
    Flags = case get_value(flags, Node) of [] -> <<>>; F -> F end,

    PartsConverted = lists:map(fun(Part) -> get_value(data, Part) end, Parts),
    PartsRawText = << <<(get_attr(raw_text, Part))/binary>> || Part <- Parts>>,
    RawText = << "["
               , Inverted/binary
               , PartsRawText/binary
               , "]"
              >>,
    #regexp{ parts       = stringify(PartsConverted)
           , raw_text    = stringify(RawText)
           , inverted    = Inverted == <<"^">>
           , ignore_case = Flags == <<"i">>
           , index       = Index
           }
   end).

-spec 'classCharacterRange'(input(), index()) -> parse_result().
'classCharacterRange'(Input, Index) ->
  p(Input, Index, 'classCharacterRange', fun(I,D) -> (p_seq([p_label('begin', fun 'classCharacter'/2), p_string(<<"-">>), p_label('end', fun 'classCharacter'/2)]))(I,D) end, fun(Node, _Idx) ->
    Begin = get_value('begin', Node),
    End   = get_value('end', Node),
    RawBegin = get_attr(raw_text, Begin),
    RawEnd   = get_attr(raw_text, End),
    case Begin > End of
      true ->
        error({invalid_character_range, {RawBegin, RawEnd}});
      false ->
        #character_range{ 'begin'  = get_value(data, Begin)
                        , 'end'    = get_value(data, End)
                        , raw_text = <<RawBegin/binary, "-", RawEnd/binary>>
                        , index    = Index
                        }
    end
   end).

-spec 'classCharacter'(input(), index()) -> parse_result().
'classCharacter'(Input, Index) ->
  p(Input, Index, 'classCharacter', fun(I,D) -> (fun 'bracketDelimitedCharacter'/2)(I,D) end, fun(Node, _Idx) ->
    #charclass{ data     = Node
              , raw_text = quote_for_regexp_class(Node)
              , index    = Index
              }
   end).

-spec 'bracketDelimitedCharacter'(input(), index()) -> parse_result().
'bracketDelimitedCharacter'(Input, Index) ->
  p(Input, Index, 'bracketDelimitedCharacter', fun(I,D) -> (p_choose([fun 'simpleBracketDelimitedCharacter'/2, fun 'simpleEscapeSequence'/2, fun 'zeroEscapeSequence'/2, fun 'hexEscapeSequence'/2, fun 'unicodeEscapeSequence'/2, fun 'eolEscapeSequence'/2]))(I,D) end, fun(Node, Idx) ->transform('bracketDelimitedCharacter', Node, Idx) end).

-spec 'simpleBracketDelimitedCharacter'(input(), index()) -> parse_result().
'simpleBracketDelimitedCharacter'(Input, Index) ->
  p(Input, Index, 'simpleBracketDelimitedCharacter', fun(I,D) -> (p_seq([p_not(p_choose([p_string(<<"]">>), p_string(<<"\\">>), fun 'eolChar'/2])), p_label('char_', p_anything())]))(I,D) end, fun(Node, _Idx) ->
    {_, Char} = lists:keyfind('char_', 1, Node),
    Char
   end).

-spec 'simpleEscapeSequence'(input(), index()) -> parse_result().
'simpleEscapeSequence'(Input, Index) ->
  p(Input, Index, 'simpleEscapeSequence', fun(I,D) -> (p_seq([p_string(<<"\\">>), p_not(p_choose([fun 'digit'/2, p_string(<<"x">>), p_string(<<"u">>), fun 'eolChar'/2])), p_label('char_', p_anything())]))(I,D) end, fun(Node, _Idx) ->
    {_, Char} = lists:keyfind('char_', 1, Node),
    case Char of
      <<"b">> -> <<"\b">>;
      <<"f">> -> <<"\f">>;
      <<"n">> -> <<"\n">>;
      <<"r">> -> <<"\r">>;
      <<"t">> -> <<"\t">>;
      <<"v">> -> <<"\v">>; %% or "\x{0B}"
      _       -> Char
    end
   end).

-spec 'zeroEscapeSequence'(input(), index()) -> parse_result().
'zeroEscapeSequence'(Input, Index) ->
  p(Input, Index, 'zeroEscapeSequence', fun(I,D) -> (p_seq([p_string(<<"\\0">>), p_not(fun 'digit'/2)]))(I,D) end, fun(_Node, _Idx) ->
    "\x{00}"
   end).

-spec 'hexEscapeSequence'(input(), index()) -> parse_result().
'hexEscapeSequence'(Input, Index) ->
  p(Input, Index, 'hexEscapeSequence', fun(I,D) -> (p_seq([p_string(<<"\\x">>), p_label('digits', p_seq([fun 'hexDigit'/2, fun 'hexDigit'/2]))]))(I,D) end, fun(Node, _Idx) ->
    Digits0 = get_value(digits, Node),
    Digits1 = [binary_to_list(D) || D <- Digits0],
    Digits = list_to_integer(lists:flatten(Digits1), 16),
    stringify(unicode:characters_to_binary([Digits]))
   end).

-spec 'unicodeEscapeSequence'(input(), index()) -> parse_result().
'unicodeEscapeSequence'(Input, Index) ->
  p(Input, Index, 'unicodeEscapeSequence', fun(I,D) -> (p_seq([p_string(<<"\\u">>), p_label('digits', p_seq([fun 'hexDigit'/2, fun 'hexDigit'/2, fun 'hexDigit'/2, fun 'hexDigit'/2]))]))(I,D) end, fun(Node, _Idx) ->
    Digits0 = get_value(digits, Node),
    Digits1 = [binary_to_list(D) || D <- Digits0],
    Digits = list_to_integer(lists:flatten(Digits1), 16),
    unicode:characters_to_binary([Digits])
   end).

-spec 'eolEscapeSequence'(input(), index()) -> parse_result().
'eolEscapeSequence'(Input, Index) ->
  p(Input, Index, 'eolEscapeSequence', fun(I,D) -> (p_seq([p_string(<<"\\">>), p_label('eol', fun 'eol'/2)]))(I,D) end, fun(Node, _Idx) ->
    Eol = get_value(eol, Node),
    Eol
   end).

-spec 'digit'(input(), index()) -> parse_result().
'digit'(Input, Index) ->
  p(Input, Index, 'digit', fun(I,D) -> (p_charclass(<<"[0-9]">>))(I,D) end, fun(Node, Idx) ->transform('digit', Node, Idx) end).

-spec 'hexDigit'(input(), index()) -> parse_result().
'hexDigit'(Input, Index) ->
  p(Input, Index, 'hexDigit', fun(I,D) -> (p_charclass(<<"[0-9a-fA-F]">>))(I,D) end, fun(Node, Idx) ->transform('hexDigit', Node, Idx) end).

-spec 'letter'(input(), index()) -> parse_result().
'letter'(Input, Index) ->
  p(Input, Index, 'letter', fun(I,D) -> (p_choose([fun 'lowerCaseLetter'/2, fun 'upperCaseLetter'/2]))(I,D) end, fun(Node, Idx) ->transform('letter', Node, Idx) end).

-spec 'lowerCaseLetter'(input(), index()) -> parse_result().
'lowerCaseLetter'(Input, Index) ->
  p(Input, Index, 'lowerCaseLetter', fun(I,D) -> (p_charclass(<<"[a-z]">>))(I,D) end, fun(Node, Idx) ->transform('lowerCaseLetter', Node, Idx) end).

-spec 'upperCaseLetter'(input(), index()) -> parse_result().
'upperCaseLetter'(Input, Index) ->
  p(Input, Index, 'upperCaseLetter', fun(I,D) -> (p_charclass(<<"[A-Z]">>))(I,D) end, fun(Node, Idx) ->transform('upperCaseLetter', Node, Idx) end).

-spec '__'(input(), index()) -> parse_result().
'__'(Input, Index) ->
  p(Input, Index, '__', fun(I,D) -> (p_zero_or_more(p_choose([fun 'whitespace'/2, fun 'eol'/2, fun 'comment'/2])))(I,D) end, fun(Node, Idx) ->transform('__', Node, Idx) end).

-spec 'comment'(input(), index()) -> parse_result().
'comment'(Input, Index) ->
  p(Input, Index, 'comment', fun(I,D) -> (p_choose([fun 'singleLineComment'/2, fun 'multiLineComment'/2]))(I,D) end, fun(Node, Idx) ->transform('comment', Node, Idx) end).

-spec 'singleLineComment'(input(), index()) -> parse_result().
'singleLineComment'(Input, Index) ->
  p(Input, Index, 'singleLineComment', fun(I,D) -> (p_seq([p_string(<<"\/\/">>), p_zero_or_more(p_seq([p_not(fun 'eolChar'/2), p_anything()]))]))(I,D) end, fun(Node, Idx) ->transform('singleLineComment', Node, Idx) end).

-spec 'multiLineComment'(input(), index()) -> parse_result().
'multiLineComment'(Input, Index) ->
  p(Input, Index, 'multiLineComment', fun(I,D) -> (p_seq([p_string(<<"\/*">>), p_zero_or_more(p_seq([p_not(p_string(<<"*\/">>)), p_anything()])), p_string(<<"*\/">>)]))(I,D) end, fun(Node, Idx) ->transform('multiLineComment', Node, Idx) end).

-spec 'eol'(input(), index()) -> parse_result().
'eol'(Input, Index) ->
  p(Input, Index, 'eol', fun(I,D) -> (p_choose([p_string(<<"\n">>), p_string(<<"\r\n">>), p_string(<<"\r">>), p_charclass(<<"[\\x{2028}]">>), p_charclass(<<"[\\x{2029}]">>)]))(I,D) end, fun(Node, Idx) ->transform('eol', Node, Idx) end).

-spec 'eolChar'(input(), index()) -> parse_result().
'eolChar'(Input, Index) ->
  p(Input, Index, 'eolChar', fun(I,D) -> (p_charclass(<<"[\n\r\\x{2028}\\x{2029}]">>))(I,D) end, fun(Node, Idx) ->transform('eolChar', Node, Idx) end).

-spec 'whitespace'(input(), index()) -> parse_result().
'whitespace'(Input, Index) ->
  p(Input, Index, 'whitespace', fun(I,D) -> (p_charclass(<<"[\s\\t\\v\\f\\x{00A0}\\x{FEFF}\\x{1680}\\x{180E}\\x{2000}-\\x{200A}\\x{202F}\\x{205F}\\x{3000}]">>))(I,D) end, fun(Node, Idx) ->transform('whitespace', Node, Idx) end).


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

-spec p_advance_index(input() | unicode:charlist() | pos_integer(), index()) -> index().
p_advance_index(MatchedInput, Index) when is_list(MatchedInput) orelse is_binary(MatchedInput)-> % strings
  lists:foldl(fun p_advance_index/2, Index, unicode:characters_to_list(MatchedInput));
p_advance_index(MatchedInput, Index) when is_integer(MatchedInput) -> % single characters
  {{line, Line}, {column, Col}} = Index,
  case MatchedInput of
    $\n -> {{line, Line+1}, {column, 1}};
    _ -> {{line, Line}, {column, Col+1}}
  end.
