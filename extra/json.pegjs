json_value = space? (object / array / string / number / true / false / null) space?
             { [Result] = lists:nth(2, Node), Result };

object     = '{' space? head:pair tail:(space? ',' space? pair)* space? '}'
              {
                [_, _, {<<"head">>, Head}, {<<"tail">>, Tails}, _, _] = Node,
                Tail = [Pair || [_, _, _, Pair] <- Tails],
                [Head | Tail]
              }
           / '{' space? '}' { {struct, []} }

pair = space? key:string space? ':' space? value:json_value space?
       {
         [_, {<<"key">>, Key}, _, _, _, {<<"value">>, Value}, _] = Node,
         {Key, Value}
       }

array = '[' space? head:json_value tail:(space? ',' space? json_value)* space? ']'
        {
          [_, _, {<<"head">>, Head}, {<<"tail">>, Tails}, _, _] = Node,
          Tail = [JSONValue || [_, _, _, JSONValue] <- Tails],
          [Head | Tail]
        }
      / '[' space? ']' { [] }

string = '"' chars:(!'"' ("\\\\" / '\\"' / .))* '"'
         {
            [_, {<<"chars">>, List}, _] = Node,
            iolist_to_binary(List)
         };

number = int frac? exp?
         {
           case Node of
             [Int, [], []] -> list_to_integer(binary_to_list(iolist_to_binary(Int)));
             [Int, Frac, []] -> list_to_float(binary_to_list(iolist_to_binary([Int, Frac])));
             [Int, [], Exp] -> list_to_float(binary_to_list(iolist_to_binary([Int, ".0", Exp])));
             _ -> list_to_float(binary_to_list(iolist_to_binary(Node)))
           end
         }

int   = '-'? (non_zero_digit digit+) / digit
frac  = '.' digit+
exp   = e digit+
e     = [eE] ('+' / '-')?
non_zero_digit = [1-9]
digit = [0-9]
true  = 'true' { true }
false = 'false' { false }
null  = 'null' { null }
space = [ \t\n\s\r]*
