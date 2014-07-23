additive = multitive "+" additive      { [A, <<"+">>, B] = Node, A + B }
         / multitive                   { [M] = Node, M }

multitive = primary "*" Mul:multitive  { [A, <<"*">>, {<<"Mul">>, B}] = Node, A * B }
          / primary                    { [P] = Node, P }

primary   = par:("(" add:additive ")") { [{<<"par">>,List}] = Node,
                                         [_, {<<"add">>, Value}, _] = List,
                                         Value
                                       }
          / dec:decimal                { [{<<"dec">>,Int}] = Node, Int }

decimal   = [0-9]+
            { [Ints] = Node,
              List = lists:flatten([binary_to_list(B) || B <- Ints]),
              list_to_integer(List)
            }
