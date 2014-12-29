additive = multitive "+" additive      { [A, _, B] = match(Node), A + B }
         / multitive

multitive = primary "*" Mul:multitive  { [A, _, {<<"Mul">>, B}] = match(Node), A * B }
          / primary

primary   = par:("(" add:additive ")") { Par = value(<<"par">>, Node),
                                         value(<<"add">>, Par)
                                       }
          / dec:decimal                { value(<<"dec">>, Node) }

decimal   = [0-9]+
            { Ints = text(Node),
              list_to_integer(binary_to_list(Ints))
            }
