/*
  Parse peg.js file and look for @append instructions

  See https://github.com/for-GET/core-pegjs for a description
*/


appends
  = (append { [B] = Node, B }/ . { <<>> })+
  {
    [List] = Node,
    lists:filter(fun(<<>>) -> false; (_) -> true end, List)
  }

append
  = ws* (comment / star) ws* "@append" ws+ $((!(eol / eof) .)+)
  {
    [_, _, _, _, _, File] = Node,
    File
  }

comment
  = "/*" / "//"

star
  = "*"*

ws
  = [ \t\v\f\u00A0\uFEFF\u1680\u180E\u2000-\u200A\u202F\u205F\u3000]

eol
  = "\n"
  / "\r\n"
  / "\r"
  / [\u2028]
  / [\u2029]

eof = !.
