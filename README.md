# An implementation of [PEG.js](http://pegjs.majda.cz) grammar for Erlang

This is a rather straightforward port/implementation of the grammar defined for
[PEG.js](http://pegjs.majda.cz/documentation#grammar-syntax-and-semantics).

## Current status

- This is a **beta** (at least, until bootstrapping is implemented)
- As far as I can tell, implements everything from the PEG.js grammar
- Generates complete useable parsers
- The project has not been bootstrapped yet (that is the parser is not yet 
  generated from the grammar by the parser itself)
- For now uses [Neotoma](https://github.com/seancribbs/neotoma) to generate the parser
- Is based on an earlier definition of the grammar (probably [this](https://github.com/dmajda/pegjs/blob/f0a6bc92cc24b623689c7811bebc1ce2921442f0/src/parser.pegjs))
  than the one that currently [exists](https://github.com/dmajda/pegjs/blob/master/src/parser.pegjs) 
  for PEG.js. Currenlty there's no timeframe or even a decision on when/whether to update the erlang version

## Further work

- Bootstrap
- Support @append (see, e.g. [core-pegjs](https://github.com/for-GET/core-pegjs) 
  in the for-GET project)
- Add more options
  - ignore some errors (such as unused top-level rules)
  - set root for multiple grammars (when @append is implemented)
  - set output file/directory
- Dialyze, create dialyzer-friendly parsers

## How to use

```erlang
> pegjs:file("extra/csv_pegjs.peg").
ok
> c("extra/csv_pegjs.erl").
{ok, csv_pegjs}
> csv_pegjs:parse("a,b,c").
[{<<"head">>,
  [{<<"head">>,[[[[],[],<<"a">>]]]},
   {<<"tail">>,
    [[[<<",">>],[[[[],[],<<"b">>]]]],
     [[<<",">>],[[[[],[],<<"c">>]]]]]}]},
 {<<"tail">>,[]}]
```

## How to contribute/develop

Suggestions and improvements are more than welcome!

Current grammar in `priv/pegjs_parse.peg` is created for [Neotoma](https://github.com/seancribbs/neotoma),
so you need that to tweak pegjs.

`pegjs_analyze` module is inspired by `neotoma_analyze` from the [2.0-refactor](https://github.com/seancribbs/neotoma/tree/2.0-refactor)
branch of neotoma.

Non-generated parser combinators can be found in `priv/pegjs.template`.

To generate the `pegjs_parse` module, all you need is:

```erlang
> neotoma:file("priv/pegjs_parse.peg", [{output, "src/"}]).
ok
> pegjs:file(.... etc ... )
```
