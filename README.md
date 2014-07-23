# An implementation of [PEG.js](http://pegjs.majda.cz) grammar for Erlang

This is a rather straightforward port/implementation of the grammar defined for
[PEG.js](http://pegjs.majda.cz/documentation#grammar-syntax-and-semantics).

## Current status

- As far as I can tell, implements everything from the PEG.js grammar
- Generates complete useable parsers
- The project is bootstrapped (see `priv/pegjs_parse.pegjs`). Original grammar for 
  [Neotoma](https://github.com/seancribbs/neotoma) is also available in `priv/pegjs_parse.peg`
- It's based on an earlier definition of the grammar (probably [this](https://github.com/dmajda/pegjs/blob/f0a6bc92cc24b623689c7811bebc1ce2921442f0/src/parser.pegjs))
  than the one that currently [exists](https://github.com/dmajda/pegjs/blob/master/src/parser.pegjs) 
  for PEG.js. Currenlty there's no timeframe or even a decision on when/whether to update the erlang version

## Further work

- Support @append (see, e.g. [core-pegjs](https://github.com/for-GET/core-pegjs) 
  in the for-GET project)
- Add more options:
  - set root for multiple grammars (when @append is implemented)
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

There are several options you can pass along to `pegjs:file(File, Options::options())`:

```erlang

-type options() :: [option()].

%% options for pegjs

-type option()  :: {output, Dir::string() | binary()} %% where to put the generated file
                                                      %% Default: directory of the input file
                 | {module, string() | binary()}      %% to change the module name
                                                      %% Default: name of the input file
                 | pegjs_analyze:option().

%% options for pegjs_analyze

-type option()  :: {ignore_unused, boolean()}         %% ignore unused rules. Default: true
                 | {ignore_duplicates, boolean()}     %% ignore duplicate rules. Default: false
                 | {ignore_unparsed, boolean()}       %% ignore incomplete parses. Default: false
                 | {ignore_missing_rules, boolean()}, %% Default: false
                 | {parser, atom()}.                  %% use a different module to parse grammars. Default: pegjs_parse

```

## How to contribute/develop

Suggestions and improvements are more than welcome!

Current grammar in `priv/pegjs_parse.peg` is created for [Neotoma](https://github.com/seancribbs/neotoma),
so you need that to tweak pegjs.

`pegjs_analyze` module is inspired by `neotoma_analyze` from the [2.0-refactor](https://github.com/seancribbs/neotoma/tree/2.0-refactor)
branch of neotoma.

Non-generated parser combinators can be found in `priv/pegjs.template`.

Safe working parser is always available at `src/pegjs_parse.erl.safe`.

### pegjs grammar

The current grammar from which the project is now bootstrapped lives in 
`priv/pegjs_parser.pegjs`. When you've tweaked it and you want to try your changes, 
generate a different module and tell `pegjs` to use your new module instead:

```erlang
> pegjs:file("priv/pegjs_parse.pegjs", [{output, "src"}, {module, modified_parser}]).
ok
> c(modified_parser).
{ok, modified_parser)
> pegjs:file("extra/json.pegjs", [{parser, modified_parser}]).
ok
... etc. ...
```

Once you're satisfied with your changes, overwrite pegjs_parser (which is used by default):

```erlang
> pegjs:file("priv/pegjs_parse.pegjs", [{output, "src"}]).
ok
> c(pegjs_parse).
{ok, pegjs_parse)
> pegjs:file("extra/json.pegjs").
ok
... etc. ...
```

### Original neotoma

The original parser for pegjs was derived from a grammar defined for [Neotoma](https://github.com/seancribbs/neotoma).
You can also start your work from there:

```erlang
> neotoma:file("priv/pegjs_parse.peg", [{output, "src/"}]).
ok
> pegjs:file(.... etc ... )
```

However, the original grammar will get increasingly outdated as time goes on, so it's there for reference only.
