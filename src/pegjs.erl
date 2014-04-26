%%%-----------------------------------------------------------------------------
%%% @doc PEG.js parser generator for Erlang
%%%      Heavily borrowed from Sean Cribbs' Neotoma
%%% @author Dmitrii Dimandt
%%%-----------------------------------------------------------------------------
-module(pegjs).

%%_* Exports ===================================================================
-export([ file/1
        , file/2
        ]).

%%_* Includes ==================================================================
-include("pegjs.hrl").

%%_* Defines ===================================================================
-record(input, { analysis
               , grammar
               , module_name
               , output_file
               }).

%%_* API =======================================================================
-spec file(string()) -> ok | {error, term()}.
file(FileName) ->
  file(FileName, []).

-spec file(string(), proplists:proplist()) -> ok | {error, term()}.
file(FileName, Options) ->
  Basename = filename:basename(FileName, ".peg"),
  InputDir = filename:dirname(FileName),
  ModuleName = proplists:get_value(module, Options, list_to_atom(Basename)),
  OutputDir = proplists:get_value(output, Options, InputDir),
  OutputFilename = filename:join(OutputDir, atom_to_list(ModuleName) ++ ".erl"),
  case {filename:absname(FileName), filename:absname(OutputFilename)} of
    {File, File} ->
      {error, input_and_output_are_the_same};
    _ ->
      generate(FileName, OutputFilename, ModuleName)
  end.


%%_* Internal ==================================================================
-spec generate(string(), string(), atom()) -> ok | {error, term()}.
generate(InputFile, OutputFile, ModuleName) ->
  case pegjs_analyze:file(InputFile) of
    {error, _} = E -> E;
    {Grammar, Analysis} ->
      {ok, File} = file:open(OutputFile, [write]),
      Result = generate(#input{ analysis = Analysis
                              , grammar = Grammar
                              , module_name = ModuleName
                              , output_file = File
                              }
                       ),
      file:close(File),
      Result
  end.

-spec generate(#input{}) -> ok | {error, term()}.
generate(Input) ->
  case chain([ fun generate_module_attributes/1
             , fun generate_combinator_defines/1
             , fun generate_api_functions/1
             , fun generate_rules/1
             , fun write_combinators/1
             ], Input) of
    {ok, _} -> ok;
    Error   -> Error
  end.


%%_* Generation ================================================================
-spec generate_module_attributes(#input{}) -> {ok, #input{}} | {error, term()}.
generate_module_attributes(#input{ module_name = ModuleName
                                 , output_file = OutputFile} = Input) ->
  file:write(OutputFile, ["-module(", atom_to_list(ModuleName), ")."
                         , "\n"
                         , "-export([ parse/1"
                         , "\n"
                         , "        , parse/2"
                         , "\n"
                         , "        , file/1"
                         , "\n"
                         , "        , file/2"
                         , "\n"
                         , "        ])."
                         , "\n\n"
                         ]),
  {ok, Input}.

-spec generate_combinator_defines(#input{}) -> {ok, #input{}} | {error, term()}.
generate_combinator_defines(#input{ analysis = #analysis{combinators = Cs}
                                  , output_file = OutputFile} = Input) ->
  Combinators = orddict:fetch_keys(Cs),
  lists:foldl(fun(Combinator, _) ->
                file:write(OutputFile, ["-define("
                                       , atom_to_list(Combinator)
                                       , ", true)."
                                       , "\n"
                                       ]
                               )
              end, ok, Combinators),
  file:write(OutputFile, "\n"),
  {ok, Input}.

-spec generate_rules(#input{}) -> {ok, #input{}} | {error, term()}.
generate_rules(#input{ grammar = #grammar{rules = Rules}
                     , output_file = OutputFile} = Input) ->
  file:write(OutputFile, [ "-spec pegjs_rule(binary(), "
                         , "binary(), tuple()) -> any().\n"]),
  write_rules(Rules, OutputFile),
  {ok, Input}.

-spec generate_api_functions(#input{}) -> {ok, #input{}} | {error, term()}.
generate_api_functions(#input{ grammar = #grammar{rules = Rules}
                             , output_file = OutputFile} = Input) ->
  [#rule{name = Root} | _] = Rules,
  Out =
    [ "-spec file(file:name()) -> any().\n"
    , "file(Filename) -> file(Filename, <<\"", Root, "\">>).\n\n"
    , "-spec file(file:name(), binary()) -> any().\n"
    , "file(Filename, Root) ->\n"
    , "  case file:read_file(Filename) of \n"
    , "    {ok,Bin} -> parse(Bin, Root);\n"
    , "    Err      -> Err\n"
    , "end.\n\n"
    , "-spec parse(binary() | list()) -> any().\n"
    , "parse(List) when is_list(List) -> parse(list_to_binary(List));\n"
    , "parse(Input) when is_binary(Input) ->\n"
    , "  parse(Input, <<\"", Root, "\">>).\n\n"
    , "-spec parse(binary() | list(), binary()) -> any().\n"
    , "parse(List, Root) when is_list(List) -> \n"
    , "  parse(list_to_binary(List), Root);\n"
    , "parse(Input, Root) when is_binary(Input) ->\n"
    , "  setup_memo(),\n"
    , "  Result = case pegjs_rule(Root, Input,{{line,1},{column,1}}) of\n"
    , "             {AST, <<>>, _Index} -> AST;\n"
    , "             Any -> Any\n"
    , "           end,\n"
    , "  release_memo(),\n"
    , "Result.\n\n"
    ],
  file:write(OutputFile, Out),
  {ok, Input}.

-spec write_combinators(#input{}) -> {ok, #input{}} | {error, term()}.
write_combinators(#input{output_file = OutputFile} = Input) ->
  {ok, Template} = file:read_file(filename:join([ code:priv_dir(neotoma)
                                                , "pegjs.template"
                                                ])
                                 ),
  file:write(OutputFile, Template),
  {ok, Input}.

%%_* Rule generators ===========================================================
-spec write_rules([#rule{}], port()) -> ok.
write_rules([], _) -> ok;
write_rules([Rule | T], OutputFile) ->
  write_rule(Rule, OutputFile),
  case T of
    [] ->
      file:write(OutputFile, ".\n\n");
    _  ->
      file:write(OutputFile, ";\n"),
      write_rules(T, OutputFile)
  end.

-spec write_rule(#rule{}, port()) -> ok.
write_rule( #rule{ expression = Expression
                 , name = Name
                 }
           , OutputFile) ->
  Out = [ "pegjs_rule(<<\"", Name, "\">>, Input, Index) -> \n"
        , "  pegjs( Input\n"
        , "       , Index\n"
        , "       , <<\"", Name, "\">>\n"
        , "       , fun(I, D) ->\n"
        , "           ", generate_combinators(Expression), "\n"
        , "         end\n"
        , "       , fun(Node, _Idx) -> Node end\n"
        , "       )"
        ],
  file:write(OutputFile, Out).

-spec generate_combinators(list()) -> iolist().
generate_combinators([]) -> [];
generate_combinators([Head | []]) ->
  generate_combinators(Head);
generate_combinators([Head | T]) ->
  [ generate_combinators(Head)
  , ", "
  , generate_combinators(T)
  ];
generate_combinators(#choice{alternatives = Alternatives}) ->
  generate_combinator(<<"'choice'">>, generate_combinators(Alternatives));
generate_combinators(#sequence{elements = Elements}) ->
  generate_combinator(<<"'sequence'">>, generate_combinators(Elements));
generate_combinators(#text{expression = Expression}) ->
  generate_combinator(<<"'text'">>, generate_combinators(Expression));
generate_combinators(#labeled{ expression = Expression
                             , label = Label}) ->
  generate_combinator( <<"'label'">>
                     , to_output_binary(Label)
                     , generate_combinators(Expression));
generate_combinators(#prefixed{expression = Expression, type = Type}) ->
  generate_combinator( <<"'prefixed'">>
                     , to_output_binary(Type)
                     , generate_combinators(Expression));
generate_combinators(#suffixed{expression = Expression, type = Type}) ->
  generate_combinator( <<"'suffixed'">>
                     , to_output_binary(Type)
                     , generate_combinators(Expression));
generate_combinators(#rule_ref{name = Name}) ->
  generate_combinator( <<"'rule_ref'">>, to_output_binary(Name));
generate_combinators(#anything{}) ->
  generate_combinator( <<"'anything'">>);
generate_combinators(#character_range{raw_text = Text}) ->
  generate_combinator( <<"'character_range'">>, to_output_binary(Text));
generate_combinators(#regexp{ raw_text = Text
                            , inverted = Inverted
                            , ignore_case = IgnoreCase
                            }) ->
  generate_combinator( <<"'regexp'">>
                     , to_output_binary(Text)
                     , to_output_binary(Inverted)
                     , to_output_binary(IgnoreCase));
generate_combinators(#code{code = Code}) ->
  generate_combinator(<<"'code'">>, Code);
generate_combinators(#literal{value = Value, ignore_case = IgnoreCase}) ->
  generate_combinator( <<"'literal'">>
                     , to_output_binary(Value)
                     , to_output_binary(IgnoreCase)).

-spec generate_combinator(binary()) -> iolist().
generate_combinator(Name) ->
  [ "pegjs_combinator(", Name, ")"].

-spec generate_combinator(binary(), binary() | iolist()) -> iolist().
generate_combinator(Name, Args) when is_binary(Args) ->
  [ "pegjs_combinator(", Name, ", ", Args, ")"];
generate_combinator(Name, Args) ->
  [ "pegjs_combinator(", Name, ", [", Args, "])"].

-spec generate_combinator(binary(), binary(), iolist()) -> iolist().
generate_combinator(Name, Args1, Args2) ->
  [ "pegjs_combinator(", Name, ", {", Args1, ", [", Args2, "]})"].

-spec generate_combinator(binary(), binary(), binary(), binary()) -> iolist().
generate_combinator(Name, Arg1, Arg2, Arg3) ->
  [ "pegjs_combinator(", Name, ", {", Arg1, ", ", Arg2, ", ", Arg3 ,"})"].

%%_* Helpers ===================================================================
-spec chain([chain_func()], #input{}) -> ok | {error, term()}.
-type chain_func() :: fun((#input{}) -> {ok, #input{}} | {error, term()}).
chain([], _) -> ok;
chain([F|T], Input0) ->
  case F(Input0) of
    {ok, Input}    -> chain(T, Input);
    {error, _} = E -> E
  end.

-spec to_output_binary(atom() | binary()) -> binary().
to_output_binary(Atom) when is_atom(Atom) ->
  atom_to_binary(Atom, unicode);
to_output_binary(Binary) when is_binary(Binary) ->
  <<"\"", Binary/binary, "\"">>.

