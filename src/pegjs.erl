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
               , template
               }).

%%_* Types =====================================================================
-type option()  :: {output, Dir::string() | binary()} %% where to put the generated file
                 | {module, string() | binary()}
                 | pegjs_analyze:option().
-type options() :: [option()].

-export_type([option/0, options/0]).

%%_* API =======================================================================
-spec file(string()) -> ok | {error, term()}.
file(FileName) ->
  file(FileName, []).

-spec file(string(), options()) -> ok | {error, term()}.
file(FileName, Options) ->
  Basename   = filename:rootname(filename:basename(FileName)),
  InputDir   = filename:dirname(FileName),
  ModuleName = proplists:get_value(module, Options, list_to_atom(Basename)),
  OutputDir  = proplists:get_value(output, Options, InputDir),
  OutputFilename = filename:join(OutputDir, atom_to_list(ModuleName) ++ ".erl"),
  case {filename:absname(FileName), filename:absname(OutputFilename)} of
    {File, File} ->
      {error, input_and_output_are_the_same};
    _ ->
      generate(FileName, OutputFilename, ModuleName, Options)
  end.


%%_* Internal ==================================================================
-spec generate(string(), string(), atom(), options()) -> ok | {error, term()}.
generate(InputFile, OutputFile, ModuleName, Options) ->
  case pegjs_analyze:file(InputFile, Options) of
    {error, _} = E -> E;
    {Grammar, Analysis} ->
      {ok, File} = file:open(OutputFile, [write]),
      Result = generate(#input{ analysis = Analysis
                              , grammar = Grammar
                              , module_name = ModuleName
                              , output_file = File
                              , template = proplists:get_value(template, Options, "pegjs.template")
                              }
                       ),
      file:close(File),
      Result
  end.

-spec generate(#input{}) -> ok | {error, term()}.
generate(Input) ->
  case chain([ fun generate_module_attributes/1
             , fun generate_combinator_defines/1
             , fun generate_initializer/1
             , fun generate_api_functions/1
             , fun generate_rules/1
             , fun generate_code/1
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
    , "             {AST, <<>>, _Index}     -> AST;\n"
    , "             {_AST, Unparsed, Index} -> {error, {no_match, {Unparsed, Index}}};\n"
    , "             {error, Error}          -> {error, Error}\n"
    , "           end,\n"
    , "  release_memo(),\n"
    , "Result.\n\n"
    ],
  file:write(OutputFile, Out),
  {ok, Input}.

-spec generate_initializer(#input{}) -> {ok, #input{}} | {error, term()}.
generate_initializer(#input{ analysis = #analysis{initializer = Initializer}
                           , output_file = OutputFile} = Input) ->
  file:write(OutputFile, Initializer),
  file:write(OutputFile, "\n\n"),
  {ok, Input}.

-spec generate_rules(#input{}) -> {ok, #input{}} | {error, term()}.
generate_rules(#input{ grammar = #grammar{rules = Rules}
                     , output_file = OutputFile} = Input) ->
  file:write(OutputFile, [ "-spec pegjs_rule(binary(), "
                         , "binary(), tuple()) -> any().\n"]),
  write_rules(Rules, OutputFile),
  {ok, Input}.

-spec generate_code(#input{}) -> {ok, #input{}} | {error, term()}.
generate_code(#input{ analysis = #analysis{code = Code}
                    , output_file = OutputFile} = Input) ->
  orddict:fold( fun(Key, Value, _) ->
                  Out = generate_code_fun(Key, Value),
                  file:write(OutputFile, Out)
                end
              , ok, Code),
  {ok, Input}.


-spec write_combinators(#input{}) -> {ok, #input{}} | {error, term()}.
write_combinators(#input{output_file = OutputFile, template = TemplateFile} = Input) ->
  {ok, Template} = file:read_file(filename:join([ code:priv_dir(pegjs)
                                                , TemplateFile
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
        , "           (", generate_combinators(Expression), ")(I, D)\n"
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
generate_combinators(#action{expression = Expression, code = Code}) ->
  generate_combinator( <<"'action'">>
                     , generate_combinator_args(generate_combinators(Expression))
                     , generate_transform(Code));
generate_combinators(#choice{alternatives = Alternatives}) ->
  generate_combinator( <<"'choice'">>
                     , generate_combinator_args(generate_combinators(Alternatives)));
generate_combinators(#sequence{elements = Elements, code = Code}) ->
  generate_combinator( <<"'sequence'">>
                     , generate_combinator_args(generate_combinators(Elements))
                     , generate_transform(Code));
generate_combinators(#text{expression = Expression}) ->
  generate_combinator(<<"'text'">>, generate_combinators(Expression));
generate_combinators(#labeled{ expression = Expression
                             , label = Label}) ->
  generate_combinator( <<"'labeled'">>
                     , generate_combinator_args( to_output_binary(Label)
                                               , generate_combinators(Expression)));
generate_combinators(#prefixed{ expression = Expression
                              , type = Type
                              , code = Code}) ->
  generate_combinator( <<"'prefixed'">>
                     , generate_combinator_args( to_output_binary(Type)
                                               , generate_combinators(Expression))
                     , generate_transform(Code));
generate_combinators(#suffixed{expression = Expression, type = Type}) ->
  generate_combinator( <<"'suffixed'">>
                     , generate_combinator_args( to_output_binary(Type)
                     , generate_combinators(Expression)));
generate_combinators(#rule_ref{name = Name}) ->
  generate_combinator(<<"'rule_ref'">>, to_output_binary(Name));
generate_combinators(#anything{}) ->
  generate_combinator( <<"'anything'">>);
generate_combinators(#regexp{ raw_text = RawText
                            , ignore_case = IgnoreCase
                            }) ->
  generate_combinator( <<"'regexp'">>
                     , generate_combinator_args( ["unicode:characters_to_binary(\"", RawText, "\")"]
                                               , to_output_binary(IgnoreCase)));
generate_combinators(#code{code = Code}) ->
  generate_combinator(<<"'code'">>, Code);
generate_combinators(#literal{value = Value, ignore_case = IgnoreCase}) ->
  generate_combinator( <<"'literal'">>
                     , generate_combinator_args( ["unicode:characters_to_binary(\"", Value, "\")"]
                                               , to_output_binary(IgnoreCase)));
generate_combinators(undefined) ->
  <<"undefined">>.

-spec generate_combinator(binary()) -> iolist().
generate_combinator(Name) ->
  generate_combinator(Name, "[]").

-spec generate_combinator(binary(), iolist()) -> iolist().
generate_combinator(Name, Args) ->
  generate_combinator(Name, Args, code_identity_fun()).

-spec generate_combinator(binary(), iolist(), binary()) -> iolist().
generate_combinator(Name, Args, Fun) ->
  ["pegjs_combinator(", Name, ", ", Args, ", ", Fun ,")"].

-spec generate_combinator_args(binary() | iolist()) -> iolist().
generate_combinator_args(Args) ->
  args_to_iolist(Args).

-spec generate_combinator_args(binary(), iolist()) -> iolist().
generate_combinator_args(Args1, Args2) ->
  ["{", args_to_iolist(Args1), ", ", args_to_iolist(Args2), "}"].

-spec args_to_iolist(binary() | list()) -> iolist().
args_to_iolist(List) when is_list(List) ->
  ["[", List, "]"];
args_to_iolist(Binary) when is_binary(Binary) ->
  Binary.

-spec generate_transform(#code{}) -> binary().
generate_transform(#code{code = Code, index = Index}) ->
  case Code of
    [] -> code_identity_fun();
    _  -> Fun = code_fun_name(Index),  <<"fun ", Fun/binary, "/2">>
  end;
generate_transform(undefined) ->
  code_identity_fun();
generate_transform(<<>>) ->
  code_identity_fun().

-spec code_fun_name(index()) -> binary().
code_fun_name({{line, L}, {column, C}}) ->
  Line = integer_to_binary(L),
  Column = integer_to_binary(C),
  <<"pegjs_code_", Line/binary, "_", Column/binary>>.

%%_* Code generator ============================================================
-spec generate_code_fun(index(), {binary(), ordsets:ordset()}) -> iolist().
generate_code_fun(Index, {Code, Vars}) ->
  NodeVar = case ordsets:is_element(<<"Node">>, Vars) of
              true  -> <<"Node">>;
              false -> <<"_Node">>
            end,
  IdxVar = case ordsets:is_element(<<"Idx">>, Vars) of
             true  -> <<"Idx">>;
             false -> <<"_Idx">>
           end,
  Fun = code_fun_name(Index),
  [ "-spec ", Fun, "(iolist(), index()) -> parse_result().\n"
  , Fun, "(", NodeVar, ", ", IdxVar, ") ->\n"
  , Code, ".\n\n"
  ].

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
  <<"<<\"", Binary/binary, "\">>">>.

code_identity_fun() ->
  <<"fun(Node, _) -> Node end">>.
