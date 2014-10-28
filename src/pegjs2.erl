%%%-----------------------------------------------------------------------------
%%% @doc PEG.js parser generator for Erlang
%%%      Heavily borrowed from Sean Cribbs' Neotoma
%%% @author Dmitrii Dimandt
%%%-----------------------------------------------------------------------------
-module(pegjs2).

%%_* Exports ===================================================================
-export([ file/1
        , file/2
        ]).

%%_* Includes ==================================================================
-include("pegjs.hrl").

%%_* Defines ===================================================================
%% -record(input, { analysis
%%                , grammar
%%                , module_name
%%                , output_file
%%                , template
%%                }).

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
      compile([ {input_file, FileName}
              , {output, OutputFilename}
              , {module, ModuleName}
              | Options
             ])
  end.


%%_* Internal ==================================================================
-spec compile(Options::proplists:proplist()) -> ok | {error, term()}.
compile(Options) ->
  pegjs_util:chain( [ fun pegjs2_analyze:analyze/1
                    , fun pegjs2_analyze:report_missing_rules/1
                    , fun pegjs2_analyze:report_left_recursion/1
                    , fun pegjs2_analyze:remove_proxy_rules/1
                    , fun pegjs2_bytecode:generate/1
                    ]
                   , Options).
