%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for the bytecode generator
%%%      Direct port of https:%%github.com/dmajda/pegjs/blob/master/spec/unit/compiler/passes/generate-bytecode.spec.js
%%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%%%
%%%-----------------------------------------------------------------------------
-module(pegjs2_bytecode_tests).
-author("dmitrii.dimandt").

%%_* Inclides ==================================================================
-include_lib("eunit/include/eunit.hrl").
-include_lib("../src/pegjs2.hrl").
-include("pegjs_test.hrl").

bytecode_test_() ->
  [ run_test( "for grammar"
            , "a = \"a\"\n"
              "b = \"b\"\n"
              "c = \"c\""
            , [ <<18, 0, 2, 2, 22, 0, 23, 1>>
              , <<18, 2, 2, 2, 22, 2, 23, 3>>
              , <<18, 4, 2, 2, 22, 4, 23, 5>>
              ]
            , []
            )
  , run_test( "for grammar"
            , "a = \"a\"\n"
              "b = \"b\"\n"
              "c = \"c\""
            , []
            , [ <<"a">>
              , #entry{ type = <<"literal">>
                      , value = <<"a">>
                      , description = <<"\\\"a\\\"">>}
              , <<"b">>
              , #entry{ type = <<"literal">>
                      , value = <<"b">>
                      , description = <<"\\\"b\\\"">>}
              , <<"c">>
              , #entry{ type = <<"literal">>
                      , value = <<"c">>
                      , description = <<"\\\"c\\\"">>}
              ])
  , run_test( "for rule"
            , "start = \"a\""
            , [
                <<18, 0, 2, 2, 22, 0, 23, 1>> %% expression
              ]
            , [])
  , run_test( "for named"
            , "start \"start\" = \"a\""
            , [ <<
                  28,                          %% SILENT_FAILS_ON
                  18, 1, 2, 2, 22, 1, 23, 2,   %% <expression>
                  29,                          %% SILENT_FAILS_OFF
                  14, 2, 0,                    %% IF_ERROR
                  23, 0                        %%   * FAIL
                >> ]
            , [ #entry{ type = <<"other">>
                      , description = <<"start">> }
              , <<"a">>
              , #entry{ type = <<"literal">>
                      , value = <<"a">>
                      , description = <<"\\\"a\\\"">> }
              ]
            )
  , run_test( "for choice"
            , "start = \"a\" / \"b\" / \"c\""
            , [<<
                 18, 0, 2, 2, 22, 0, 23, 1,   %% <alternatives[0]>
                 14, 21, 0,                   %% IF_ERROR
                 6,                           %%   * POP
                 18, 2, 2, 2, 22, 2, 23, 3,   %%     <alternatives[1]>
                 14, 9, 0,                    %%     IF_ERROR
                 6,                           %%       * POP
                 18, 4, 2, 2, 22, 4, 23, 5    %%         <alternatives[2]>
               >>]
            , []
            )
  , run_test( "for action, without labels"
            , "start = \"a\" { code }"
            , [<<
                 5,                           %% PUSH_CURR_POS
                 18, 0, 2, 2, 22, 0, 23, 1,   %% <expression>
                 15, 6, 0,                    %% IF_NOT_ERROR
                 24, 1,                       %%   * LOAD_SAVED_POS
                 26, 2, 1, 0,                 %%     CALL
                 9                            %% NIP
              >>]
            , [ <<"a">>
              , #entry{ type = <<"literal">>
                      , value = <<"a">>
                      , description = <<"\\\"a\\\"">>}
              , {function, [], <<" code ">>}
              ]
            )
  , run_test( "for action, with one label"
            , "start = a:\"a\" { code }"
            , [<<
                 5,                           %% PUSH_CURR_POS
                 18, 0, 2, 2, 22, 0, 23, 1,   %% <expression>
                 15, 7, 0,                    %% IF_NOT_ERROR
                 24, 1,                       %%   * LOAD_SAVED_POS
                 26, 2, 1, 1, 0,              %%     CALL
                 9                            %% NIP
              >>]
            , [ <<"a">>
              , #entry{ type = <<"literal">>
                      , value = <<"a">>
                      , description = <<"\\\"a\\\"">>}
              , {function, [<<"a">>], <<" code ">>}
              ]
            )
  , run_test( "for action, with multiple labels"
            , "start = a:\"a\" b:\"b\" c:\"c\" { code }"
            , [<<
                 5,                           %% PUSH_CURR_POS
                 18, 0, 2, 2, 22, 0, 23, 1,   %% <elements[0]>
                 15, 40, 3,                   %% IF_NOT_ERROR
                 18, 2, 2, 2, 22, 2, 23, 3,   %%   * <elements[1]>
                 15, 25, 4,                   %%     IF_NOT_ERROR
                 18, 4, 2, 2, 22, 4, 23, 5,   %%       * <elements[2]>
                 15, 10, 4,                   %%         IF_NOT_ERROR
                 24, 3,                       %%           * LOAD_SAVED_POS
                 26, 6, 3, 3, 2, 1, 0,        %%             CALL
                 9,                           %%             NIP
                 8, 3,                        %%           * POP_N
                 7,                           %%             POP_CURR_POS
                 3,                           %%             PUSH_FAILED
                 8, 2,                        %%       * POP_N
                 7,                           %%         POP_CURR_POS
                 3,                           %%         PUSH_FAILED
                 6,                           %%   * POP
                 7,                           %%     POP_CURR_POS
                 3                            %%     PUSH_FAILED
              >>]
            , [
                <<"a">>
              , #entry{ type = <<"literal">>
                      , value = <<"a">>
                      , description = <<"\\\"a\\\"">>}
              , <<"b">>
              , #entry{ type = <<"literal">>
                      , value = <<"b">>
                      , description = <<"\\\"b\\\"">>}
              , <<"c">>
              , #entry{ type = <<"literal">>
                      , value = <<"c">>
                      , description = <<"\\\"c\\\"">>}
              , {function, [<<"a">>, <<"b">>, <<"c">>], <<" code ">>}
              ]
            )
  , run_test( "for sequence"
            , "start = \"a\" \"b\" \"c\""
            , [<<
                 5,                           %% PUSH_CURR_POS
                 18, 0, 2, 2, 22, 0, 23, 1,   %% <elements[0]>
                 15, 33, 3,                   %% IF_NOT_ERROR
                 18, 2, 2, 2, 22, 2, 23, 3,   %%   * <elements[1]>
                 15, 18, 4,                   %%     IF_NOT_ERROR
                 18, 4, 2, 2, 22, 4, 23, 5,   %%       * <elements[2]>
                 15, 3, 4,                    %%         IF_NOT_ERROR
                 11, 3,                       %%           * WRAP
                 9,                           %%             NIP
                 8, 3,                        %%           * POP_N
                 7,                           %%             POP_CURR_POS
                 3,                           %%             PUSH_FAILED
                 8, 2,                        %%       * POP_N
                 7,                           %%         POP_CURR_POS
                 3,                           %%         PUSH_FAILED
                 6,                           %%   * POP
                 7,                           %%     POP_CURR_POS
                 3                            %%     PUSH_FAILED
              >>]
            , [
                <<"a">>,
                #entry{type = <<"literal">>, value = <<"a">>, description = <<"\\\"a\\\"">>},
                <<"b">>,
                #entry{type = <<"literal">>, value = <<"b">>, description = <<"\\\"b\\\"">>},
                <<"c">>,
                #entry{type = <<"literal">>, value = <<"c">>, description = <<"\\\"c\\\"">>}
              ]
            )
  , run_test( "for labeled"
            , "start = a:\"a\""
            , [<<
                 18, 0, 2, 2, 22, 0, 23, 1    %%  PUSH_FAILED
              >>]
            , []
            )
  , run_test( "for text"
            , "start = $\"a\""
            , [<<
                 5,                           %% PUSH_CURR_POS
                 18, 0, 2, 2, 22, 0, 23, 1,   %% <expression>
                 15, 2, 1,                    %% IF_NOT_ERROR
                 6,                           %%   * POP
                 12,                          %%     TEXT
                 9                            %%   * NIP
              >>]
            , []
            )
  , run_test( "for simple_and"
            , "start = &\"a\""
            , [<<
                 5,                           %% PUSH_CURR_POS
                 28,                          %% SILENT_FAILS_ON
                 18, 0, 2, 2, 22, 0, 23, 1,   %% <expression>
                 29,                          %% SILENT_FAILS_OFF
                 15, 3, 3,                    %% IF_NOT_ERROR
                 6,                           %%   * POP
                 7,                           %%     POP_CURR_POS
                 1,                           %%     PUSH_UNDEFINED
                 6,                           %%   * POP
                 6,                           %%     POP
                 3                            %%     PUSH_FAILED
              >>]
            , [
                <<"a">>,
                #entry{ type = <<"literal">>
                      , value = <<"a">>
                      , description = <<"\\\"a\\\"">>}

              ]
            )
  , run_test( "for simple_not"
            , "start = !\"a\""
            , [<<
                 5,                           %% PUSH_CURR_POS
                 28,                          %% SILENT_FAILS_ON
                 18, 0, 2, 2, 22, 0, 23, 1,   %% <expression>
                 29,                          %% SILENT_FAILS_OFF
                 14, 3, 3,                    %% IF_ERROR
                 6,                           %%   * POP
                 6,                           %%     POP
                 1,                           %%     PUSH_UNDEFINED
                 6,                           %%   * POP
                 7,                           %%     POP_CURR_POS
                 3                            %%     PUSH_FAILED              >>]
               >>]
            , [
                <<"a">>,
                #entry{ type = <<"literal">>
                      , value = <<"a">>
                      , description = <<"\\\"a\\\"">>}
              ]
            )
  , run_test( "for optional"
            , "start = \"a\"?"
            , [<<
                 18, 0, 2, 2, 22, 0, 23, 1,   %% <expression>
                 14, 2, 0,                    %% IF_ERROR
                 6,                           %%   * POP
                 2                            %%     PUSH_NULL
              >>]
            , [
                <<"a">>,
                #entry{ type = <<"literal">>
                      , value = <<"a">>
                      , description = <<"\\\"a\\\"">>}
              ]
            )
  , run_test( "for zero_or_more"
            , "start = \"a\"*"
            , [<<
                 4,                           %% PUSH_EMPTY_ARRAY
                 18, 0, 2, 2, 22, 0, 23, 1,   %% <expression>
                 16, 9,                       %% WHILE_NOT_ERROR
                 10,                          %%   * APPEND
                 18, 0, 2, 2, 22, 0, 23, 1,   %%     <expression>
                 6                            %% POP
              >>]
            , [
                <<"a">>,
                #entry{ type = <<"literal">>
                      , value = <<"a">>
                      , description = <<"\\\"a\\\"">>}
              ]
            )
  , run_test( "for one_or_more"
            , "start = \"a\"+"
            , [<<
                 4,                           %% PUSH_EMPTY_ARRAY
                 18, 0, 2, 2, 22, 0, 23, 1,   %% <expression>
                 15, 12, 3,                   %% IF_NOT_ERROR
                 16, 9,                       %%   * WHILE_NOT_ERROR
                 10,                          %%       * APPEND
                 18, 0, 2, 2, 22, 0, 23, 1,   %%         <expression>
                 6,                           %%     POP
                 6,                           %%   * POP
                 6,                           %%     POP
                 3                            %%     PUSH_FAILED
              >>]
            , [
                <<"a">>,
                #entry{ type = <<"literal">>
                      , value = <<"a">>
                      , description = <<"\\\"a\\\"">>}
              ]
            )
  , run_test( "for semantic_and, without labels"
            , "start = &{ code }"
            , [<<
                 25,            %% UPDATE_SAVED_POS
                 26, 0, 0, 0,   %% CALL
                 13, 2, 2,      %% IF
                 6,             %%   * POP
                 1,             %%     PUSH_UNDEFINED
                 6,             %%   * POP
                 3              %%     PUSH_FAILED
              >>]
            , [
                {function, [], <<" code ">>}
              ]
            )
  , run_test( "for semantic_and, with labels"
            , "start = a:\"a\" b:\"b\" c:\"c\" &{ code }"
            , [<<
                 5,                           %% PUSH_CURR_POS
                 18, 0, 2, 2, 22, 0, 23, 1,   %% <elements[0]>
                 15, 55, 3,                   %% IF_NOT_ERROR
                 18, 2, 2, 2, 22, 2, 23, 3,   %%   * <elements[1]>
                 15, 40, 4,                   %%     IF_NOT_ERROR
                 18, 4, 2, 2, 22, 4, 23, 5,   %%       * <elements[2]>
                 15, 25, 4,                   %%         IF_NOT_ERROR
                 25,                          %%           * UPDATE_SAVED_POS
                 26, 6, 0, 3, 2, 1, 0,        %%             CALL
                 13, 2, 2,                    %%             IF
                 6,                           %%               * POP
                 1,                           %%                 PUSH_UNDEFINED
                 6,                           %%               * POP
                 3,                           %%                 PUSH_FAILED
                 15, 3, 4,                    %%             IF_NOT_ERROR
                 11, 4,                       %%               * WRAP
                 9,                           %%                 NIP
                 8, 4,                        %%               * POP_N
                 7,                           %%                 POP_CURR_POS
                 3,                           %%                 PUSH_FAILED
                 8, 3,                        %%           * POP_N
                 7,                           %%             POP_CURR_POS
                 3,                           %%             PUSH_FAILED
                 8, 2,                        %%       * POP_N
                 7,                           %%         POP_CURR_POS
                 3,                           %%         PUSH_FAILED
                 6,                           %%   * POP
                 7,                           %%     POP_CURR_POS
                 3                            %%     PUSH_FAILED
              >>]
            , [
                <<"a">>,
                #entry{type = <<"literal">>, value = <<"a">>, description = <<"\\\"a\\\"">>},
                <<"b">>,
                #entry{type = <<"literal">>, value = <<"b">>, description = <<"\\\"b\\\"">>},
                <<"c">>,
                #entry{type = <<"literal">>, value = <<"c">>, description = <<"\\\"c\\\"">>},
                {function, [<<"a">>, <<"b">>, <<"c">>], <<" code ">>}
              ]
            )
    , run_test( "for semantic_not, without labels"
              , "start = !{ code }"
              , [<<
                   25,            %% UPDATE_SAVED_POS
                   26, 0, 0, 0,   %% CALL
                   13, 2, 2,      %% IF
                   6,             %%   * POP
                   3,             %%     PUSH_FAILED
                   6,             %%   * POP
                   1              %%     PUSH_UNDEFINED
                >>]
              , [
                  {function, [], <<" code ">>}
                ]
              )
    , run_test( "for semantic_not, with labels"
              , "start = a:\"a\" b:\"b\" c:\"c\" !{ code }"
              , [<<
                   5,                           %% PUSH_CURR_POS
                   18, 0, 2, 2, 22, 0, 23, 1,   %% <elements[0]>
                   15, 55, 3,                   %% IF_NOT_ERROR
                   18, 2, 2, 2, 22, 2, 23, 3,   %%   * <elements[1]>
                   15, 40, 4,                   %%     IF_NOT_ERROR
                   18, 4, 2, 2, 22, 4, 23, 5,   %%       * <elements[2]>
                   15, 25, 4,                   %%         IF_NOT_ERROR
                   25,                          %%           * UPDATE_SAVED_POS
                   26, 6, 0, 3, 2, 1, 0,        %%             CALL
                   13, 2, 2,                    %%             IF
                   6,                           %%               * POP
                   3,                           %%                 PUSH_FAILED
                   6,                           %%               * POP
                   1,                           %%                 PUSH_UNDEFINED
                   15, 3, 4,                    %%             IF_NOT_ERROR
                   11, 4,                       %%               * WRAP
                   9,                           %%                 NIP
                   8, 4,                        %%               * POP_N
                   7,                           %%                 POP_CURR_POS
                   3,                           %%                 PUSH_FAILED
                   8, 3,                        %%           * POP_N
                   7,                           %%             POP_CURR_POS
                   3,                           %%             PUSH_FAILED
                   8, 2,                        %%       * POP_N
                   7,                           %%         POP_CURR_POS
                   3,                           %%         PUSH_FAILED
                   6,                           %%   * POP
                   7,                           %%     POP_CURR_POS
                   3                            %%     PUSH_FAILED
                >>]
              , [
                  <<"a">>,
                  #entry{ type = <<"literal">>
                        , value = <<"a">>
                        , description = <<"\\\"a\\\"">>},
                  <<"b">>,
                  #entry{ type = <<"literal">>
                        , value = <<"b">>
                        , description = <<"\\\"b\\\"">>},
                  <<"c">>,
                  #entry{ type = <<"literal">>
                        , value = <<"c">>
                        , description = <<"\\\"c\\\"">>},
                  {function, [<<"a">>, <<"b">>, <<"c">>], <<" code ">>}
                ]
              )
    , run_test( "for rule_ref"
              , "start = other\n"
                "other = \"other\""
              , [
                  <<27, 1>>,       %% RULE
                  <<18, 0, 2, 2, %% MATCH_STRING
                  22, 0,       %%   * ACCEPT_STRING
                  23, 1>>        %%   * FAIL
                ]
              , []
              )
    , run_test( "for literal, empty"
              , "start = \"\""
              , [<<
                 0, 0           %% PUSH
                >>]
              , [
                  <<>>
                ]
              )
    , run_test( "for literal, non-empty case-sensitive"
              , "start = \"a\""
              , [<<
                   18, 0, 2, 2,   %% MATCH_STRING
                   22, 0,         %%   * ACCEPT_STRING
                   23, 1          %%   * FAIL
                >>]
              , [
                  <<"a">>,
                  #entry{ type = <<"literal">>
                        , value = <<"a">>
                        , description = <<"\\\"a\\\"">>
                        , ignore_case = false}
                ]
              )
    , run_test( "for literal, non-empty case-insensitive"
              , "start = \"A\"i"
              , [<<
                   19, 0, 2, 2,   %% MATCH_STRING_IC
                   21, 1,         %%   * ACCEPT_N
                   23, 1          %%   * FAIL
                >>]
              , [
                    <<"A">>,
                    #entry{ type = <<"literal">>
                          , value = <<"A">>
                          , description = <<"\\\"A\\\"">>
                          , ignore_case = true}
                ]
              )
    , run_test( "for class"
              , "start = [a]"
              , [<<
                   20, 0, 2, 2,   %% MATCH_REGEXP
                   21, 1,         %%   * ACCEPT_N
                   23, 1          %%   * FAIL
                >>]
              , []
              )
    , run_test( "for class, non-empty non-inverted case-sensitive"
              , "start = [a]"
              , []
              , [
                  <<"^[a]">>,
                  #entry{ type = <<"class">>
                        , value = <<"^[a]">>
                        , description = <<"^[a]">>}
                ]
              )
    , run_test( "for class, non-empty inverted case-sensitive"
              , "start = [^a]"
              , []
              , [
                  <<"^[^a]">>,
                  #entry{ type = <<"class">>
                        , value = <<"^[^a]">>
                        , description = <<"^[^a]">>
                        , inverted = true}
                ]
              )
    , run_test( "for class, non-empty non-inverted case-insensitive"
              , "start = [a]i"
              , []
              , [
                  <<"^[a]">>,
                  #entry{ type = <<"class">>
                        , value = <<"^[a]">>
                        , description = <<"^[a]">>
                        , ignore_case = true
                        }
                ]
              )
    , run_test( "for class, non-empty complex"
              , "start = [ab-def-hij-l]"
              , []
              , [
                  <<"^[ab-def-hij-l]">>,
                  #entry{ type = <<"class">>
                        , value = <<"^[ab-def-hij-l]">>
                        , description = <<"^[ab-def-hij-l]">>}
                ]
              )
    , run_test( "for class, empty non-inverted"
              , "start = []"
              , []
              , [
                  <<"^(?!)">>,
                  #entry{ type = <<"class">>
                        , value = <<"^(?!)">>
                        , description = <<"^(?!)">>}
                ]
              )
    , run_test( "for class, empty inverted"
              , "start = [^]"
              , []
              , [
                 <<"^[\\S\\s]">>,
                  #entry{ type = <<"class">>
                        , value = <<"^[\\S\\s]">>
                        , description = <<"^[\\S\\s]">>
                        , inverted = true}
                ]
              )
    , run_test( "for any"
              , "start = ."
              , [<<
                17, 2, 2,   %%  MATCH_ANY
                21, 1,      %%    * ACCEPT_N
                23, 0       %%    * FAIL
                >>]
              , [
                 #entry{ type = <<"any">>
                       , description = <<"any character">> }
                ]
              )
  ] .

%%_* Internal ==================================================================

run_test(Description, Grammar, Bytecode, Consts) ->
  [ case Bytecode of
      [] -> [];
      _  -> bytecode(Description, Grammar, Bytecode)
    end
  , case Consts of
      [] -> [];
      _  -> consts(Description, Grammar, Consts)
    end
  ].


bytecode(Description0, Grammar, ExpectedBytecode) ->
  Description = "Correct bytecode " ++ Description0 ++ "\n" ++ Grammar ++ "\n",
  { Description
  , fun() ->
      io:format(user, "~s~n", [Description]),
      Bytecode = get_bytecode(Grammar),
      ?whenFail( io:format(user, "Expected bytecode:~n~p~n"
                                 "Got bytecode:~n~p~n", [ ExpectedBytecode
                                                       , Bytecode])
               , ?assertMatch(ExpectedBytecode, Bytecode)
               )
    end
  }.

consts(Description0, Grammar, ExpectedConsts) ->
  Description = "Correct consts " ++ Description0 ++ "\n" ++ Grammar ++ "\n",
  { Description
  , fun() ->
      io:format(user, "~s~n", [Description]),
      Consts = get_consts(Grammar),
      ?whenFail( io:format(user, "Expected consts:~n~p~n"
                                 "Got consts:~n~p~n", [ ExpectedConsts
                                                      , Consts])
               , begin
                   Result = lists:foldl(fun(E, [H|T]) ->
                                          ?assertMatch(E, H),
                                          T
                                        end, Consts, ExpectedConsts),
                   ?assertMatch([], Result)
                 end
               )
    end
  }.

%%_* Helpers ===================================================================
get_bytecode(Grammar) ->
  #analysis{data = Data} = parse(Grammar),
  proplists:get_value(bytecode, Data).

get_consts(Grammar) ->
  #analysis{data = Data} = parse(Grammar),
  Consts = proplists:get_value(consts, Data),
  [K || {K, _} <- Consts, K /= '__pegjs$counter__'].

parse(Grammar) ->
  pegjs_util:chain( [ fun pegjs2_analyze:analyze/1
                    , fun pegjs2_analyze:report_missing_rules/1
                    , fun pegjs2_analyze:report_left_recursion/1
                    , fun pegjs2_analyze:remove_proxy_rules/1
                    , fun pegjs2_bytecode:generate/1
                    ]
                  , [{input, Grammar}]).
