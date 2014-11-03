%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for the bytecode generator
%%%      Direct port of https://github.com/dmajda/pegjs/blob/master/spec/unit/compiler/passes/generate-bytecode.spec.js
%%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%%%
%%%-----------------------------------------------------------------------------
-module(pegjs2_bytecode_tests).
-author("dmitrii.dimandt").

%%_* Inclides ==================================================================
-include_lib("eunit/include/eunit.hrl").
-include_lib("../src/pegjs2.hrl").

-define( whenFail(OnFail, Condition), try Condition catch _:___Reason -> OnFail, error(___Reason) end).

bytecode_test_() ->
  [ run_test( "for grammar"
            , "a = \"a\"\n"
              "b = \"b\"\n"
              "c = \"c\""
            , [ 14, 0, 2, 2, 18, 0, 19, 1
              , 14, 2, 2, 2, 18, 2, 19, 3
              , 14, 4, 2, 2, 18, 4, 19, 5
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
                14, 0, 2, 2, 18, 0, 19, 1 %% expression
              ]
            , [])
  , run_test( "for named"
            , "start \"start\" = \"a\""
            , [
                24,                          %% SILENT_FAILS_ON
                14, 1, 2, 2, 18, 1, 19, 2,   %% <expression>
                25,                          %% SILENT_FAILS_OFF
                10, 2, 0,                    %% IF_ERROR
                19, 0                        %%   * FAIL
              ]
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
            , [
                14, 0, 2, 2, 18, 0, 19, 1,   %% <alternatives[0]>
                10, 21, 0,                   %% IF_ERROR
                2,                           %%   * POP
                14, 2, 2, 2, 18, 2, 19, 3,   %%     <alternatives[1]>
                10, 9, 0,                    %%     IF_ERROR
                2,                           %%       * POP
                14, 4, 2, 2, 18, 4, 19, 5    %%         <alternatives[2]>
              ]
            , []
            )
  , run_test( "for action, without labels"
            , "start = \"a\" { code }"
            , [
                1,                           %% PUSH_CURR_POS
                14, 0, 2, 2, 18, 0, 19, 1,   %% <expression>
                11, 6, 0,                    %% IF_NOT_ERROR
                20, 1,                       %%   * REPORT_SAVED_POS
                22, 2, 1, 0,                 %%     CALL
                5                            %% NIP
              ]
            , [ <<"a">>
              , #entry{ type = <<"literal">>
                      , value = <<"a">>
                      , description = <<"\\\"a\\\"">>}
              , {function, [], [<<" code ">>]} %% TODO: 'function() { code }'
              ]
            )
  , run_test( "for action, with one label"
            , "start = a:\"a\" { code }"
            , [
                1,                           %% PUSH_CURR_POS
                14, 0, 2, 2, 18, 0, 19, 1,   %% <expression>
                11, 7, 0,                    %% IF_NOT_ERROR
                20, 1,                       %%   * REPORT_SAVED_POS
                22, 2, 1, 1, 0,              %%     CALL
                5                            %% NIP
              ]
            , [ <<"a">>
              , #entry{ type = <<"literal">>
                      , value = <<"a">>
                      , description = <<"\\\"a\\\"">>}
              , {function, [<<"a">>], [<<" code ">>]}
              ]
            )
  , run_test( "for action, with multiple labels"
            , "start = a:\"a\" b:\"b\" c:\"c\" { code }"
            , [
                1,                           %% PUSH_CURR_POS
                14, 0, 2, 2, 18, 0, 19, 1,   %% <elements[0]>
                11, 40, 3,                   %% IF_NOT_ERROR
                14, 2, 2, 2, 18, 2, 19, 3,   %%   * <elements[1]>
                11, 25, 4,                   %%     IF_NOT_ERROR
                14, 4, 2, 2, 18, 4, 19, 5,   %%       * <elements[2]>
                11, 10, 4,                   %%         IF_NOT_ERROR
                20, 3,                       %%           * REPORT_SAVED_POS
                22, 6, 3, 3, 2, 1, 0,        %%             CALL
                5,                           %%             NIP
                4, 3,                        %%           * POP_N
                3,                           %%             POP_CURR_POS
                28,                          %%             PUSH_FAILED
                4, 2,                        %%       * POP_N
                3,                           %%         POP_CURR_POS
                28,                          %%         PUSH_FAILED
                2,                           %%   * POP
                3,                           %%     POP_CURR_POS
                28                           %%     PUSH_FAILED
              ]
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
              , {function, [<<"a">>, <<"b">>, <<"c">>], [<<" code ">>]}
              ]
            )
  , run_test( "for sequence"
            , "start = \"a\" \"b\" \"c\""
            , [
                1,                           %% PUSH_CURR_POS
                14, 0, 2, 2, 18, 0, 19, 1,   %% <elements[0]>
                11, 33, 3,                   %% IF_NOT_ERROR
                14, 2, 2, 2, 18, 2, 19, 3,   %%   * <elements[1]>
                11, 18, 4,                   %%     IF_NOT_ERROR
                14, 4, 2, 2, 18, 4, 19, 5,   %%       * <elements[2]>
                11, 3, 4,                    %%         IF_NOT_ERROR
                7, 3,                        %%           * WRAP
                5,                           %%             NIP
                4, 3,                        %%           * POP_N
                3,                           %%             POP_CURR_POS
                28,                          %%             PUSH_FAILED
                4, 2,                        %%       * POP_N
                3,                           %%         POP_CURR_POS
                28,                          %%         PUSH_FAILED
                2,                           %%   * POP
                3,                           %%     POP_CURR_POS
                28                           %%     PUSH_FAILED
              ]
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
            , [
                14, 0, 2, 2, 18, 0, 19, 1    %%  PUSH_FAILED
              ]
            , []
            )
  , run_test( "for text"
            , "start = $\"a\""
            , [
                1,                           %%  PUSH_CURR_POS
                14, 0, 2, 2, 18, 0, 19, 1,   %%  <expression>
                11, 2, 1,                    %%  IF_NOT_ERROR
                2,                           %%    * POP
                8,                           %%      TEXT
                5                            %%    * NIP
              ]
            , []
            )
  , run_test( "for simple_and"
            , "start = &\"a\""
            , [
                1,                           %%  PUSH_CURR_POS
                24,                          %%  SILENT_FAILS_ON
                14, 0, 2, 2, 18, 0, 19, 1,   %%  <expression>
                25,                          %%  SILENT_FAILS_OFF
                11, 3, 3,                    %%  IF_NOT_ERROR
                2,                           %%    * POP
                3,                           %%      POP_CURR_POS
                26,                          %%      PUSH_UNDEFINED
                2,                           %%    * POP
                2,                           %%      POP
                28                           %%      PUSH_FAILED
              ]
            , [
                <<"a">>,
                #entry{ type = <<"literal">>
                      , value = <<"a">>
                      , description = <<"\\\"a\\\"">>}

              ]
            )
  , run_test( "for simple_not"
            , "start = !\"a\""
            , [
                1,                           %%  PUSH_CURR_POS
                24,                          %%  SILENT_FAILS_ON
                14, 0, 2, 2, 18, 0, 19, 1,   %%  <expression>
                25,                          %%  SILENT_FAILS_OFF
                10, 3, 3,                    %%  IF_ERROR
                2,                           %%    * POP
                2,                           %%      POP
                26,                          %%      PUSH_UNDEFINED
                2,                           %%    * POP
                3,                           %%      POP_CURR_POS
                28                           %%      PUSH_FAILED
              ]
            , [
                <<"a">>,
                #entry{ type = <<"literal">>
                      , value = <<"a">>
                      , description = <<"\\\"a\\\"">>}
              ]
            )
  , run_test( "for optional"
            , "start = \"a\"?"
            , [
                14, 0, 2, 2, 18, 0, 19, 1,   %%  <expression>
                10, 2, 0,                    %%  IF_ERROR
                2,                           %%    * POP
                27                           %%      PUSH_NULL
              ]
            , [
                <<"a">>,
                #entry{ type = <<"literal">>
                      , value = <<"a">>
                      , description = <<"\\\"a\\\"">>}
              ]
            )
  , run_test( "for zero_or_more"
            , "start = \"a\"*"
            , [
                29,                          %%  PUSH_EMPTY_ARRAY
                14, 0, 2, 2, 18, 0, 19, 1,   %%  <expression>
                12, 9,                       %%  WHILE_NOT_ERROR
                6,                           %%    * APPEND
                14, 0, 2, 2, 18, 0, 19, 1,   %%      <expression>
                2                            %%  POP
              ]
            , [
                <<"a">>,
                #entry{ type = <<"literal">>
                      , value = <<"a">>
                      , description = <<"\\\"a\\\"">>}
              ]
            )
  , run_test( "for one_or_more"
            , "start = \"a\"+"
            , [
                29,                          %%  PUSH_EMPTY_ARRAY
                14, 0, 2, 2, 18, 0, 19, 1,   %%  <expression>
                11, 12, 3,                   %%  IF_NOT_ERROR
                12, 9,                       %%    * WHILE_NOT_ERROR
                6,                           %%        * APPEND
                14, 0, 2, 2, 18, 0, 19, 1,   %%          <expression>
                2,                           %%      POP
                2,                           %%    * POP
                2,                           %%      POP
                28                           %%      PUSH_FAILED
              ]
            , [
                <<"a">>,
                #entry{ type = <<"literal">>
                      , value = <<"a">>
                      , description = <<"\\\"a\\\"">>}
              ]
            )
  , run_test( "for semantic_and, without labels"
            , "start = &{ code }"
            , [
                    21,            %%  REPORT_CURR_POS
                    22, 0, 0, 0,   %%  CALL
                    9, 2, 2,       %%  IF
                    2,             %%    * POP
                    26,            %%      PUSH_UNDEFINED
                    2,             %%    * POP
                    28             %%      PUSH_FAILED
              ]
            , [
                {function, [], [<<" code ">>]} %% TODO: 'function() { code }'
              ]
            )
%% TODO:
  , run_test( "for semantic_and, with labels"
            , "start = a:\"a\" b:\"b\" c:\"c\" &{ code }"
            , [
                    1,                           %%  PUSH_CURR_POS
                    14, 0, 2, 2, 18, 0, 19, 1,   %%  <elements[0]>
                    11, 55, 3,                   %%  IF_NOT_ERROR
                    14, 2, 2, 2, 18, 2, 19, 3,   %%    * <elements[1]>
                    11, 40, 4,                   %%      IF_NOT_ERROR
                    14, 4, 2, 2, 18, 4, 19, 5,   %%        * <elements[2]>
                    11, 25, 4,                   %%          IF_NOT_ERROR
                    21,                          %%            * REPORT_CURR_POS
                    22, 6, 0, 3, 2, 1, 0,        %%              CALL
                    9, 2, 2,                     %%              IF
                    2,                           %%                * POP
                    26,                          %%                  PUSH_UNDEFINED
                    2,                           %%                * POP
                    28,                          %%                  PUSH_FAILED
                    11, 3, 4,                    %%              IF_NOT_ERROR
                    7, 4,                        %%                * WRAP
                    5,                           %%                  NIP
                    4, 4,                        %%                * POP_N
                    3,                           %%                  POP_CURR_POS
                    28,                          %%                  PUSH_FAILED
                    4, 3,                        %%            * POP_N
                    3,                           %%              POP_CURR_POS
                    28,                          %%              PUSH_FAILED
                    4, 2,                        %%        * POP_N
                    3,                           %%          POP_CURR_POS
                    28,                          %%          PUSH_FAILED
                    2,                           %%    * POP
                    3,                           %%      POP_CURR_POS
                    28                           %%      PUSH_FAILED
              ]
            , [
                <<"a">>,
                #entry{type = <<"literal">>, value = <<"a">>, description = <<"\\\"a\\\"">>},
                <<"b">>,
                #entry{type = <<"literal">>, value = <<"b">>, description = <<"\\\"b\\\"">>},
                <<"c">>,
                #entry{type = <<"literal">>, value = <<"c">>, description = <<"\\\"c\\\"">>},
                {function, [<<"a">>, <<"b">>, <<"c">>], [<<" code ">>]}
              ]
            )
    , run_test( "for semantic_not, without labels"
              , "start = !{ code }"
              , [
                  21,            %%  REPORT_CURR_POS
                  22, 0, 0, 0,   %%  CALL
                  9, 2, 2,       %%  IF
                  2,             %%    * POP
                  28,            %%      PUSH_FAILED
                  2,             %%    * POP
                  26             %%      PUSH_UNDEFINED
                ]
              , [
                  {function, [], [<<" code ">>]} %% TODO: 'function() { code }'
                ]
              )
%% TODO:
    , run_test( "for semantic_not, with labels"
              , "start = a:\"a\" b:\"b\" c:\"c\" !{ code }"
              , [
                    1,                           %%  PUSH_CURR_POS
                    14, 0, 2, 2, 18, 0, 19, 1,   %%  <elements[0]>
                    11, 55, 3,                   %%  IF_NOT_ERROR
                    14, 2, 2, 2, 18, 2, 19, 3,   %%    * <elements[1]>
                    11, 40, 4,                   %%      IF_NOT_ERROR
                    14, 4, 2, 2, 18, 4, 19, 5,   %%        * <elements[2]>
                    11, 25, 4,                   %%          IF_NOT_ERROR
                    21,                          %%            * REPORT_CURR_POS
                    22, 6, 0, 3, 2, 1, 0,        %%              CALL
                    9, 2, 2,                     %%              IF
                    2,                           %%                * POP
                    28,                          %%                  PUSH_FAILED
                    2,                           %%                * POP
                    26,                          %%                  PUSH_UNDEFINED
                    11, 3, 4,                    %%              IF_NOT_ERROR
                    7, 4,                        %%                * WRAP
                    5,                           %%                  NIP
                    4, 4,                        %%                * POP_N
                    3,                           %%                  POP_CURR_POS
                    28,                          %%                  PUSH_FAILED
                    4, 3,                        %%            * POP_N
                    3,                           %%              POP_CURR_POS
                    28,                          %%              PUSH_FAILED
                    4, 2,                        %%        * POP_N
                    3,                           %%          POP_CURR_POS
                    28,                          %%          PUSH_FAILED
                    2,                           %%    * POP
                    3,                           %%      POP_CURR_POS
                    28                           %%      PUSH_FAILED
                ]
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
                  {function, [<<"a">>, <<"b">>, <<"c">>], [<<" code ">>]}
                ]
              )

%% TODO:
%%     , run_test( "for rule_ref"
%%               , "start = other\n"
%%                 "other = \"other\""
%%               , [
%%                   23, 1 %% RULE
%%                 ]
%%               , []
%%               )
    , run_test( "for literal, empty"
              , "start = \"\""
              , [
                 0, 0           %% PUSH
                ]
              , [
                  <<>>
                ]
              )
    , run_test( "for literal, non-empty case-sensitive"
              , "start = \"a\""
              , [
                    14, 0, 2, 2,   %%  MATCH_STRING
                    18, 0,         %%    * ACCEPT_STRING
                    19, 1          %%    * FAIL
                ]
              , [
                  <<"a">>,
                  #entry{ type = <<"literal">>
                        , value = <<"a">>
                        , description = <<"\\\"a\\\"">>}
                ]
              )
%% TODO:
%%     , run_test( "for literal, non-empty case-insensitive"
%%               , "start = \"A\"i"
%%               , [
%%                     15, 0, 2, 2,   %%  MATCH_STRING_IC
%%                     17, 1,         %%    * ACCEPT_N
%%                     19, 1          %%    * FAIL
%%                 ]
%%               , [
%%                     <<"a">>,
%%                     #entry{ type = <<"literal">>
%%                           , value = <<"A">>
%%                           , description = <<"\\\"A\\\"">>}
%%                 ]
%%               )
    , run_test( "for class"
              , "start = [a]"
              , [
                  16, 0, 2, 2,   %%  MATCH_REGEXP
                  17, 1,         %%    * ACCEPT_N
                  19, 1          %%    * FAIL
                ]
              , []
              )
    , run_test( "for class, non-empty non-inverted case-sensitive"
              , "start = [a]"
              , []
              , [
                  <<"^[a]">>,
                  #entry{ type = <<"class">>
                        , value = <<"[a]">>
                        , description = <<"[a]">>}
                ]
              )
    , run_test( "for class, non-empty inverted case-sensitive"
              , "start = [^a]"
              , []
              , [
                  <<"^[^a]">>,
                  #entry{ type = <<"class">>
                        , value = <<"[^a]">>
                        , description = <<"[^a]">>}
                ]
              )
%% TODO:
%%     , run_test( "for class, non-empty non-inverted case-insensitive"
%%               , "start = [a]i"
%%               , []
%%               , [
%%                   <<"^[a]">>,
%%                   #entry{ type = <<"class">>
%%                         , value = <<"[a]i">>
%%                         , description = <<"[a]i">>}
%%                 ]
%%               )
%% TODO:
%%     , run_test( "for class, non-empty complex"
%%               , "start = [ab-def-hij-l]"
%%               , []
%%               , [
%%                   <<"^[ab-def-hij-l]">>,
%%                   #entry{ type = <<"class">>
%%                         , value = <<"[ab-def-hij-l]">>
%%                         , description = <<"[ab-def-hij-l]">>}
%%                 ]
%%               )
    , run_test( "for class, empty non-inverted"
              , "start = []"
              , []
              , [
                  <<"^(?!)">>,
                  #entry{ type = <<"class">>
                        , value = <<"[]">>
                        , description = <<"[]">>}
                ]
              )
    , run_test( "for class, empty inverted"
              , "start = [^]"
              , []
              , [
                 <<"^[\\S\\s]">>,
                  #entry{ type = <<"class">>
                        , value = <<"[^]">>
                        , description = <<"[^]">>}
                ]
              )
    , run_test( "for any"
              , "start = ."
              , [
                13, 2, 2,   %%  MATCH_ANY
                17, 1,      %%    * ACCEPT_N
                19, 0       %%    * FAIL
            ]
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
  {Bytecode, _, _} = parse(Grammar),
  Bytecode.

get_consts(Grammar) ->
  {_, Consts, _} = parse(Grammar),
  [K || {K, _} <- Consts, K /= '__pegjs$counter__'].

parse(Grammar) ->
  pegjs_util:chain( [ fun pegjs2_analyze:analyze/1
                    , fun pegjs2_analyze:report_missing_rules/1
                    , fun pegjs2_analyze:report_left_recursion/1
                    , fun pegjs2_analyze:remove_proxy_rules/1
                    , fun pegjs2_bytecode:generate/1
                    ]
                  , [{input, Grammar}]).
