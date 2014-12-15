%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for left recursion
%%%      Direct port of https://github.com/pegjs/pegjs/blob/master/spec/unit/compiler/passes/report-left-recursion.spec.js
%%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%%%
%%%-----------------------------------------------------------------------------
-module(pegjs2_left_recursion_tests).
-author("dmitriid").

%%_* Inclides ==================================================================
-include_lib("eunit/include/eunit.hrl").
-include_lib("../src/pegjs2.hrl").

-define( whenFail(OnFail, Condition), try Condition catch _:___Reason -> OnFail, error(___Reason) end).

%% TODO: add rules to error conditions to check that we return proper rules

left_recursion_test_() ->
  [ run_test( "reports direct left recursion"
            , "start = start"
            , error %% {error, <<"start">>}
            )
  , run_test( "reports indirect left recursion"
            , "start = stop\n"
              "stop = start"
            , error %% {error, <<"stop">>}
            )
  , run_test( "in sequences: reports left recursion only for the first element"
            , "start = start \"a\" \"b\""
            , error %% {error, <<"start">>}
            )
  , run_test( "in sequences: reports left recursion only for the first element"
            , "start = \"a\" start \"b\""
            , pass
            )
  , run_test( "in sequences: reports left recursion only for the first element"
            , "start = \"a\" \"b\" start"
            , pass
            )
  ].

%%_* Internal ==================================================================
run_test(Description, Grammar, Expected) ->
  { Description
  , fun() ->
      io:format(user, "~s~n", [Description]),
      Result = get_result(Grammar),
      ?whenFail( io:format(user, "Expected:~n~p~n"
                                 "Result:~n~p~n", [ Expected
                                                  , Result])
               , begin
                   case Expected of
                     pass          ->
                       ?assertMatch(#analysis{}, Result);
                     error ->
                       ?assertMatch( {error, {left_recursion, _}}
                                   , Result
                                   )
                   end
                 end
               )
    end
  }.

get_result(Grammar) ->
  pegjs_util:chain([fun pegjs2_analyze:analyze/1
                   , fun pegjs2_analyze:report_missing_rules/1
                   , fun pegjs2_analyze:report_left_recursion/1
                   ]
                  , [{input, Grammar}]).
