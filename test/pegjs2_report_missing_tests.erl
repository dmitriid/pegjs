%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for report missing rules
%%%      Direct port of https://github.com/pegjs/pegjs/blob/master/spec/unit/compiler/passes/report-missing-rules.spec.js
%%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%%%
%%%-----------------------------------------------------------------------------
-module(pegjs2_report_missing_tests).
-author("dmitrii.dimandt").

%%_* Inclides ==================================================================
-include_lib("eunit/include/eunit.hrl").
-include_lib("../src/pegjs2.hrl").

-define(whenFail(OnFail, Condition), try Condition catch _:___Reason -> OnFail, error(___Reason) end).

missing_test_() ->
  [ run_test( "compiler pass |reportMissingRules| reports missing rules"
            , "start = missing"
            , <<"missing">>
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
               , ?assertMatch( {error, {missing_rules, [{Expected, _}]}}
                             , Result)
               )
    end
  }.

get_result(Grammar) ->
  pegjs_util:chain([ fun pegjs2_analyze:analyze/1
                   , fun pegjs2_analyze:report_missing_rules/1
                   ]
                  , [{input, Grammar}]).

