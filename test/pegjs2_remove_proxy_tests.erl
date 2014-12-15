%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for removal of proxy tests
%%%      Direct port of https://github.com/pegjs/pegjs/blob/master/spec/unit/compiler/passes/remove-proxy-rules.spec.js
%%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%%%
%%%-----------------------------------------------------------------------------
-module(pegjs2_remove_proxy_tests).
-author("dmitriid").

%%_* Includes ==================================================================
-include_lib("eunit/include/eunit.hrl").
-include_lib("../src/pegjs2.hrl").

-define(whenFail(OnFail, Condition), try Condition catch _:___Reason -> OnFail, error(___Reason) end).

proxy_test_() ->
  [ run_test( "when a proxy rule isn't listed in |allowedStartRules| "
              "update references and remove it"
            , "start = proxy\n"
              "proxy = proxied\n"
              "proxied = \"a\""
            , [{allowed_start_rules, [<<"start">>]}]
            , [ [ {name, <<"start">>}
                , {expression, [ {type, <<"rule_ref">>}
                               , {name, <<"proxied">>}]
                  }
                ]
              , [{name, <<"proxied">> }]
              ]
            )
%% TODO: when `allowed_start_rules` option is implemented
%%   , run_test( "when a proxy rule is listed in |allowedStartRules| "
%%               "update references but don't remove it"
%%             , "start = proxy\n"
%%               "proxy = proxied\n"
%%               "proxied = \"a\""
%%             , [{allowed_start_rules, [<<"start">>, <<"proxy">>]}]
%%             , [ [ {name, <<"start">>}
%%                 , {expression, [ {type, <<"rule_ref">>}
%%                                , {name, <<"proxied">>}]
%%                   }
%%                 ]
%%               , [ {name, <<"proxy">>}
%%                 , {expression, [ {type, <<"rule_ref">>}
%%                                , {name, <<"proxied">>}]
%%                   }
%%                 ]
%%               , [{name, <<"proxied">> }]
%%               ]
%%             )
  ].

%%_* Internal ==================================================================
run_test(Description, GrammarText, Options, Expected) ->
  { Description
  , fun() ->
      io:format(user, "~s~n", [Description]),
      Grammar = get_grammar(GrammarText, Options),
      ?whenFail( io:format(user, "Expected result:~n~p~n"
                                 "Got result:~n~p~n", [ Expected
                                                      , Grammar#entry.rules])
               , assert_match(Expected, Grammar#entry.rules)
               )
    end
  }.

%%_* Internal ==================================================================
get_grammar(Grammar, Options) ->
  Analysis = pegjs_util:chain([ fun pegjs2_analyze:analyze/1
                              , fun pegjs2_analyze:report_missing_rules/1
                              , fun pegjs2_analyze:report_left_recursion/1
                              , fun pegjs2_analyze:remove_proxy_rules/1
                              ]
                             , [{input, Grammar} | Options]),
  Analysis#analysis.grammar.

assert_match([], []) ->
  ok;
assert_match( [ExpectedRule|ExpectedRules]
            , [ResultRule|ResultRules]) ->
  assert_match_rule(ExpectedRule, ResultRule),
  assert_match(ExpectedRules, ResultRules).

assert_match_rule([], _) ->
  ok;
assert_match_rule([{name, Name} | Rest], Entry) ->
  ?assertEqual(Name, Entry#entry.name),
  assert_match_rule(Rest, Entry);
assert_match_rule([{type, Type} | Rest], Entry) ->
  ?assertEqual(Type, Entry#entry.type),
  assert_match_rule(Rest, Entry);
assert_match_rule([{expression, Expression} | Rest], Entry) ->
  assert_match([Expression], Entry#entry.expression),
  assert_match_rule(Rest, Entry).
