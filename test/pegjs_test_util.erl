%%%-----------------------------------------------------------------------------
%%% @doc Miscellaneous utilities used in unit tests
%%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%%%
%%%-----------------------------------------------------------------------------
-module(pegjs_test_util).

%%_* Exports ===================================================================
-export([ assert_match_rule/2
        , when_fail/2
        ]).

%%_* Includes ==================================================================
-include_lib("eunit/include/eunit.hrl").
-include_lib("../src/pegjs2.hrl").

%%_* API =======================================================================

assert_match_rule([], _) ->
  true;
assert_match_rule([H|T], [H1|T1]) ->
  io:format(user, "LIST is ~p~n~p~n", [H, H1]),
  assert_match_rule(H, H1),
  assert_match_rule(T, T1);
assert_match_rule([H|T], Actual) ->
  io:format(user, "RULESET is ~p~n~p~n", [H, Actual]),
  assert_match_rule(H, Actual),
  assert_match_rule(T, Actual);
assert_match_rule({Part, Expected}, Actual) ->
  io:format(user, "RULE is ~p~n~p~n", [Part, Actual]),
  case validate_what(Part, Actual) of
    {validate, ActualValue} ->
      assert_match_rule(Expected, ActualValue);
    ActualValue ->
      io:format(user, "Value is ~p~n", [ActualValue]),
      ok = ?assertEqual(Expected, ActualValue)
  end.

validate_what(type,         #entry{type         = Value}) -> Value;
validate_what(name,         #entry{name         = Value}) -> Value;
validate_what(display_name, #entry{display_name = Value}) -> Value;
validate_what(label,        #entry{label        = Value}) -> Value;
validate_what(rules,        #entry{rules        = Value}) -> {validate, Value};
validate_what(expression,   #entry{expression   = Value}) -> {validate, Value};
validate_what(alternatives, #entry{alternatives = Value}) -> {validate, Value};
validate_what(elements,     #entry{elements     = Value}) -> {validate, Value};
validate_what(code,         #entry{code         = Value}) -> Value;
validate_what(parts,        #entry{parts        = Value}) -> Value;
validate_what(inverted,     #entry{inverted     = Value}) -> Value;
validate_what(ignore_case,  #entry{ignore_case  = Value}) -> Value;
validate_what(raw_text,     #entry{raw_text     = Value}) -> Value;
validate_what(value,        #entry{value        = Value}) -> Value;
validate_what(initializer,  #entry{initializer  = Value}) -> {validate, Value};
validate_what(index,        #entry{index        = Value}) -> Value;
validate_what(description,  #entry{description  = Value}) -> Value.




when_fail(OnFail, Condition) ->
  try
    Condition
  catch
    _:__Reason -> OnFail, error(__Reason)
  end.