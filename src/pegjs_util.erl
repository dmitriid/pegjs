%%%-----------------------------------------------------------------------------
%%% @doc Misc utils
%%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%%%
%%%-----------------------------------------------------------------------------
-module(pegjs_util).
-author("dmitrii.dimandt").

%% API
-export([chain/2]).

-spec chain([chain_func()], any()) -> ok | {error, term()}.
-type chain_func() :: fun((any()) -> {ok, any()} | {error, term()}).
chain([], Any) -> Any;
chain([F | T], Any) ->
  case F(Any) of
    {error, _} = Error -> Error;
    Result             -> chain(T, Result)
  end.
