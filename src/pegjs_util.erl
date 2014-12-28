%%%-----------------------------------------------------------------------------
%%% @doc Misc utils
%%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%%%
%%%-----------------------------------------------------------------------------
-module(pegjs_util).
-author("dmitrii.dimandt").

%% API
-export([ chain/2
        , escape_for_regex/1
        ]).

-spec chain([chain_func()], any()) -> ok | {error, term()}.
-type chain_func() :: fun((any()) -> {ok, any()} | {error, term()}).
chain([], Any) -> Any;
chain([F | T], Any) ->
  case F(Any) of
    {error, _} = Error -> Error;
    Result             -> chain(T, Result)
  end.

-spec escape_for_regex(binary() | integer()) -> binary().
escape_for_regex($\\) -> <<$\\, $\\>>;      % backslash
%% escape_for_regex($/) -> <<$\\, $\\, $/>>;   % closing slash
%% escape_for_regex($]) -> <<$\\, $\\, $]>>;   % closing bracket
%% escape_for_regex($^) -> <<$\\, $\\, $^>>;   % caret
%% escape_for_regex($-) -> <<$\\, $\\, $->>;   % dash
%% escape_for_regex($\0) -> <<$\\, $\\, $0>>;  % null
%% escape_for_regex($\t) -> <<$\\, $\\, $t>>;  % horizontal tab
%% escape_for_regex($\n) -> <<$\\, $\\, $n>>;  % line feed
%% escape_for_regex($\v) -> <<$\\, $\\, $v>>;  % vertical tab
%% escape_for_regex($\f) -> <<$\\, $\\, $f>>;  % form feed
%% escape_for_regex($\r) -> <<$\\, $\\, $r>>;  % carriage return
%% escape_for_regex($\") -> <<$\\, $\\, $\\, $\">>;      % ampersand
escape_for_regex(I) when is_integer(I) ->
  <<I/integer>>;
escape_for_regex(B) when is_binary(B) ->
  << <<(escape_for_regex(C))/binary>> || <<C>> <= B >>.
