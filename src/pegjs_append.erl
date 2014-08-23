-module('pegjs_append').
-export([ parse/1
        , parse/2
        , file/1
        , file/2
        ]).

-define(anything, true).
-define(choice, true).
-define(labeled, true).
-define(literal, true).
-define(prefixed, true).
-define(regexp, true).
-define(sequence, true).
-define(suffixed, true).



-spec file(file:name()) -> any().
file(Filename) -> file(Filename, <<"appends">>).

-spec file(file:name(), binary()) -> any().
file(Filename, Root) ->
  case file:read_file(Filename) of 
    {ok,Bin} -> parse(Bin, Root);
    Err      -> Err
end.

-spec parse(binary() | list()) -> any().
parse(List) when is_list(List) -> parse(list_to_binary(List));
parse(Input) when is_binary(Input) ->
  parse(Input, <<"appends">>).

-spec parse(binary() | list(), binary()) -> any().
parse(List, Root) when is_list(List) -> 
  parse(list_to_binary(List), Root);
parse(Input, Root) when is_binary(Input) ->
  setup_memo(),
  Result = case pegjs_rule(Root, Input,{{line,1},{column,1}}) of
             {AST, <<>>, _Index}     -> AST;
             {_AST, Unparsed, Index} -> {error, {no_match, {Unparsed, Index}}};
             {error, Error}          -> {error, Error}
           end,
  release_memo(),
Result.

-spec pegjs_rule(binary(), binary(), tuple()) -> any().
pegjs_rule(<<"appends">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"appends">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('suffixed', {one_or_more, [pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"append">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_9_30/2), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('anything', [], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_9_42/2)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_15_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"append">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"append">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('suffixed', {zero_or_more, [pegjs_combinator('rule_ref', <<"ws">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"comment">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"star">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('suffixed', {zero_or_more, [pegjs_combinator('rule_ref', <<"ws">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("@append")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('suffixed', {one_or_more, [pegjs_combinator('rule_ref', <<"ws">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('text', pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('suffixed', {one_or_more, [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"eol">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('rule_ref', <<"eof">>, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end), pegjs_combinator('labeled', {undefined, [pegjs_combinator('anything', [], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun pegjs_code_22_1/2))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"comment">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"comment">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("/*")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("//")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"star">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"star">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('suffixed', {zero_or_more, [pegjs_combinator('literal', {[unicode:characters_to_binary("*")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"ws">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"ws">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('regexp', {[unicode:characters_to_binary("[ \\t\\v\\f\x{00A0}\x{FEFF}\x{1680}\x{180E}\x{2000}-\x{200A}\x{202F}\x{205F}\x{3000}]")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"eol">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"eol">>
       , fun(I, D) ->
           (pegjs_combinator('choice', [pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\n")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\r\n")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('literal', {[unicode:characters_to_binary("\r")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('regexp', {[unicode:characters_to_binary("[\x{2028}]")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end), pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('regexp', {[unicode:characters_to_binary("[\x{2029}]")], false}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       );
pegjs_rule(<<"eof">>, Input, Index) -> 
  pegjs( Input
       , Index
       , <<"eof">>
       , fun(I, D) ->
           (pegjs_combinator('sequence', [pegjs_combinator('labeled', {undefined, [pegjs_combinator('prefixed', {simple_not, [pegjs_combinator('anything', [], fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)]}, fun(Node, _) -> Node end)], fun(Node, _) -> Node end))(I, D)
         end
       , fun(Node, _Idx) -> Node end
       ).

-spec pegjs_code_9_30(iolist(), index()) -> parse_result().
pegjs_code_9_30(Node, _Idx) ->
 [B] = Node, B .

-spec pegjs_code_9_42(iolist(), index()) -> parse_result().
pegjs_code_9_42(_Node, _Idx) ->
 <<>> .

-spec pegjs_code_15_1(iolist(), index()) -> parse_result().
pegjs_code_15_1(Node, _Idx) ->

    [List] = Node,
    lists:filter(fun(<<>>) -> false; (_) -> true end, List)
  .

-spec pegjs_code_22_1(iolist(), index()) -> parse_result().
pegjs_code_22_1(Node, _Idx) ->

    [_, _, _, _, _, File] = Node,
    File
  .

%% -type index() :: {{line, pos_integer()}, {column, pos_integer()}}.
-type input() :: binary().
-ifndef(index).
-type index() :: {{line, integer()}, {column, integer()}}.
-endif.
-type parse_failure() :: {error, term()}.
-type parse_success() :: {term(), input(), index()}.
-type parse_result() :: parse_failure() | parse_success().
-type parse_fun() :: fun((input(), index()) -> parse_result()).
-type xform_fun() :: fun((input(), index()) -> term()).

-spec pegjs(input(), index(), binary(), parse_fun(), xform_fun()) -> parse_result().
pegjs(Inp, StartIndex, Name, ParseFun, TransformFun) ->
  case get_memo(StartIndex, Name) of      % See if the current reduction is memoized
    {ok, Memo} -> %Memo;                     % If it is, return the stored result
      Memo;
    _ ->                                        % If not, attempt to parse
      Result = case ParseFun(Inp, StartIndex) of
        {error,_} = Failure ->                       % If it fails, memoize the failure
          Failure;
        {Match, InpRem, NewIndex} ->               % If it passes, transform and memoize the result.
          Transformed = TransformFun(Match, StartIndex),
          {Transformed, InpRem, NewIndex}
      end,
      memoize(StartIndex, Name, Result),
      Result
  end.

-spec setup_memo() -> ets:tid().
setup_memo() ->
  put({parse_memo_table, ?MODULE}, ets:new(?MODULE, [set])).

-spec release_memo() -> true.
release_memo() ->
  ets:delete(memo_table_name()).

-spec memoize(index(), atom(), term()) -> true.
memoize(Index, Name, Result) ->
  Memo = case ets:lookup(memo_table_name(), Index) of
              [] -> [];
              [{Index, Plist}] -> Plist
         end,
  ets:insert(memo_table_name(), {Index, [{Name, Result}|Memo]}).

-spec get_memo(index(), atom()) -> {ok, term()} | {error, not_found}.
get_memo(Index, Name) ->
  case ets:lookup(memo_table_name(), Index) of
    [] -> {error, not_found};
    [{Index, Plist}] ->
      case proplists:lookup(Name, Plist) of
        {Name, Result}  -> {ok, Result};
        _  -> {error, not_found}
      end
    end.

-spec memo_table_name() -> ets:tid().
memo_table_name() ->
    get({parse_memo_table, ?MODULE}).

%%_* Implemented combinators ===================================================

pegjs_combinator('anything', _, _) ->
  fun(<<>>, Index) -> {error, {expected, any_character, Index}};
     (Input, Index) when is_binary(Input) ->
          <<C/utf8, Rest/binary>> = Input,
          {<<C/utf8>>, Rest, p_advance_index(<<C/utf8>>, Index)}
  end;

pegjs_combinator('choice', Parsers, TransformFun) ->
  fun(Input, Index) ->
      pegjs_combinator('attempt', {Parsers, Input, Index, none}, TransformFun)
  end;

pegjs_combinator('attempt', {[], _Input, _Index, Failure}, _) -> Failure;
pegjs_combinator('attempt', {[P|Parsers], Input, Index, FirstFailure}, TransformFun)->
  case P(Input, Index) of
    {error, _} = Failure ->
      case FirstFailure of
        none -> pegjs_combinator('attempt', {Parsers, Input, Index, Failure}, TransformFun);
        _ -> pegjs_combinator('attempt', {Parsers, Input, Index, FirstFailure}, TransformFun)
      end;
    Result -> Result
  end;

pegjs_combinator('sequence', P, TransformFun) ->
  fun(Input, Index) ->
      pegjs_combinator('all', {P, Input, Index, []}, TransformFun)
  end;

pegjs_combinator('all', {[], Inp, Index, Accum}, TransformFun) ->
  Result = lists:reverse(Accum),
  case TransformFun(Result, Index) of
    {error, _} = E -> E;
    Node -> {Node, Inp, Index}
  end;
pegjs_combinator('all', {[P|Parsers], Inp, Index, Accum}, TransformFun) ->
  case P(Inp, Index) of
    {error, _} = Failure -> Failure;
    {Result, InpRem, NewIndex} -> pegjs_combinator('all', {Parsers, InpRem, NewIndex, [Result|Accum]}, TransformFun)
  end;

pegjs_combinator('text', P, _) ->
  fun(Input, Index) ->
      Result = P(Input, Index),
      case Result of
        {error, _} = E -> E;
        {MaybeIoList, NewInput, NewIndex} ->
          IoList = convert_to_iolist(MaybeIoList),
          {iolist_to_binary(IoList), NewInput, NewIndex}
      end
  end;

pegjs_combinator('labeled', {Tag, [P]}, _) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {error,_} = Failure ->
           Failure;
        {Result, InpRem, NewIndex} ->
          case Tag of
            undefined -> {Result, InpRem, NewIndex};
            _         -> {{Tag, Result}, InpRem, NewIndex}
          end
      end
  end;

pegjs_combinator('prefixed', {simple_not, [P]}, _) ->
  fun(Input, Index)->
      case P(Input,Index) of
        {error,_} ->
          {[], Input, Index};
        {Result, _, _} -> {error, {expected, {no_match, Result}, Index}}
      end
  end;
pegjs_combinator('prefixed', {simple_and, [P]}, _) ->
  fun(Input, Index) ->
    case P(Input, Index) of
      {error, _} = Failure -> Failure;
      _ -> {[], Input, Index}
    end
  end;
pegjs_combinator('prefixed', {semantic_and, _}, TransformFun) ->
  fun(Input, Index) ->
    case TransformFun(Input, Index) of
      {error, Reason} -> {error, {Reason, Index}};
      _ -> {[], Input, Index}
    end
  end;
pegjs_combinator('prefixed', {semantic_not, _}, TransformFun) ->
  fun(Input, Index) ->
    case TransformFun(Input, Index) of
      {error, _} -> {[], Input, Index};
      Result -> {error, {expected, {no_match, Result}, Index}}
    end
  end;

pegjs_combinator('suffixed', {zero_or_more, [P]}, TransformFun) ->
  fun(Input, Index) ->
      pegjs_combinator('scan', {P, Input, Index, []}, TransformFun)
  end;
pegjs_combinator('suffixed', {one_or_more, [P]}, TransformFun) ->
  fun(Input, Index)->
      Result = pegjs_combinator('scan', {P, Input, Index, []}, TransformFun),
      case Result of
        {[_|_], _, _} ->
          Result;
        _ ->
          {error, {_, Failure, _}} = P(Input,Index),
          {error, {expected, {at_least_one, Failure}, Index}}
      end
  end;
pegjs_combinator('suffixed', {optional, [P]}, _) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {error,_} -> {[], Input, Index};
        {_, _, _} = Success -> Success
      end
  end;

pegjs_combinator('scan', { _, [], Index, Accum}, _) -> {lists:reverse( Accum ), [], Index};
pegjs_combinator('scan', {P, Inp, Index, Accum}, TransformFun) ->
  case P(Inp, Index) of
    {error,_N} -> {lists:reverse(Accum), Inp, Index};
    {Result, InpRem, NewIndex} -> pegjs_combinator('scan', {P, InpRem, NewIndex, [Result | Accum]}, TransformFun)
  end;

pegjs_combinator('literal', {[String], IsCaseInsensitive}, _) ->
    Length = erlang:byte_size(String),
    fun(Input, Index) ->
      case Input of
        <<String:Length/binary, Rest/binary>> ->
          {String, Rest, p_advance_index(String, Index)};
        _ ->
          Modifier = case IsCaseInsensitive of
                       true  -> case_insensitive;
                       false -> case_sensitive
                     end,
          case IsCaseInsensitive of
            false ->
              {error, {match_failed, {{String, Modifier}, Input}, Index}};
            true ->
              case re:run( Input
                         , <<"^[", String/binary, "](.*)">>
                         , [caseless]) of
                {match, [{_, Len}, _]} ->
                  <<Result:Len/binary, Rest/binary>> = Input,
                  { Result
                  , Rest
                  , p_advance_index(Result, Index)
                  };
                {match, _} ->
                  { Input
                  , <<>>
                  , p_advance_index(String, Index)
                  };
                nomatch    ->
                  {error, {match_failed, {{String, Modifier}, Input}, Index}}
              end
          end
      end
    end;

pegjs_combinator('regexp', {Regexp0, IsIgnoreCase}, _) ->
    Regexp = unicode:characters_to_binary(Regexp0),
    Options0 = [unicode, dotall, anchored],
    Options = case IsIgnoreCase of
                true  -> [caseless | Options0];
                false -> Options0
              end,
    {ok, RE} = re:compile(Regexp, Options),
    fun(Inp, Index) ->
        case re:run(Inp, RE) of
            {match, [{0, Length}|_]} ->
                {Head, Tail} = erlang:split_binary(Inp, Length),
                {Head, Tail, p_advance_index(Head, Index)};
            _ ->
              Modifier = case IsIgnoreCase of
                             true  -> case_insensitive;
                             false -> case_sensitive
                           end,
              {error, { expected
                      , {regexp, binary_to_list(Regexp), Modifier}
                      , Index}}
        end
    end;

pegjs_combinator('rule_ref', Rule, _) ->
    fun(Input, Index) ->
        pegjs_rule(Rule, Input, Index)
    end;

pegjs_combinator(Name, _, _) ->
  {error, {invalid_combinator, Name}}.

p_advance_index(MatchedInput, Index) when is_list(MatchedInput) orelse is_binary(MatchedInput)-> % strings
  lists:foldl(fun p_advance_index/2, Index, unicode:characters_to_list(MatchedInput));
p_advance_index(MatchedInput, Index) when is_integer(MatchedInput) -> % single characters
  {{line, Line}, {column, Col}} = Index,
  case MatchedInput of
    $\n -> {{line, Line+1}, {column, 1}};
    _ -> {{line, Line}, {column, Col+1}}
  end.

convert_to_iolist(MaybeIoList) ->
  convert_to_iolist(MaybeIoList, []).

convert_to_iolist([], Acc) ->
  lists:reverse(Acc);
convert_to_iolist([H | T], Acc) ->
  convert_to_iolist(T, [convert_to_iolist(H) | Acc]);
convert_to_iolist({Label, Value}, Acc) when is_binary(Label) ->
  [convert_to_iolist(Value) | Acc];
convert_to_iolist(Other, Acc) ->
  convert_to_iolist([], [Other | Acc]).
