-module(sc_cache).

%% API
-export([
  init/1,
  put/4,
  get/2, expire/2,
  delete/2, increment/4,
  clean/1, clean/2, delete_all_objects/1,
  is_member/2,
  size/1, memory/1,
  fold/3
]).

-define(INFINITY, 100000000000000000000).

init(Name) ->
  Name = ets:new(Name, [ named_table, public, set, { read_concurrency, true }, { write_concurrency, true } ]),
  ok.

put(Name, Key, Data, infinity) ->
  ets:insert(Name, { Key, Data, ?INFINITY });
put(Name, Key, Data, TTL) when is_integer(TTL) ->
  ets:insert(Name, { Key, Data, TTL + time:now_sec() }).

get(Name, Key) ->
  case ets:lookup(Name, Key) of
    [] -> undefined;
    [{_, Value, _Time}] ->
      { ok, Value }
  end.

expire(Name, Key) ->
  case ets:lookup(Name, Key) of
    [] -> undefined;
    [{_, Value, Expire}] -> { ok, Value, Expire }
  end.

increment(Name, Key, Step, TTL) ->
  Expire = TTL + sc_time:now_sec(),
  ets:update_counter(Name, Key, [{2, Step}, {3,0,0, Expire }], { Key, Step, Expire }).

fold(Name, Acc, Fun) ->
  ets:foldl(fun({Key, Value, Expire}, A) ->
    Fun({Key, Value, Expire}, A)
  end, Acc, Name).

delete(Name, Key) ->
  ets:delete(Name, Key),
  ok.

clean(Name) ->
  clean(Name, sc_time:now_sec()).

clean(Name, Now) ->
  ets:foldl(fun({Key, Value, Time}, Acc) ->
    case Time < Now of
      true ->
        delete(Name, Key),
        [ {Key, Value} | Acc ];
      false -> Acc
    end
  end, [], Name).

delete_all_objects(Name) ->
  ets:delete_all_objects(Name).

size(Name) ->
  ets:info(Name, size).

is_member(Name, Key) ->
  gb_sets:is_member(Name, Key).

memory(Name) ->
  ets:info(Name, memory).
