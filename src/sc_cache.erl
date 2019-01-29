-module(sc_cache).

%% API
-export([
  new/1, new/2, destroy/1,
  put/4,
  get/2, expire/2,
  delete/2, increment/4,
  clean/1, clean/2, clean_with_callback/3,
  delete_all_objects/1,
  is_member/2,
  size/1, memory/1,
  fold/3
]).

-define(INFINITY, infinity).

new(Name) ->
  new(Name, []).

new(Name, Opts) ->
  ReadConcurrencyFlag = proplists:get_value(read_concurrency, Opts, true),
  WriteConcurrencyFlag = proplists:get_value(write_concurrency, Opts, true),
  Public = case proplists:is_defined(public, Opts) of
    true -> public;
    false -> protected
  end,
  Name = ets:new(Name, [ Public | [ named_table, set, { read_concurrency, ReadConcurrencyFlag }, { write_concurrency, WriteConcurrencyFlag } ]]),
  ok.

destroy(Name) ->
  ets:delete(Name).

put(Name, Key, Data, infinity) ->
  true = ets:insert(Name, { Key, Data, ?INFINITY }),
  ok;
put(Name, Key, Data, TTL) when is_integer(TTL) ->
  true = ets:insert(Name, { Key, Data, get_ttl_time(TTL, sc_time:now_sec()) }),
  ok.

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
  Expire = get_ttl_time(TTL, sc_time:now_sec()),
  [ Value, _ ] = ets:update_counter(Name, Key, [{2, Step}, {3,0,0, Expire }], { Key, 0, Expire }),
  Value.

fold(Name, Acc, Fun) ->
  ets:foldl(fun({_Key, _Value, _Expire} = Item, A) ->
    Fun(Item, A)
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


clean_with_callback(Name, Now, DestroyFun) ->
  ets:foldl(fun({Key, Value, Time} = Item, Acc) ->
    case Time < Now of
      true ->
        Result = DestroyFun(Item),
        delete(Name, Key),
        [ {Key, Value, Result} | Acc ];
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

%% Internal

get_ttl_time(infinity, _) -> infinity;
get_ttl_time(TTL, Now) ->
  TTL + Now.
