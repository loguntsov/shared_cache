-module(shared_cache).

%% API
-export([
  new/2, new/3,
  destroy/1,
  put/4,
  get/2, expire/2,
  delete/2, increment/4,
  clean/1, clean_with_callback/3, delete_all_objects/1,
  is_member/2,
  size/1, memory/1
]).

new(Name, Number) ->
  new(Name, Number, []).

new(Name, Number, Opts) ->
  Tables = lists:map(fun(N) ->
    Ets = list_to_atom(atom_to_list(Name) ++ integer_to_list(N)),
    sc_cache:new(Ets, Opts),
    Ets
  end, lists:seq(1, Number)),
  ok = beam_storage:compile_if_not(Name, [
    { list, { term, Tables }},
    { tuple, { term, list_to_tuple(Tables) }}
  ]),
  ok.

destroy(Name) ->
  Tables = names_list(Name),
  lists:foreach(fun(Table) ->
      sc_cache:destroy(Table)
  end, Tables),
  true = beam_storage:delete(Name),
  ok.


names_tuple(Name) ->
  Name:tuple().

names_list(Name) ->
  Name:list().

get_table(Name, Key) ->
  Tables = names_tuple(Name),
  element(erlang:phash(Key, erlang:size(Tables)), Tables).

put(Name, Key, Value, TTL) ->
  sc_cache:put(get_table(Name, Key), Key, Value, TTL).

increment(Name, Key, Step, TTL) ->
  sc_cache:increment(get_table(Name, Key), Key, Step, TTL).

get(Name, Key) ->
  sc_cache:get(get_table(Name, Key), Key).

expire(Name, Key) ->
  sc_cache:expire(get_table(Name, Key), Key).

is_member(Name, Key) ->
  sc_cache:is_member(get_table(Name, Key), Key).

delete(Name, Key) ->
  sc_cache:delete(get_table(Name, Key), Key).

clean(Name) ->
  lists:merge(sc_pmap:pmap(fun(Table) ->
    sc_cache:clean(Table)
  end, names_list(Name))).

clean_with_callback(Name, Now, Callback) ->
  lists:merge(sc_pmap:pmap(fun(Table) ->
    sc_cache:clean_with_callback(Table, Now, Callback)
  end), names_list(Name)).

delete_all_objects(Name) ->
  lists:foreach(fun(Table) ->
    sc_cache:delete_all_objects(Table)
  end, names_list(Name)).

size(Name) ->
  lists:sum([ sc_cache:size(T) || T <- names_list(Name)]).

memory(Name) ->
  lists:sum([ sc_cache:memory(T) || T <- names_list(Name)]).


