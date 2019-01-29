-module(shared_cache_test_SUITE).


%% API
-export([]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-compile(export_all).

-define(NAME, name).
-define(COUNT, 5).
-define(TTL, 5).

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(shared_cache),
  Config.

end_per_suite(Config) ->
  Config.

init_per_testcase(_, Config) ->
  shared_cache:new(?NAME, ?COUNT, [ public ]),
  Config.

end_per_testcase(_TestCaseName, _Config) ->
  shared_cache:destroy(?NAME),
  _Config.

all() -> [
  put_key_test,
  clean_key_test,
  increment_counters
].

put_key_test(_) ->
  N = 20,
  Keys = maps:from_list(lists:zip(lists:seq(1, N), lists:seq(1, N))),
  maps:fold(fun(Key, Value, _) ->
    ok = shared_cache:put(?NAME, Key, Value, 100),
    ok
  end, ok, Keys),

  maps:fold(fun(Key, Value, _) ->
    { ok, Value } = shared_cache:get(?NAME, Key),
    ok
  end, ok, Keys),

  undefined = shared_cache:get(?NAME, not_existed_key),

  ok.

clean_key_test(_) ->
  N = 20,
  Keys = maps:from_list(lists:zip(lists:seq(1, N), lists:seq(1, N))),
  maps:fold(fun(Key, Value, _) ->
    ok = shared_cache:put(?NAME, Key, Value, -100),
    ok
  end, ok, Keys),

  shared_cache:clean(?NAME),

  0 = shared_cache:size(?NAME),

  ok.

increment_counters(_) ->
  N = 20,
  Keys = maps:from_list(lists:zip(lists:seq(1, N), lists:seq(1, N))),
  maps:fold(fun(Key, Value, _) ->
    ok = shared_cache:put(?NAME, Key, Value, -100),
    ok
  end, ok, Keys),

  maps:fold(fun(Key, Value, _) ->
    NewValue = Value + Value,
    NewValue = shared_cache:increment(?NAME, Key, Value, 100),
    ok
  end, ok, Keys),

  10 = shared_cache:increment(?NAME, not_existed_key, 10, 100),

  ok.

