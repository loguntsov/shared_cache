-module(sc_pmap).

%% API
-export([
	pmap/2, pmap/3,
	map_reduce/4,
	foreach/2, foreach/3,
	flatmap/2,
	is_size_less_than/2,
	default_workers/0
]).

-record(state, {
	ref :: reference(),
	count :: integer(),
	workers :: integer(),
	f :: function()
}).

pmap(_, [], _) -> [];
pmap(Fun, List, Workers) ->
	Ref = make_ref(),
	Count = length(List),
	St = #state{
		ref = Ref,
		count = Count,
		workers = Workers,
		f = Fun
	},
	Data = lists:zip(lists:seq(1, Count), List),
	loop1(Data, [], St).

loop1([{ Number, Item} | List], Acc, St = #state{ workers = Workers, ref = Ref, f = Fun }) when Workers > 0 ->
	Pid = self(),
	spawn_link(fun() ->
		Result = Fun(Item),
		Pid ! { result, Ref, Number, Result }
			 end),
	loop1(List, Acc, St#state{ workers = St#state.workers - 1 });

loop1(List, Acc, St = #state{ ref = Ref, count = Count }) when Count > 0 ->
	receive
		{ result, R, Number, Result } when R =:= Ref ->
			loop1(List, [{Number, Result} | Acc ], St#state{
				count = St#state.count - 1, workers = St#state.workers + 1
			})
	end;

loop1([], Acc, _) ->
	{ _, Result } = lists:unzip(lists:keysort(1, Acc)),
	Result.

pmap(Fun, List) ->
	pmap(Fun, List, default_workers()).

map_reduce(MapFun, ReduceFun, Acc, List) ->
	Result = pmap(MapFun, List),
	lists:foldl(ReduceFun, Acc, Result).

%% TODO: Need make more intelligent logic because this function return nothing always.
foreach(Fun, List, Workers) ->
	pmap(Fun, List, Workers),
	ok.

foreach(Fun, List) ->
	foreach(Fun, List, default_workers()),
	ok.

flatmap(Fun, List) ->
	Result = pmap(Fun, List),
	lists:foldl(fun
		([], Acc) -> Acc;
		(List0, Acc) ->
			lists:foldl(fun(I, A) -> [ I | A ] end, Acc, List0)
	end, [], lists:reverse(Result)).

is_size_less_than([], Count) when Count > 0 -> true;
is_size_less_than([], _) -> false;
is_size_less_than([ _ | List ], Count) ->
	is_size_less_than(List, Count - 1).

default_workers() ->
	erlang:system_info(logical_processors_available) * 2.
