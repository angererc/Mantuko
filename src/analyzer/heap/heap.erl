-module (heap).

-include("include/debug.hrl").
-include("include/values.hrl").

-export ([new/0, new_struct/2, new_array/2, new_lock/2]).
-export ([get/2, set/3]).
-export ([zip/4, merge/4]).

-record (heap, {mem=dict:new()}).
	
% ***********************************************
% ***********************************************
new() ->
	#heap{}.

new_struct(Loc, Heap) ->
	struct_loc = object:loc_type(Loc),
	Heap#heap{mem=utils:dict_store_if_not_present(Loc, fun struct:new/0, Heap#heap.mem)}.
	
new_array(Loc, Heap) ->
	array_loc = object:loc_type(Loc),
	Heap#heap{mem=utils:dict_store_if_not_present(Loc, fun array:new/0, Heap#heap.mem)}.
	
new_lock(Loc, Heap) ->
	lock_loc = object:loc_type(Loc),
	Heap#heap{mem=utils:dict_store_if_not_present(Loc, fun lock:new/0, Heap#heap.mem)}.
				
get(Loc, Heap) ->
	case dict:find(Loc, Heap#heap.mem) of
		{ok, Value} ->
			Value;
		error ->
			debug:fatal("didn't find object for ref ~w", [Loc]),
			error
	end.
	
set(Loc, Obj, Heap) ->
	?f("heap setting object at location ~s to ~s", [debug:val_to_string(Loc), debug:val_to_string(Obj)]),
	Heap#heap{mem=dict:store(Loc, Obj, Heap#heap.mem)}.

zip(ZippingNodeID, Heap1, Heap2, Sched) ->
	Mem = dict:merge(
		fun(Loc, Object1, Object2) ->
			object:zip(ZippingNodeID, Loc, Object1, Object2, Sched)
		end,
		Heap1#heap.mem, 
		Heap2#heap.mem
	),
	#heap{mem=Mem}.
	
merge(MergingNodeID, Heap1, Heap2, Sched) ->
	Mem = dict:merge(
		fun(Loc, Object1, Object2) ->
			object:merge(MergingNodeID, Loc, Object1, Object2, Sched)
		end,
		Heap1#heap.mem,
		Heap2#heap.mem
	),
	#heap{mem=Mem}.