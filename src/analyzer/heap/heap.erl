-module (heap).

-include("include/values.hrl").
-include("include/heap.hrl").

-export ([new/0, new_struct/2, new_array/2, new_lock/2]).
-export ([get/2, set/3]).
-export ([compute_incoming_heap/2]).

-record (heap, {mem=dict:new()}).

new() ->
	#heap{}.
	
new_struct(Loc, Heap) ->
	%if the heap contains something at Loc already it is for sure a struct
	%because the location contains the creating statement and the statement
	%is obviously a "new struct"; intrinsics have to be careful, though!
	Heap#heap{mem=utils:dict_store_if_not_present(Loc, fun struct:new/0, Heap#heap.mem)}.
	
new_array(Loc, Heap) ->
	Heap#heap{mem=utils:dict_store_if_not_present(Loc, fun array:new/0, Heap#heap.mem)}.
	
new_lock(Loc, Heap) ->
	Heap#heap{mem=utils:dict_store_if_not_present(Loc, fun lock:new/0, Heap#heap.mem)}.
		
get(Ref, Heap) ->
	case dict:find(Ref, Heap#heap.mem) of
		{ok, Value} ->
			Value;
		error ->
			debug:fatal("didn't find object for ref ~w", [Ref]),
			error
	end.
	
set(Ref, Obj, Heap) ->
	Heap#heap{mem=dict:store(Ref, Obj, Heap#heap.mem)}.
	
compute_incoming_heap(_Ref, Heap) ->
	Heap.