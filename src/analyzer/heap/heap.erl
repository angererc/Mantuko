-module (heap).

-include("include/values.hrl").
-include("include/heap.hrl").

-export ([new/0, new_struct/2, new_array/2, new_lock/2, new_node/3]).
-export ([get/2]).

-record (heap, {mem=dict:new(), sched=sched:new()}).

new() ->
	#heap{}.
	
new_struct(Loc, #heap{mem=Mem}=Heap) ->
	%if the heap contains something at Loc already it is for sure a struct
	%because the location contains the creating statement and the statement
	%is obviously a "new struct"; intrinsics have to be careful, though!
	Heap#heap{mem=utils:dict_store_if_not_present(Loc, fun struct:new/0, Mem)}.
	
new_array(Loc, #heap{mem=Mem}=Heap) ->
	Heap#heap{mem=utils:dict_store_if_not_present(Loc, fun array:new/0, Mem)}.
	
new_lock(Loc, #heap{mem=Mem}=Heap) ->
	Heap#heap{mem=utils:dict_store_if_not_present(Loc, fun lock:new/0, Mem)}.
	
new_node(ActivationRef, Node, #heap{sched=Sched}=Heap) ->
	Heap#heap{sched=sched:new_node(ActivationRef, Node, Sched)}.
	
get(#loc{}=Loc, #heap{mem=Mem}) ->
	case dict:find(Loc, Mem) of
		{ok, Value} ->
			Value;
		error ->
			events:fatal("didn't find object for location ~w", [Loc]),
			error
	end;
get(#activation_ref{}=Ref, #heap{sched=Sched}) ->
	sched:get(Ref, Sched).