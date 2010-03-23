-module (heap).

-include("include/debug.hrl").
-include("include/values.hrl").

-export ([new/0, new_struct/2, new_array/2, new_lock/2]).
-export ([loc_type/1, struct_loc/2, array_loc/2, lock_loc/2]).
-export ([get/2, set/3]).

-record (heap, {mem=dict:new()}).

-record (struct_loc, {nth, act_loc}).
-record (array_loc, {nth, act_loc}).
-record (lock_loc, {nth, act_loc}).

% ***********************************************
%	Locations
% ***********************************************
loc_type(Loc) ->
	element(1, Loc).
	
struct_loc(Nth, ActLoc) when is_integer(Nth) ->
	#struct_loc{nth=Nth, act_loc=ActLoc}.
	
array_loc(Nth, ActLoc) when is_integer(Nth) ->
	#array_loc{nth=Nth, act_loc=ActLoc}.
	
lock_loc(Nth, ActLoc) when is_integer(Nth) ->
	#lock_loc{nth=Nth, act_loc=ActLoc}.

% ***********************************************
% ***********************************************
new() ->
	#heap{}.
	
new_struct(#struct_loc{}=Loc, Heap) ->
	Heap#heap{mem=utils:dict_store_if_not_present(Loc, fun struct:new/0, Heap#heap.mem)}.
	
new_array(#array_loc{}=Loc, Heap) ->
	Heap#heap{mem=utils:dict_store_if_not_present(Loc, fun array:new/0, Heap#heap.mem)}.
	
new_lock(#lock_loc{}=Loc, Heap) ->
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
