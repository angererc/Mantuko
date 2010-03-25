-module (object).

%an object is either a struct or an array

-export ([loc_type/1, struct_loc/2, array_loc/2, lock_loc/2]).
-export ([set/3, get/2]).
-export ([zip/5, merge/5]).

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
		
set(SlotName, Value, Object) ->	
	case struct:is_struct(Object) of
		true ->	struct:set(SlotName, Value, Object);
		false -> array:set(SlotName, Value, Object)
	end.
	
get(SlotName, Object) ->
	case struct:is_struct(Object) of
		true -> struct:get(SlotName, Object);
		false -> array:get(SlotName, Object)
	end.

zip(ZippingNodeID, Loc, Object1, Object2, Sched) ->
	case struct:is_struct(Object1) of
		true -> struct:zip(ZippingNodeID, Loc, Object1, Object2, Sched);
		false -> array:zip(ZippingNodeID, Loc, Object1, Object2, Sched)
	end.
	
merge(ZippingNodeID, Loc, Object1, Object2, Sched) ->
	case struct:is_struct(Object1) of
		true -> struct:merge(ZippingNodeID, Loc, Object1, Object2, Sched);
		false -> array:merge(ZippingNodeID, Loc, Object1, Object2, Sched)
	end.