-module (object).

%an object is either a struct or an array

-export ([loc_type/1, struct_loc/2, array_loc/2, lock_loc/2]).
-export ([write/4, read/3]).
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
		
% -> Object2
write(WritingNodeID, SlotName, Value, Object) ->	
	case struct:is_struct(Object) of
		true ->	struct:write(WritingNodeID, SlotName, Value, Object);
		false -> array:write(WritingNodeID, SlotName, Value, Object)
	end.
	
% -> {Value, Object2}
read(WritingNodeID, SlotName, Object) ->
	case struct:is_struct(Object) of
		true -> struct:read(WritingNodeID, SlotName, Object);
		false -> array:read(WritingNodeID, SlotName, Object)
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