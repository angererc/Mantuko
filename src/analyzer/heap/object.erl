-module (object).

-include("include/objects.hrl").

-export ([loc_type/1, struct_loc/2, array_loc/2, lock_loc/2]).
-export ([write/4, read/3]).
-export ([zip/5, merge/5]).

-record (struct_loc, {nth, act_loc}).
-record (array_loc, {nth, act_loc}).
-record (lock_loc, {nth, act_loc}).

% objects are structs, arrays, and locks

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
write(WritingNodeID, SlotName, Value, #struct{}=Object) ->	
	struct:write(WritingNodeID, SlotName, Value, Object);
write(WritingNodeID, SlotName, Value, #array{}=Object) ->	
	array:write(WritingNodeID, SlotName, Value, Object).
	
% -> {Value, Object2}
read(WritingNodeID, SlotName, #struct{}=Object) ->
	struct:read(WritingNodeID, SlotName, Object);
read(WritingNodeID, SlotName, #array{}=Object) ->
	array:read(WritingNodeID, SlotName, Object).

zip(ZippingNodeID, Loc, #struct{}=Object1, #struct{}=Object2, Sched) ->
	struct:zip(ZippingNodeID, Loc, Object1, Object2, Sched);
zip(ZippingNodeID, Loc, #array{}=Object1, #array{}=Object2, Sched) ->
	array:zip(ZippingNodeID, Loc, Object1, Object2, Sched).
	
merge(ZippingNodeID, Loc, #struct{}=Object1, #struct{}=Object2, Sched) ->
	struct:merge(ZippingNodeID, Loc, Object1, Object2, Sched);
merge(ZippingNodeID, Loc, #array{}=Object1, #array{}=Object2, Sched) ->
	array:merge(ZippingNodeID, Loc, Object1, Object2, Sched).