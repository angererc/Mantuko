-module (struct).

-include("include/objects.hrl").
-include("include/values.hrl").
-include("include/debug.hrl").

-export ([new/1]).
-export ([write/4, read/3]).
-export ([merge/4]).

new(NodeID) ->
	#struct{reader=NodeID, writer=NodeID, slots=dict:new()}.
	
%note: we cannot use the whole #sym{} record because the same sym might
% occur with different nth fields (not sure if that will stay, but that's how it is right now!)
write(WritingNodeID, #sym{name=Slot}, #nil{}, Struct) ->	
	?f("setting struct slot ~w to nil", [Slot]),
	Struct#struct{writer=WritingNodeID, slots=dict:erase(Slot, Struct#struct.slots)};
write(WritingNodeID, #sym{name=Slot}, Value, Struct) ->
	?f("setting struct slot ~w to ~s", [Slot, pretty:string(Value)]),
	Struct#struct{writer=WritingNodeID, slots=dict:store(Slot, Value, Struct#struct.slots)}.
	
read(ReadingNodeID, #sym{name=Slot}, Struct) ->
	case dict:find(Slot, Struct#struct.slots) of
		{ok, Value} -> 
			?f("getting struct slot ~w => ~s", [Slot, pretty:string(Value)]),
			{Value, Struct#struct{reader=ReadingNodeID}};
		error -> 
			?f("getting struct slot ~w => nil", [Slot]),
			{#nil{}, Struct#struct{reader=ReadingNodeID}}
	end.
		
merge(LastReader, LastWriter, S1, S2) ->
	%can't use dict:merge here because we have to merge-in the #nil{} when one dict doesn't contain a slot
	AllSlots = sets:from_list(dict:fetch_keys(S1#struct.slots) ++ dict:fetch_keys(S2#struct.slots)),
	
	NewSlots = sets:fold(
		fun(SlotName, Acc)->	
			Value1 = case dict:find(SlotName, S1#struct.slots) of
				{ok, Val1} -> Val1;
				error -> #nil{}
			end,
			Value2 = case dict:find(SlotName, S2#struct.slots) of
				{ok, Val2} -> Val2;
				error -> #nil{}
			end,
			dict:store(SlotName, value:merge(Value1, Value2), Acc)
		end,
		dict:new(),
		AllSlots),
	
	#struct{reader=LastReader, writer=LastWriter, slots=NewSlots}.

