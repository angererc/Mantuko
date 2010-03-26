-module (struct).

-include("include/objects.hrl").
-include("include/values.hrl").
-include("include/debug.hrl").

-export ([new/1]).
-export ([write/4, read/3]).
-export ([zip/5, merge/5]).

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
	
zip(ZippingNodeID, Loc, S1, S2, Sched) ->
	case sched:get_relationship(S1#struct.writer, S2#struct.writer, Sched) of
		same -> %parallel reads are OK
			true = S1#struct.slots =:= S2#struct.slots, %for sanity reasons
			#struct{reader=ZippingNodeID, writer=S1#struct.writer, slots=S1#struct.slots};
		concurrent ->
			debug:fatal("write/write race in struct ~s: written concurrently in ~s and ~s", 
						[pretty:string(Loc), pretty:string(S1#struct.writer), pretty:string(S2#struct.writer)]),
			error;
		left_hb_right -> %S2#struct.writer is the last
			case sched:get_relationship(S1#struct.reader, S1#struct.writer, Sched) of
				same -> #struct{reader=ZippingNodeID, writer=S2#struct.writer, slots=S2#struct.slots};
				left_hb_right -> #struct{reader=ZippingNodeID, writer=S2#struct.writer, slots=S2#struct.slots};
				right_hb_left ->
					debug:fatal("read/write race in struct ~s: ~s read concurrently to ~s writing",
						[pretty:string(Loc), pretty:string(S1#struct.reader), pretty:string(S2#struct.writer)])
			end;
		right_hb_left ->
			%writer of S1 is last writer; make sure that reader2 -> writer1
			% because we just saw that Writer2 -> Writer1 and the Writer2 is the point where we split the two
			%heaps, we can also check that Reader2 -> Writer2 (and therefore also Writer1)
			%I think that's a little faster becausse Reader2 and Writer2 are probably closer together
			case sched:get_relationship(S2#struct.reader, S2#struct.writer, Sched) of
				same -> #struct{reader=ZippingNodeID, writer=S1#struct.writer, slots=S1#struct.slots};
				left_hb_right -> #struct{reader=ZippingNodeID, writer=S1#struct.writer, slots=S1#struct.slots};
				right_hb_left ->
					debug:fatal("read/write race in struct ~s: ~s read concurrently to ~s writing",
						[pretty:string(Loc), pretty:string(S2#struct.reader), pretty:string(S1#struct.writer)])
				%we don't check for concurrent, because the reader and writer of the same struct value must
				%be ordered somehow; 
			end
	end.
		
merge(MergingNodeID, _Loc, S1, S2, Sched) ->
	LastReader = case sched:get_relationship(S1#struct.reader, S2#struct.reader, Sched) of
		same -> S1#struct.reader;
		exclusive -> MergingNodeID;
		left_hb_right -> S2#struct.reader;
		right_hb_left -> S1#struct.reader
		%we don't have a concurrent here, because merge is called for exclusive nodes
		%as long as the struct was still "the same" they are equal; from the point of where they
		%split they can only be read/written by exclusive nodes until they are merged back again
	end,
	LastWriter = case sched:get_relationship(S1#struct.writer, S2#struct.writer, Sched) of
		same -> S1#struct.writer;
		exclusive -> MergingNodeID;
		left_hb_right -> S2#struct.writer;
		right_hb_left -> S1#struct.writer
	end,
	
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
			dict:store(SlotName, merge_values(Value1, Value2), Acc)
		end,
		dict:new(),
		AllSlots),
	
	#struct{reader=LastReader, writer=LastWriter, slots=NewSlots}.

%any with anything else
merge_values(#any{}=Any, _Value) ->
	Any;
merge_values(_Value, #any{}=Any) ->
	Any;
	
%some with some, some with option, some with value
merge_values(#some{type=Type}, #some{type=Type}=V) ->
	V;
merge_values(#some{type=_Type1}, #some{type=_Type2}) ->
	#any{};
merge_values(#some{type=Type}=V, #one_of{type=Type}) ->
	V;
merge_values(#one_of{type=Type}, #some{type=Type}=V) ->
	V;
merge_values(#some{type=_Type1}, #one_of{type=_Type2}) ->
	#any{};
merge_values(#one_of{type=_Type2}, #some{type=_Type}) ->
	#any{};
merge_values(#some{type=Type}=V, Value) ->
	if
		Type =:= element(1, Value) ->
			V;
		true ->
			#any{}
	end;
merge_values(Value, #some{type=Type}=V) ->
	if
		Type =:= element(1, Value) ->
			V;
		true ->
			#any{}
	end;
merge_values(#one_of{type=Type}, #some{type=Type}=V) ->
	V;

%option with option, option with value
merge_values(#one_of{type=Type, value_set=Options1}, #one_of{type=Type, value_set=Options2}) ->
	#one_of{type=Type, value_set=sets:union(Options1, Options2)};
merge_values(#one_of{type=Type, value_set=Options1}, Value) ->
	if
		Type =:= element(1, Value) ->
			#one_of{type=Type, value_set=sets:add_element(Value, Options1)};
		true ->
			#any{}
	end;
merge_values(Value, #one_of{type=Type, value_set=Options1}) ->
	if
		Type =:= element(1, Value) ->
			#one_of{type=Type, value_set=sets:add_element(Value, Options1)};
		true ->
			#any{}
	end;

%now, the only unhandled possibilities are only values sym, num, block, nil, loc, act
merge_values(Value, Value) -> 
	Value;
merge_values(Value, #nil{}) ->
	#one_of{type=element(1, Value), value_set=sets:from_list([Value, #nil{}])};
merge_values(#nil{}, Value) ->
	#one_of{type=element(1, Value), value_set=sets:from_list([Value, #nil{}])};
%two sym, num, block, nil, loc, act in any combination
merge_values(Value1, Value2) ->
	Type1 = element(1, Value1),
	Type2 = element(1, Value2),
	if 
		Type1 =:= Type2 ->
			#one_of{type=Type1, value_set=sets:from_list([Value1, Value2])};
		true ->
			#any{}
	end.