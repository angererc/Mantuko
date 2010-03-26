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
		
merge(MergingNodeID, Loc, O1, O2, Sched) ->
	LastReader = case sched:get_relationship(reader(O1), reader(O2), Sched) of
		same -> reader(O1);
		exclusive -> MergingNodeID;
		left_hb_right -> reader(O2);
		right_hb_left -> reader(O1)
		%we don't have a concurrent here, because merge is called for exclusive nodes
		%as long as the struct was still "the same" they are equal; from the point of where they
		%split they can only be read/written by exclusive nodes until they are merged back again
	end,
	LastWriter = case sched:get_relationship(writer(O1), writer(O2), Sched) of
		same -> writer(O1);
		exclusive -> MergingNodeID;
		left_hb_right -> writer(O2);
		right_hb_left -> writer(O1)
	end,
	
	case Loc of
		#struct_loc{} ->
			struct:merge(LastReader, LastWriter, O1, O2);
		#array_loc{} ->
			array:merge(LastReader, LastWriter, O1, O2);
		#lock_loc{} ->
			lock:merge(LastReader, LastWriter, O1, O2)
	end.
	
zip(ZippingNodeID, Loc, O1, O2, Sched) ->
	case sched:get_relationship(writer(O1), writer(O2), Sched) of
		same -> %parallel reads are OK
			assert_equal_payload(O1, O2),
			set_reader(ZippingNodeID, O1);
		concurrent ->
			debug:fatal("write/write race in struct ~s: written concurrently in ~s and ~s", 
						[pretty:string(Loc), pretty:string(writer(O1)), pretty:string(writer(O2))]),
			error;
		right_hb_left ->
			%writer of O1 is last writer; make sure that reader2 -> writer1
			% because we just saw that Writer2 -> Writer1 and the Writer2 is the point where we did split the two
			%heaps, we can also check that Reader2 -> Writer2 (and therefore also Writer1)
			%I think that's a little faster becausse Reader2 and Writer2 are probably closer together
			case sched:get_relationship(reader(O2), writer(O2), Sched) of
				same -> set_reader(ZippingNodeID, O1);
				left_hb_right -> set_reader(ZippingNodeID, O1);
				right_hb_left ->
					debug:fatal("read/write race in struct ~s: ~s read concurrently to ~s writing",
						[pretty:string(Loc), pretty:string(reader(O2)), pretty:string(writer(O1))])
				%we don't check for concurrent, because the reader and writer of the same struct value must
				%be ordered somehow; 
			end;
		left_hb_right -> %O2Writer is the last
			case sched:get_relationship(reader(O1), writer(O1), Sched) of
				same -> set_reader(ZippingNodeID, O2);
				left_hb_right -> set_reader(ZippingNodeID, O2);
				right_hb_left ->
					debug:fatal("read/write race in struct ~s: ~s read concurrently to ~s writing",
						[pretty:string(Loc), pretty:string(reader(O1)), pretty:string(writer(O2))])
			end
	end.
	
% "Polymorphism"
reader(#struct{reader=R}) -> R;
reader(#array{reader=R}) -> R;
reader(#lock{reader=R}) -> R.

writer(#struct{writer=W}) -> W;
writer(#array{writer=W}) -> W;
writer(#lock{writer=W}) -> W.

set_reader(Reader, #struct{}=S) ->
	S#struct{reader=Reader};
set_reader(Reader, #array{}=S) ->
	S#array{reader=Reader};
set_reader(Reader, #lock{}=S) ->
	S#lock{reader=Reader}.
	
assert_equal_payload(#struct{slots=S}, #struct{slots=S}) ->
	ok;
assert_equal_payload(#array{values=V}, #array{values=V}) ->
	ok;
assert_equal_payload(#lock{}, #lock{}) ->
	ok.