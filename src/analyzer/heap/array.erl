-module (array).

-export ([new/0]).
-export ([write/4, read/3]).
-export ([merge/5, zip/5]).

-record (array, {reader, writer, values}).

new(NodeID) ->
	#array{reader=NodeID, writer=NodeID, values=dict:new()}.
	
write(WritingNodeID, Index, Value, #array{}=Arr) ->
	Arr#array{writer=WritingNodeID, values=dict:store(Index, Value, Arr#array.values)}.
	
read(_WritingNodeID, Index, not_yet_implemented) ->
	ok.
	
	
merge(MergingNodeID, _Loc, A1, A2, Sched) ->
	#array{values=sets:to_list(sets:from_list(A1#array.values ++ A2#array.values))}.