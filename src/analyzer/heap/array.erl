-module (array).

-include("include/objects.hrl").

-export ([new/1]).
-export ([write/4, read/3]).
-export ([merge/4]).

new(NodeID) ->
	#array{reader=NodeID, writer=NodeID, values=dict:new()}.
	
write(WritingNodeID, Index, Value, #array{}=Arr) ->
	Arr#array{writer=WritingNodeID, values=dict:store(Index, Value, Arr#array.values)}.
	
read(_WritingNodeID, _Index, not_yet_implemented) ->
	ok.
	
	
merge(LastReader, LastWriter, A1, A2) ->
	Values = dict:merge(fun(_Index, Value1, Value2)-> value:merge(Value1, Value2) end, A1#array.values, A2#array.values),
	#array{reader=LastReader, writer=LastWriter, values=Values}.
