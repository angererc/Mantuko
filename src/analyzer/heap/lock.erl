-module (lock).

-include("include/objects.hrl").

-export ([new/1]).
-export ([merge/4]).

new(NodeID) ->
	#lock{reader=NodeID, writer=NodeID}.
	
merge(LastReader, LastWriter, _L1, _L2) ->
	#lock{reader=LastReader, writer=LastWriter}.