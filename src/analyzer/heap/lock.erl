-module (lock).

-export ([new/1]).

-record (lock, {reader, writer}).

new(NodeID) ->
	#lock{reader=NodeID, writer=NodeID}.