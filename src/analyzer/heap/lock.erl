-module (lock).

-include("include/objects.hrl").

-export ([new/1]).

new(NodeID) ->
	#lock{reader=NodeID, writer=NodeID}.