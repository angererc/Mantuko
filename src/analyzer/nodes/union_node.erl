-module (union_node).

-include("include/debug.hrl").

-export ([new/0, analyze/5]).

-record (union_node, {}).

new() ->
	#union_node{}.
	
analyze(_NodeID, _Parents, _Heap, _Sched, _Loader) ->
	ok.