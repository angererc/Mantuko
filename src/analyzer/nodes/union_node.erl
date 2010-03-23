-module (union_node).

-include("include/debug.hrl").

-export ([new/0, merge/2, analyze/5]).

-record (union_node, {}).

new() ->
	#union_node{}.
	
%currently we only have one node instance
merge(One, One) ->
	One.
	
analyze(_NodeID, _Parents, _Heap, _Sched, _Loader) ->
	ok.