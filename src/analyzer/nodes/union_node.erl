-module (union_node).

-include("include/debug.hrl").
-include("include/heap.hrl").
-include("include/nodes.hrl").

-export ([new/0, analyze/6]).

new() ->
	#union_node{}.
	
analyze(_ActivationRef, _Node, _Parents, _Heap, _Sched, _Loader) ->
	ok.