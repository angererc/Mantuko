-module (branch_out_node).

-include("include/debug.hrl").
-include("include/heap.hrl").

-export ([new/0, analyze/5]).

new() ->
	#branch_out_node{}.
	
analyze(_ActivationRef, _Node, _Parents, _Heap, _Loader) ->
	ok.