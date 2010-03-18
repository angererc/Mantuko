-module (node).

-include("include/debug.hrl").
-include("include/heap.hrl").
-include("include/nodes.hrl").

-export ([analyze/5]).

% faking some virtual method call and inheritance here
analyze(ActivationRef, Parents, Heap, Sched, Loader) ->
	case sched:get_node(ActivationRef, Sched) of
		#branch_in_node{}=Node ->
			branch_in_node:analyze(ActivationRef, Node, Parents, Heap, Sched, Loader);
		#branch_out_node{}=Node ->
			branch_out_node:analyze(ActivationRef, Node, Parents, Heap, Sched, Loader);
		#option_node{}=Node ->
			option_node:analyze(ActivationRef, Node, Parents, Heap, Sched, Loader)
	end.
	