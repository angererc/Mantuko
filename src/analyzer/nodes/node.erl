-module (node).

-include("include/heap.hrl").

-export ([analyze/4]).

% faking some virtual method call and inheritance here
analyze(ActivationRef, Parents, Heap, Loader) ->
	case heap:get(ActivationRef, Heap) of
		#branch_in_node{}=Node ->
			branch_in_node:analyze(ActivationRef, Node, Parents, Heap, Loader);
		#branch_out_node{}=Node ->
			branch_out_node:analyze(ActivationRef, Node, Parents, Heap, Loader);
		#option_node{}=Node ->
			option_node:analyze(ActivationRef, Node, Parents, Heap, Loader)
	end.
	