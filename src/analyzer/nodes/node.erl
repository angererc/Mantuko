-module (node).

-include("include/debug.hrl").
-include("include/heap.hrl").
-include("include/nodes.hrl").

-export ([analyze/5]).

% faking some virtual method call and inheritance here
analyze(ActivationRef, Parents, Heap, Sched, Loader) ->
	case sched:get_node(ActivationRef, Sched) of
		#split_node{}=Node ->
			split_node:analyze(ActivationRef, Node, Parents, Heap, Sched, Loader);
		#union_node{}=Node ->
			union_node:analyze(ActivationRef, Node, Parents, Heap, Sched, Loader);
		#atom_node{}=Node ->
			atom_node:analyze(ActivationRef, Node, Parents, Heap, Sched, Loader)
	end.
	