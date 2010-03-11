-module (node).

-include("include/nodes.hrl").

-export ([analyze/3]).

% faking some virtual method call and inheritance here
analyze(#branch_in_node{}=Node, InputHeap, Loader) ->
	branch_in_node:analyze(Node, InputHeap, Loader);
analyze(#branch_out_node{}=Node, InputHeap, Loader) ->
	branch_out_node:analyze(Node, InputHeap, Loader);
analyze(#option_node{}=Node, InputHeap, Loader) ->
	option_node:analyze(Node, InputHeap, Loader).