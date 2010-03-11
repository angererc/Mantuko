-module (node).

-include("include/nodes.hrl").

-export ([analyze/1]).

% faking some virtual method call and inheritance here
analyze(#branch_node{}=Node) ->
	branch_node:analyze(Node);
analyze(#option_node{}=Node) ->
	option_node:analyze(Node).