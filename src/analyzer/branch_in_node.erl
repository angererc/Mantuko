-module (branch_in_node).

-include("include/nodes.hrl").

-export ([new/0, add_option/3, analyze/3]).

% @spec new(Options) -> nodes:branch_in_node()
% Options = [values:activation_option()]
% @end
new() ->
	#branch_in_node{activation_options=sets:new()}.
	
add_option(BlockRef, ThisLoc, #branch_in_node{activation_options=AOs}=Node) ->
	Node#branch_in_node{
		activation_options=sets:add_element(
							refs:activation_option(BlockRef, ThisLoc),
							AOs)}.
	
analyze(_Node, _InputHeap, _Loader) ->
	ok.