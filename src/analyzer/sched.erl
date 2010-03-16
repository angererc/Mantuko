-module (sched).

-include("include/nodes.hrl").

-export ([new/0, new_node/3]).

% the schedule is part of the heap, so don't use it directly but only through heap functions

new() ->
	ok.
	
new_node(ActivationRef, #branch_in_node{}=Node, Sched) ->
	% make sure to create a phi node too
	ok.