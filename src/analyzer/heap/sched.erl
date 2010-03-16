-module (sched).

-include("include/heap.hrl").

-export ([new/0, new_node/3]).
-export ([get/2]).

% the schedule is part of the heap, so don't use it directly but only through heap functions
new() ->
	dict:new().
	
new_node(ActivationRef, #branch_in_node{}=Node, Sched) ->
	BranchOutRef = refs:activation_ref(refs:branch_out_node(), ActivationRef),
	BranchOutNode = branch_out_node:new(),
	
	Sched2 = dict:store(ActivationRef, Node, Sched),
	dict:store(BranchOutRef, BranchOutNode, Sched2).
	
get(ActivationRef, Sched) ->
	case dict:find(ActivationRef, Sched) of
		{ok, Value} ->
			Value
	end.