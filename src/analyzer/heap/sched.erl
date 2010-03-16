-module (sched).

-include("include/heap.hrl").

-export ([new/0, new_node/3]).
-export ([new_edge/3, new_creation_edge/3]).
-export ([get/2]).

% the schedule is part of the heap, so don't use it directly but only through heap functions
new() ->
	dict:new().
	
new_node(ActivationRef, #branch_in_node{}=Node, Sched) ->
	Sched2 = dict:store(ActivationRef, Node, Sched),
	
	BranchOutRef = refs:activation_ref(refs:branch_out_node(), ActivationRef),
	BranchOutNode = branch_out_node:new(),
	Sched3 = dict:store(BranchOutRef, BranchOutNode, Sched2),
	Sched4 = new_creation_edge(ActivationRef, BranchOutRef, Sched3),
	Sched4;
new_node(ActivationRef, Node, Sched) ->
	dict:store(ActivationRef, Node, Sched).
	
new_edge(SourceActivationRef, TargetActivationRef, Sched) ->
	Sched.
	
new_creation_edge(SourceActivationRef, TargetActivationRef, Sched) ->
	Sched.
	
get(ActivationRef, Sched) ->
	case dict:find(ActivationRef, Sched) of
		{ok, Value} ->
			Value
	end.