-module (branch_in_node).

-include("include/debug.hrl").
-include("include/heap.hrl").
-include("include/nodes.hrl").

-export ([new/0, create_branch_out_node/2, add_option/3, analyze/6]).

new() ->
	#branch_in_node{activation_options=sets:new()}.
	
create_branch_out_node(BranchInActivationRef, Sched) ->
	BranchOutRef = refs:activation_ref(refs:branch_out_node(), BranchInActivationRef),
	BranchOutNode = branch_out_node:new(),
	Sched2 = sched:set_node(BranchOutRef, BranchOutNode, Sched),
	Sched3 = sched:new_edge(BranchInActivationRef, BranchOutRef, Sched2),
	Sched3.
	
add_option(BlockRef, ThisLoc, #branch_in_node{activation_options=AOs}=Node) ->
	Node#branch_in_node{
		activation_options=sets:add_element(
							refs:activation_option(BlockRef, ThisLoc),
							AOs)}.
	
analyze(ActivationRef, #branch_in_node{activation_options=AOs}, Parents, Heap, Sched, Loader) ->
	%store the incoming heap
	Sched2 = sched:set_result(ActivationRef, Heap, Sched),
	case check_for_loop(AOs, Parents, Sched) of
		{true, _Parent} ->
			loop_found;
		false ->
			{Sched3, NewNodes} = create_nodes(ActivationRef, AOs, Sched2),
			analyze_children(ActivationRef, NewNodes, [], [ActivationRef|Parents], Heap, Sched3, Loader)
	end.

create_nodes(ActivationRef, ActivationOptions, Sched)	->
	sets:fold( %create a node for each option and add the edges
		fun(Option, {SchedAcc, NewNodes})-> 
			NewActivationRef = refs:activation_ref(Option, ActivationRef),
			NewNode = option_node:new(Option),
			SchedAcc2 = sched:set_node(NewActivationRef, NewNode, SchedAcc),
			% this branch node created the new activation
			SchedAcc3 = sched:new_edge(ActivationRef, NewActivationRef, SchedAcc2),
			% add a happens-before between the new activation and our branch_out node
			%the branch out node has been created before when this branch node has been created
			SchedAcc4 = sched:new_edge(
							NewActivationRef, 
							refs:activation_ref(refs:branch_out_node(), ActivationRef), 
							SchedAcc3),
			%and continue with the next option
			{SchedAcc4, [NewActivationRef|NewNodes]}
		end, 
		{Sched, []}, ActivationOptions).
		
%we added ourselves to Parents already!
analyze_children(_ActivationRef, [], Leftovers, _Parents, _Heap, _Sched, _Loader) ->
	Leftovers;
analyze_children(ActivationRef, [ChildActivationRef|Rest], Leftovers, Parents, Heap, Sched, Loader) ->
	case sched:is_schedulable(ChildActivationRef, Sched) of
		true ->
			ChildHeap = heap:compute_incoming_heap(ChildActivationRef, Heap),
			ChildLeftovers = node:analyze(ChildActivationRef, Parents, ChildHeap, Loader),
			analyze_children(ActivationRef, Rest ++ ChildLeftovers, Leftovers, Parents, Heap, Sched, Loader);
		false ->
			analyze_children(ActivationRef, Rest, [ChildActivationRef, Leftovers], Parents, Heap, Sched, Loader)
	end.

check_for_loop(_MyActivationOptions, [], _Sched) ->
	false;
check_for_loop(MyActivationOptions, [Parent|Rest], Sched) ->
	% that's wrong; we have to check if there is any option with the same block, the
	% this pointer is not relevant
	#branch_in_node{activation_options=ParentActivationOptions} = sched:get_node(Parent, Sched),
	case sets:size(sets:intersection(MyActivationOptions, ParentActivationOptions)) of
		0 -> check_for_loop(MyActivationOptions, Rest, Sched);
		_Else -> {true, Parent}
	end.
	