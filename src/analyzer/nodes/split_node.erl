-module (split_node).

-include("include/debug.hrl").

-export ([new/0, create_union_node/2, add_closure/2, analyze/5]).
-export ([get_block_refs/2, get_struct_locs/2]).

-record (split_node, {closures}).

new() ->
	#split_node{closures=sets:new()}.
	
create_union_node(SplitNodeID, Sched) ->
	BranchOutRef = node:union_node_id(SplitNodeID),
	BranchOutNode = union_node:new(),
	Sched2 = sched:set_node(BranchOutRef, BranchOutNode, Sched),
	Sched3 = sched:new_edge(SplitNodeID, BranchOutRef, Sched2),
	Sched3.
	
add_closure(Closure, #split_node{closures=Closures}=Node) ->
	Node#split_node{closures=sets:add_element(Closure, Closures)}.
	
analyze(MyNodeID, Parents, Heap, Sched, Loader) ->
	#split_node{closures=Closures} = sched:get_node(MyNodeID, Sched),
	%store the incoming heap
	Sched2 = sched:set_result(MyNodeID, Heap, Sched),
	case check_for_loop(Closures, Parents, Sched) of
		{true, _Parent} ->
			loop_found;
		false ->
			{Sched3, NewNodes} = create_nodes(MyNodeID, Closures, Sched2),
			analyze_children(NewNodes, [], [MyNodeID|Parents], Heap, Sched3, Loader)
	end.

create_nodes(MyNodeID, Closures, Sched)	->
	sets:fold( %create a node for each closure and add the edges
		fun(Closure, {SchedAcc, NewNodes})-> 
			NewNodeID = node:atom_node_id(Closure, MyNodeID),
			NewNode = atom_node:new(Closure),
			SchedAcc2 = sched:set_node(NewNodeID, NewNode, SchedAcc),
			% this branch node created the new activation
			SchedAcc3 = sched:new_edge(MyNodeID, NewNodeID, SchedAcc2),
			% add a happens-before between the new activation and our branch_out node
			%the branch out node has been created before when this branch node has been created
			SchedAcc4 = sched:new_edge(
							NewNodeID, 
							node:union_node_id(MyNodeID),
							SchedAcc3),
			%and continue with the next option
			{SchedAcc4, [NewNodeID|NewNodes]}
		end, 
		{Sched, []}, Closures).

get_block_refs(MyNodeID, Sched) ->
	#split_node{closures=Closures} = sched:get_node(MyNodeID, Sched),
	closure:extract_blocks(Closures).
	
get_struct_locs(MyNodeID, Sched) ->
	#split_node{closures=Closures} = sched:get_node(MyNodeID, Sched),
	closure:extract_structs(Closures).
		
%we added ourselves to Parents already!
analyze_children([], Leftovers, _Parents, _Heap, _Sched, _Loader) ->
	Leftovers;
analyze_children([ChildNodeID|Rest], Leftovers, Parents, Heap, Sched, Loader) ->
	case sched:is_schedulable(ChildNodeID, Sched) of
		true ->
			ChildHeap = heap:compute_incoming_heap(ChildNodeID, Heap),
			ChildLeftovers = node:analyze(ChildNodeID, Parents, ChildHeap, Sched, Loader),
			analyze_children(Rest ++ ChildLeftovers, Leftovers, Parents, Heap, Sched, Loader);
		false ->
			analyze_children(Rest, [ChildNodeID, Leftovers], Parents, Heap, Sched, Loader)
	end.



check_for_loop(_MyClosures, [], _Sched) ->
	false;
check_for_loop(MyClosures, [Parent|Grandpa], Sched) ->
	#split_node{closures=ParentClosures} = sched:get_node(Parent, Sched),
	MyActivationBlocks = closures:extract_blocks(MyClosures),
	ParentActivationBlocks = closures:extract_blocks(ParentClosures),
	case sets:size(sets:intersection(MyActivationBlocks, ParentActivationBlocks)) of
		0 -> check_for_loop(MyClosures, Grandpa, Sched);
		_Else -> {true, Parent}
	end.
	