-module (split_node).

-include("include/debug.hrl").

-export ([new/0, merge/2, create_union_node/2, add_closure/2, analyze/5]).
-export ([get_block_refs/2, get_struct_locs/2]).

-record (split_node, {closures}).

new() ->
	#split_node{closures=sets:new()}.

merge(One, Other) ->
	#split_node{closures=sets:union(One#split_node.closures, Other#split_node.closures)}.
	
create_union_node(SplitNodeID, Sched) ->
	UnionNodeID = node:union_node_id(SplitNodeID),
	UnionNode = union_node:new(),
	Sched2 = sched:new_node(UnionNodeID, Sched),
	Sched3 = sched:set_node_info(UnionNodeID, UnionNode, Sched2),
	Sched4 = sched:new_edge(SplitNodeID, UnionNodeID, Sched3),
	Sched4.
	
add_closure(Closure, #split_node{closures=Closures}=Node) ->
	Node#split_node{closures=sets:add_element(Closure, Closures)}.
	
analyze(MyNodeID, Parents, Heap, ParentSched, Loader) ->
	#split_node{closures=Closures} = sched:get_node_info(MyNodeID, ParentSched),
	MySched = sched:new_child_schedule(ParentSched),
	case check_for_loop(Closures, Parents, MySched) of
		{true, _Parent} ->
			loop_found;
		false ->
			%store the incoming heap
			MySched = sched:new_child_schedule(ParentSched),
			MySched2 = sched:set_result(MyNodeID, Heap, MySched),
			MySched3 = create_nodes(MyNodeID, Closures, MySched2),
			analyze_till_fixed_point([MyNodeID|Parents], MySched3, Loader)
	end.

create_nodes(MyNodeID, Closures, MySched)	->
	sets:fold( %create a node for each closure and add the edges
		fun(Closure, SchedAcc)-> 
			NewNodeID = node:atom_node_id(Closure, MyNodeID),
			NewNode = atom_node:new(Closure),
			SchedAcc2 = sched:new_node(NewNodeID, SchedAcc),
			SchedAcc3 = sched:set_node_info(NewNodeID, NewNode, SchedAcc2),
			% this branch node created the new activation
			SchedAcc4 = sched:new_edge(MyNodeID, NewNodeID, SchedAcc3),
			% add a happens-before between the new activation and our branch_out node
			%the branch out node has been created before when this branch node has been created
			SchedAcc5 = sched:new_edge(
							NewNodeID, 
							node:union_node_id(MyNodeID),
							SchedAcc4),
			%and continue with the next option
			SchedAcc5
		end, 
		MySched, 
		Closures).

get_block_refs(MyNodeID, Sched) ->
	#split_node{closures=Closures} = sched:get_node_info(MyNodeID, Sched),
	closure:extract_blocks(Closures).
	
get_struct_locs(MyNodeID, Sched) ->
	#split_node{closures=Closures} = sched:get_node_info(MyNodeID, Sched),
	closure:extract_structs(Closures).
		
analyze_till_fixed_point(Parents, MySched, Loader) ->
	%since initially, I get the old schedule here, the find_schedulable_nodes
	%will find nodes from my parent that might look schedulable but are not my business.
	%I probably want to keep the parent schedule out of this... or only use the
	%already analyzed ones, but not the open ones...
	case sched:get_schedulable_nodes(MySched) of
		[] -> 
			NewNodes = sched:get_new_nodes(MySched),
			?f("finished analyzing schedulable nodes; ~w are still open: ~w", [length(NewNodes), NewNodes]),
			MySched; %we are done, return to the split node that called us
		Schedulable ->
			?f("found ~w schedulable nodes: ~w", [length(Schedulable), Schedulable]),
			analyze_schedulable_nodes(Schedulable, Parents, MySched, Loader)
	end.
	
analyze_schedulable_nodes(Schedulable, Parents, MySched, Loader) ->
	%
	ChildSchedules = lists:map(
		fun(ChildNodeID)->
			ChildHeap = sched:compute_incoming_heap(ChildNodeID, MySched),
			%every node can be analyzed starting from our schedule; they cannot influence each other!
			% is that true? needs a proof or something...
			ChildSched = node:analyze(ChildNodeID, Parents, ChildHeap, MySched, Loader),
			ChildSched
		end,
		Schedulable),
	FoldedSched = lists:foldl(
		fun(ChildSched, Acc)-> 
			sched:merge(Acc, ChildSched) 
		end,
		%first, remove all the nodes that we just computed
		sched:remove_new_nodes(Schedulable, MySched), 
		ChildSchedules),
	analyze_till_fixed_point(Parents, FoldedSched, Loader).
	
check_for_loop(_MyClosures, [], _Sched) ->
	false;
check_for_loop(MyClosures, [Parent|Grandpa], Sched) ->
	#split_node{closures=ParentClosures} = sched:get_node_info(Parent, Sched),
	MyActivationBlocks = closure:extract_blocks(MyClosures),
	ParentActivationBlocks = closure:extract_blocks(ParentClosures),
	case sets:size(sets:intersection(MyActivationBlocks, ParentActivationBlocks)) of
		0 -> check_for_loop(MyClosures, Grandpa, Sched);
		_Else -> {true, Parent}
	end.
	