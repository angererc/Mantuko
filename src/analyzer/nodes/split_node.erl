-module (split_node).

-include("include/debug.hrl").
-include("include/nodes.hrl").

-export ([new/0, merge/2, create_union_node/2, add_closure/2, analyze/5]).
-export ([get_block_refs/2, get_struct_locs/2]).

-record (loop, {head_id, node_id, heap}).
new() ->
	#split_node{closures=sets:new()}.

merge(One, Other) ->
	#split_node{closures=sets:union(One#split_node.closures, Other#split_node.closures)}.
	
% -> {NewNode, Sched2}
create_union_node(SplitNodeID, Sched) ->
	UnionNodeID = node:union_node_id(SplitNodeID),
	UnionNode = union_node:new(),

	Sched2 = sched:set_node_info(UnionNodeID, UnionNode, Sched),
	Sched3 = sched:new_edge_no_smartness(SplitNodeID, UnionNodeID, Sched2),
	{UnionNodeID, Sched3}.
	
add_closure(Closure, #split_node{closures=Closures}=Node) ->
	Node#split_node{closures=sets:add_element(Closure, Closures)}.
	
analyze_after_loop(MyNodeID, ParentSplitNodes, Heap, Sched, Loader) ->
	?f("analyzing node ~s after loop", [pretty:string(MyNodeID)]),
	#split_node{closures=Closures} = sched:get_node_info(MyNodeID, Sched),
	%store the incoming heap
	Sched2 = sched:set_result(MyNodeID, Heap, Sched),
	{Worklist, Sched3} = create_nodes(MyNodeID, Closures, Sched2),
	%we leave ourselves out of the parent split nodes because we are not the loop head
	analyze_till_fixed_point(MyNodeID, ParentSplitNodes, Worklist, [], Sched3, Loader).
	
analyze(MyNodeID, ParentSplitNodes, Heap, Sched, Loader) ->
	?f("analyzing node ~s", [pretty:string(MyNodeID)]),
	#split_node{closures=Closures} = sched:get_node_info(MyNodeID, Sched),
	case check_for_loop(Closures, ParentSplitNodes, Sched) of
		{true, Parent} ->
			{[], [#loop{head_id=Parent, node_id=MyNodeID, heap=Heap}], Sched};
		false ->
			%store the incoming heap
			Sched2 = sched:set_result(MyNodeID, Heap, Sched),
			{Worklist, Sched3} = create_nodes(MyNodeID, Closures, Sched2),
			analyze_till_fixed_point(MyNodeID, [MyNodeID|ParentSplitNodes], Worklist, [], Sched3, Loader)
	end.

% -> {NewNodes, Sched2}
create_nodes(MyNodeID, Closures, Sched)	->
	sets:fold( %create a node for each closure and add the edges
		fun(Closure, {NewNodesAcc, SchedAcc})-> 
			NewNodeID = node:atom_node_id(Closure, MyNodeID),
			NewNode = atom_node:new(Closure),
			
			SchedAcc2 = sched:set_node_info(NewNodeID, NewNode, SchedAcc),
			% this branch node created the new activation
			SchedAcc3 = sched:new_edge_no_smartness(MyNodeID, NewNodeID, SchedAcc2),
			% add a happens-before between the new activation and our branch_out node
			%the branch out node has been created before when this branch node has been created
			SchedAcc4 = sched:new_edge(
							NewNodeID, 
							node:union_node_id(MyNodeID),
							SchedAcc3),
			%and continue with the next option
			{[NewNodeID|NewNodesAcc], SchedAcc4}
		end, 
		{[], Sched}, 
		Closures).

get_block_refs(MyNodeID, Sched) ->
	#split_node{closures=Closures} = sched:get_node_info(MyNodeID, Sched),
	closure:extract_blocks(Closures).
	
get_struct_locs(MyNodeID, Sched) ->
	#split_node{closures=Closures} = sched:get_node_info(MyNodeID, Sched),
	closure:extract_structs(Closures).
		
	% we first analyze all the schedulable nodes
	% then we try the loop heads that we found
	% if a loop head was analyzed, we try the schedulable nodes again
	% we end if no schedulable nodes are left and the loops that we found are all our parent's issue
analyze_till_fixed_point(MyNodeID, ParentSplitNodes, Worklist, LoopMarkers, Sched, Loader) ->
	?f("node ~s analyze_till_fixed_point", [pretty:string(MyNodeID)]),
	case sched:separate_schedulable_nodes(Worklist, Sched) of
		{[], Unschedulables} -> 
			?f("finished analyzing schedulable nodes; ~w are still open and we have ~w loop markers", [length(Unschedulables), length(LoopMarkers)]),
			analyze_loop_markers(MyNodeID, ParentSplitNodes, Unschedulables, LoopMarkers, Sched, Loader);
		{Schedulables, Unschedulables} ->
			?f("found ~w schedulable nodes: ~s", [length(Schedulables), pretty:string(Schedulables)]),
			analyze_schedulable_nodes(MyNodeID, ParentSplitNodes, Schedulables, Unschedulables, LoopMarkers, Sched, Loader)
	end.

analyze_schedulable_nodes(MyNodeID, ParentSplitNodes, Schedulables, Unschedulables, LoopMarkers, Sched, Loader) ->
	?f("node ~s analyze_schedulable_nodes", [pretty:string(MyNodeID)]),
	%delay computation so that the debug output has a chance to appear in the terminal...
	timer:sleep(20),
	%
	{OpenNodes, LoopMarkers2, MergedSched} = lists:foldl(
		fun(ChildNodeID, {NewNodesAcc, LoopMarkersAcc, SchedAcc}) ->
			ChildHeap = sched:compute_incoming_heap(ChildNodeID, Sched),
			{NewChildNodes, NewLoopMarkers, ChildSched} = node:analyze(ChildNodeID, ParentSplitNodes, ChildHeap, Sched, Loader),
			%TODO make sure we deal with genuine edges here!!
			{NewChildNodes ++ NewNodesAcc, NewLoopMarkers ++ LoopMarkersAcc, sched:plus(ChildSched, SchedAcc)}
		end,
		{Unschedulables, LoopMarkers, Sched},
		Schedulables
	),
	analyze_till_fixed_point(MyNodeID, ParentSplitNodes, OpenNodes, LoopMarkers2, MergedSched, Loader).

analyze_loop_markers(MyNodeID, ParentSplitNodes, Unschedulables, LoopMarkers, Sched, Loader) ->
	{OpenNodes, OpenLoopMarkers, MergedSched} = lists:foldl(
		fun(#loop{}=Marker, {NewNodesAcc, LoopMarkersAcc, SchedAcc}) ->
			case Marker#loop.head_id of
				MyNodeID -> %we are the loop head
					%shift the heaps etc...
					{NewChildNodes, NewLoopMarkers, ChildSched} = analyze_after_loop(Marker#loop.node_id, ParentSplitNodes, Marker#loop.heap, Sched, Loader),
					{NewChildNodes ++ NewNodesAcc, NewLoopMarkers ++ LoopMarkersAcc, sched:plus(ChildSched, SchedAcc)};
				_Some ->
					{NewNodesAcc, [Marker|LoopMarkersAcc], SchedAcc}
			end
		end,
		{Unschedulables, [], Sched},
		LoopMarkers		
	),
	if
		LoopMarkers =:= OpenLoopMarkers ->
			%we didn't achieve anything, so we are done here; we will return to the parent node's analysis
			{OpenNodes, OpenLoopMarkers, MergedSched};
		true ->
			%we computed some loop head, so we now can try to analyse more of the previously unschedulable ones
			analyze_till_fixed_point(MyNodeID, ParentSplitNodes, OpenNodes, OpenLoopMarkers, MergedSched, Loader)
	end.
		
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
	