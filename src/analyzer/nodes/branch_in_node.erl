-module (branch_in_node).

-include("include/debug.hrl").
-include("include/heap.hrl").

-export ([new/0, add_option/3, analyze/5]).

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
	
analyze(ActivationRef, #branch_in_node{activation_options=AOs}=Node, Parents, Heap, Loader) ->
	debug:log("XXXXX", []),
	case check_for_loop(AOs, Parents, Heap) of
		{true, _Parent} ->
			loop_found;
		false ->
			{Heap2, NewNodes} = create_nodes(ActivationRef, AOs, Heap),
			Heap3 = heap:set(ActivationRef, Node#branch_in_node{result_heap=Heap2}, Heap2),
			analyze_children(ActivationRef, NewNodes, [], [ActivationRef|Parents], Heap3, Loader)
	end.

create_nodes(ActivationRef, ActivationOptions, Heap)	->
	sets:fold( %create a node for each option and add the edges
		fun(Option, {HeapAcc, NewNodes})-> 
			NewActivationRef = refs:activation_ref(Option, ActivationRef),
			NewNode = option_node:new(Option),
			HeapAcc2 = heap:new_node(NewActivationRef, NewNode, HeapAcc),
			% our branch node created the new activation
			HeapAcc3 = heap:new_creation_edge(ActivationRef, NewActivationRef, HeapAcc2),
			% add a happens-before between the new activation and our branch_out node
			HeapAcc4 = heap:new_edge(
							NewActivationRef, 
							refs:activation_ref(refs:branch_out_node(), ActivationRef), 
							HeapAcc3),
			%and continue with the next option
			{HeapAcc4, [NewActivationRef|NewNodes]}
		end, 
		{Heap, []}, ActivationOptions).
		
%we added ourselves to Parents already!
analyze_children(_ActivationRef, [], Leftovers, _Parents, _Heap, _Loader) ->
	Leftovers;
analyze_children(ActivationRef, [ChildActivationRef|Rest], Leftovers, Parents, Heap, Loader) ->
	case heap:is_schedulable(ChildActivationRef, Heap) of
		true ->
			ChildHeap = heap:compute_incoming_heap(ChildActivationRef, Heap),
			ChildLeftovers = node:analyze(ChildActivationRef, Parents, ChildHeap, Loader),
			analyze_children(ActivationRef, Rest ++ ChildLeftovers, Leftovers, Parents, Heap, Loader);
		false ->
			analyze_children(ActivationRef, Rest, [ChildActivationRef, Leftovers], Parents, Heap, Loader)
	end.

	
	
check_for_loop(_MyActivationOptions, [], _Heap) ->
	false;
check_for_loop(MyActivationOptions, [Parent|Rest], Heap) ->
	% that's wrong; we have to check if there is any option with the same block, the
	% this pointer is not relevant
	#branch_in_node{activation_options=ParentActivationOptions} = heap:get(Parent, Heap),
	case sets:size(sets:intersection(MyActivationOptions, ParentActivationOptions)) of
		0 -> check_for_loop(MyActivationOptions, Rest, Heap);
		_Else -> {true, Parent}
	end.
	