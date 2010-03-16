-module (branch_in_node).

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
	
analyze(ActivationRef, #branch_in_node{activation_options=AOs}, Parents, Heap, Loader) ->
	case check_for_loop(AOs, Parents, Heap) of
		{true, _Parent} ->
			loop_found;
		false ->
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
				{Heap, []}, AOs)
	end.
	
check_for_loop(_MyActivationOptions, [], _Heap) ->
	false;
check_for_loop(MyActivationOptions, [Parent|Rest], Heap) ->
	#branch_in_node{activation_options=ParentActivationOptions} = heap:get(Parent, Heap),
	case sets:size(sets:intersection(MyActivationOptions, ParentActivationOptions)) of
		0 -> check_for_loop(MyActivationOptions, Rest, Heap);
		_Else -> {true, Parent}
	end.
	