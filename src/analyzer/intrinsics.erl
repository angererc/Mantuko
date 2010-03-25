-module (intrinsics).

-include("include/debug.hrl").
-include("include/values.hrl").

-compile(export_all).

is_nummeric(#num{}) -> true;
is_nummeric(#transformed_value{type=num}) -> true;
is_nummeric(_Else) -> false.

% intrinsics must return:
% a tuple with the updated schedule and heap and a list of results
% -> {[NewNodes], Sched2, Heap2, [ResultValues]}
geq(Sched, Heap, _Nth, _Now, LHS, RHS) ->
	true = is_nummeric(LHS),
	true = is_nummeric(RHS),
	{[], Sched, Heap, [#some{type=boolean}]}.
	
inc(Sched, Heap, Nth, Now, Parent) ->
	true = is_nummeric(Parent),
	{[], Sched, Heap, [#transformed_value{type=num, 
										nth=Nth, 
										node_id=Now,
										parent_value=Parent, 
										operation={plus, 1}}]}.
	
branch(Sched, Heap, Nth, Now, _Test, FalseBlockRef, FalseStructLoc, TrueBlockRef, TrueStructLoc) ->
	TrueClause = closure:new(TrueBlockRef, TrueStructLoc),
	FalseClause = closure:new(FalseBlockRef, FalseStructLoc),
	
	NewNodeID = node:split_node_id(Nth, Now),
	NewNode = split_node:add_closure(FalseClause, split_node:add_closure(TrueClause, split_node:new())),

	Sched2 = sched:set_node_info(NewNodeID, NewNode, Sched),
	{UnionNodeID, Sched3} = split_node:create_union_node(NewNodeID, Sched2),
	Sched4 = sched:new_edge(Now, NewNodeID, Sched3),
	
	{[NewNodeID, UnionNodeID], Sched4, Heap, [NewNodeID]}.