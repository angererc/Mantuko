-module (intrinsics).

-include("include/debug.hrl").
-include("include/values.hrl").

-compile(export_all).

% intrinsics can return:
% a list of results, a single result,
% or a tuple with the updated schedule and heap and a list of results
geq(_Sched, _Heap, _Nth, _Now, #num{value=Lhs}, #num{value=Rhs}) ->
	if
		Lhs >= Rhs -> true;
		true -> false
	end.
	
branch(Sched, Heap, Nth, Now, _Test, FalseBlockRef, FalseStructLoc, TrueBlockRef, TrueStructLoc) ->
	TrueClause = closure:new(TrueBlockRef, TrueStructLoc),
	FalseClause = closure:new(FalseBlockRef, FalseStructLoc),
	
	NewNodeID = node:split_node_id(Nth, Now),
	NewNode = split_node:add_closure(FalseClause, split_node:add_closure(TrueClause, split_node:new())),
	Sched2 = sched:new_node(NewNodeID, Sched),
	Sched3 = sched:set_node_info(NewNodeID, NewNode, Sched2),
	Sched4 = split_node:create_union_node(NewNodeID, Sched3),
	Sched5 = sched:new_edge(Now, NewNodeID, Sched4),
	
	{Sched5, Heap, [NewNodeID]}.