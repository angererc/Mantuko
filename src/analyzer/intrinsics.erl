-module (intrinsics).

-include("include/debug.hrl").
-include("include/values.hrl").

-compile(export_all).

% intrinsics must return:
% a tuple with the updated schedule and heap and a list of results
% -> {Sched2, Heap2, [ResultValues]}
geq(Sched, Heap, _Nth, _Now, #num{}=_LHS, #num{}=_RHS) ->
	{Sched, Heap, [#some{type=boolean}]}.
	
inc(Sched, Heap, Nth, Now, #num{}=Parent) ->
	{Sched, Heap, [#transformed_value{type=num, 
										nth=Nth, 
										node_id=Now,
										parent_value=Parent, 
										operation={plus, 1}}]};
inc(Sched, Heap, Nth, Now, #transformed_value{type=num}=Parent) ->
	{Sched, Heap, [#transformed_value{type=num, 
										nth=Nth, 
										node_id=Now,
										parent_value=Parent, 
										operation={plus, 1}}]}.
	
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