-module (sched).

-export ([new/0]).
-export ([new_edge/3, set_node/3, set_result/3]).
-export ([get_node/2, is_schedulable/2]).
-export ([in_neighbours/2]).

-record (sched, {nodes, edges, results}).

new() ->
	#sched{nodes=dict:new(), edges=[], results=dict:new()}.
	
new_edge(SourceNodeID, TargetNodeID, Sched) ->
	Sched#sched{edges=[{SourceNodeID, TargetNodeID}|Sched#sched.edges]}.
	
set_node(NodeID, Node, Sched) ->
	Sched#sched{nodes=dict:store(NodeID, Node, Sched#sched.nodes)}.
	
set_result(NodeID, Heap, Sched) ->
	Sched#sched{results=dict:store(NodeID, Heap, Sched#sched.results)}.
	
get_node(NodeID, Sched) ->
	case dict:find(NodeID, Sched#sched.nodes) of
		{ok, Value} ->
			Value;
		error ->
			debug:fatal("asked for node '~p' that doesn't exist!!!", [NodeID]),
			error
	end.
	
in_neighbours(NodeID, Sched) ->
	lists:foldl(
			fun({Source, Target}, Acc)->
			if
				Target =:= NodeID -> [Source|Acc];
				true -> Acc
			end
		end,
		[],
		Sched#sched.edges).
	
is_schedulable(NodeID, Sched) ->
	IncomingNodes = in_neighbours(NodeID, Sched),
	%if we have a result for every incoming node, we are good to go...
	lists:all(fun(In)-> dict:is_key(In, Sched#sched.results) end, IncomingNodes).