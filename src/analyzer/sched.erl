-module (sched).

-export ([new/0]).
-export ([new_edge/3, set_node/3, set_result/3]).
-export ([get_node/2, get_schedulable_nodes/1, is_schedulable/2]).
-export ([in_neighbours/2, merge/2]).

-record (sched, {nodes, in_edges, open, results}).

new() ->
	#sched{nodes=dict:new(), in_edges=dict:new(), open=sets:new(), results=dict:new()}.
		
new_edge(SourceNodeID, TargetNodeID, Sched) ->
	Sched#sched{in_edges=dict:append(TargetNodeID, SourceNodeID, Sched#sched.in_edges)}.
	
set_node(NodeID, Node, Sched) ->
	Sched#sched{nodes=dict:store(NodeID, Node, Sched#sched.nodes), open=sets:add_element(NodeID, Sched#sched.open)}.
	
set_result(NodeID, Heap, Sched) ->
	%assert that the node is really registered
	true = dict:is_key(NodeID, Sched#sched.nodes),
	Sched#sched{results=dict:store(NodeID, Heap, Sched#sched.results), open=sets:del_element(NodeID, Sched#sched.open)}.
	
get_node(NodeID, Sched) ->
	case dict:find(NodeID, Sched#sched.nodes) of
		{ok, Value} ->
			Value;
		error ->
			debug:fatal("asked for node '~p' that doesn't exist!!!", [NodeID]),
			error
	end.
	
is_schedulable(NodeID, Sched) ->
	IncomingNodes = in_neighbours(NodeID, Sched),
	%if we have a result for every incoming node, we are good to go...
	lists:all(fun(In)-> dict:is_key(In, Sched#sched.results) end, IncomingNodes).
	
% -> [NodeIDs]
get_schedulable_nodes(Sched) ->
	sets:fold(fun(NodeID, Acc)->
			case is_schedulable(NodeID, Sched) of
				true -> [NodeID|Acc];
				false -> Acc
			end
		end,
		[],
		Sched#sched.open
	).
	
in_neighbours(NodeID, Sched) ->
	case dict:find(NodeID, Sched#sched.in_edges) of
		{ok, InNodes} -> InNodes;
		error -> []
	end.
		
merge(S1, S2) ->
	#sched{
		%must be the same node in both, i think?!?
		nodes=dict:merge(fun(_NodeID, Node, Node)-> Node end, S1#sched.nodes, S2#sched.nodes),
		in_edges=dict:merge(fun(_SourceNode, E1, E2)-> E1 ++ E2 end, S1#sched.in_edges, S2#sched.in_edges),
		open=sets:union(S1#sched.open, S2#sched.open), 
		%must be the same result or rather, they should be distinct in the first place?!?
		results=dict:merge(fun(_NodeID, Res, Res)-> erlang:error("hm... not sure if that's allowed") end, S1#sched.results, S2#sched.results)
	}.
	