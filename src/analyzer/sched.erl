-module (sched).

-include("include/debug.hrl").

-export ([new_empty_schedule/0, new_child_schedule/1]).
-export ([new_edge/3, new_edge_no_smartness/3, new_node/2, is_new_node/2]).
-export ([remove_new_nodes/2]).
-export ([set_node_info/3, get_node_info/2, has_node_info/2, has_result/2, set_result/3, get_result/2]).
-export ([get_schedulable_nodes/1, get_new_nodes/1, is_schedulable/2]).
-export ([in_neighbours/2, merge/2]).
-export ([compute_incoming_heap/2]).

-record (sched, {new_nodes, node_infos, in_edges, results}).

new_empty_schedule() ->
	assert_invariants(
		#sched{new_nodes=[], node_infos=dict:new(), in_edges=dict:new(), results=dict:new()}
	).

new_child_schedule(Parent) ->
	assert_invariants(
		Parent#sched{new_nodes=[]}
	).

% the invariants are:
% all new_nodes and results have to have corresponding node_infos
% there can be more node_infos than that
% all edges must have node_infos
% new_nodes and results must be disjoint
assert_invariants(Sched) ->
	%new_nodes and results are disjoint
	true = lists:all(fun(Node)-> not dict:is_key(Node, Sched#sched.results) end, Sched#sched.new_nodes),
	% we have infos about all nodes used in new_nodes, in_edges, results
	AllUsedNodes = Sched#sched.new_nodes ++ dict:fetch_keys(Sched#sched.results) ++ dict:fetch_keys(Sched#sched.in_edges),
	AllUsedNodes2 = dict:fold(fun(_, Sources, Acc)-> Sources ++ Acc end, AllUsedNodes, Sched#sched.in_edges),
	case lists:all(fun(Node)-> dict:is_key(Node, Sched#sched.node_infos) end, AllUsedNodes2) of
		true -> ok;
		false ->
			?f("sched assertion failed: not all used nodes have a corresponding info: used=~s, defined=~s", 
				[pretty:string(lists:sort(sets:to_list(sets:from_list(AllUsedNodes2)))), 
				 pretty:string(lists:sort(dict:fetch_keys(Sched#sched.node_infos)))])
	end,
	Sched.

new_edge_no_smartness(SourceNodeID, TargetNodeID, Sched) ->
	?f("adding edge ~s -> ~s", [pretty:string(SourceNodeID), pretty:string(TargetNodeID)]),
	assert_invariants(
		Sched#sched{in_edges=dict:append(TargetNodeID, SourceNodeID, Sched#sched.in_edges)}
	).
				
new_edge(SourceNodeID, TargetNodeID, Sched) ->
	new_edge_no_smartness(node:node_as_edge_source(SourceNodeID), TargetNodeID, Sched).

new_node(NodeID, Sched) ->
	false = has_result(NodeID, Sched),
	false = has_node_info(NodeID, Sched),
	%we don't assert the invariants here, because we want users to first do a new_node
	%and then a set_node_info, not the other way round to avoid that you might forget to add it as a new node
	%assert_invariants(
		Sched#sched{new_nodes=[NodeID|Sched#sched.new_nodes]}.
	%).

remove_new_nodes(NodeList, Sched) ->
	Sched#sched{new_nodes=lists:subtract(Sched#sched.new_nodes, NodeList)}.
		
% -> [NodeID]
get_new_nodes(Sched) ->
	Sched#sched.new_nodes.
	
is_new_node(NodeID, Sched) ->
	lists:member(NodeID, Sched#sched.new_nodes).
		
set_node_info(NodeID, Node, Sched) ->
	true = is_new_node(NodeID, Sched),
	assert_invariants(
		Sched#sched{node_infos=dict:store(NodeID, Node, Sched#sched.node_infos)}
	).

get_node_info(NodeID, Sched) ->
	case dict:find(NodeID, Sched#sched.node_infos) of
		{ok, Value} ->
			Value;
		error ->
			debug:fatal("asked for node '~p' that doesn't exist!!!", [NodeID]),
			error
	end.

has_node_info(NodeID, Sched) ->
	dict:is_key(NodeID, Sched#sched.node_infos).

has_result(NodeID, Sched) ->
	dict:is_key(NodeID, Sched#sched.results).
	
set_result(NodeID, Heap, Sched) ->
	%TODO here I could assert that the NodeID is the one of the currently analyzed node
	%but I cannot assert that the NodeID is in the new_nodes list because a node works on a
	%"child copy" of the parent schedule, that is, it removes all the neew_nodes including itself
	%I can assert some other things, though
	false = has_result(NodeID, Sched),
	% is_new_node is false because a node removes itself from the new list
	% by working on a child copy of the parent schedule
	false = is_new_node(NodeID, Sched),
	assert_invariants(
		Sched#sched{
			new_nodes=lists:delete(NodeID, Sched#sched.new_nodes),
			results=dict:store(NodeID, Heap, Sched#sched.results)}
	).
	
get_result(NodeID, Sched) ->
	case dict:find(NodeID, Sched#sched.results) of
		{ok, Value} -> 
			Value;
		error ->
			debug:fatal("asked for result of node '~p' that doesn't exist!!!", [NodeID]),
			error
	end.
	
is_schedulable(NodeID, Sched) ->
	true = is_new_node(NodeID, Sched),
	
	IncomingNodes = in_neighbours(NodeID, Sched),
	%if we have a result for every incoming node, we are good to go...
	lists:all(fun(InNode)-> has_result(InNode, Sched) end, IncomingNodes).

% all from get_new_nodes wher is_schedulable is true	
% -> [NodeIDs]
get_schedulable_nodes(Sched) ->
	lists:foldl(fun(NodeID, Acc)->
			case is_schedulable(NodeID, Sched) of
				true -> [NodeID|Acc];
				false -> Acc
			end
		end,
		[],
		get_new_nodes(Sched)
	).
	
in_neighbours(NodeID, Sched) ->
	case dict:find(NodeID, Sched#sched.in_edges) of
		{ok, InNodes} -> InNodes;
		error -> []
	end.
		
merge(S1, S2) ->
	NodeInfos = dict:merge(
					fun(_NodeID, Info1, Info2)-> node:merge(Info1, Info2) end, 
					S1#sched.node_infos, 
					S2#sched.node_infos),
	InEdges = dict:merge(
					fun(_SourceNode, Sources1, Sources2)-> Sources1 ++ Sources2 end, 
					S1#sched.in_edges, 
					S2#sched.in_edges),
	Results = dict:merge(
					fun(_NodeID, Same, Same)-> 
						Same
					end, 
					S1#sched.results, 
					S2#sched.results),
		
	assert_invariants(
		#sched{
			new_nodes = S1#sched.new_nodes ++ S2#sched.new_nodes,
			node_infos = NodeInfos,
			in_edges=InEdges,
			results=Results
		}
	).
	
compute_incoming_heap(NodeID, Sched) ->
	InNodes = in_neighbours(NodeID, Sched),
	?f("compute incomng heap: ~w ~w", [NodeID, InNodes]),
	[InHeap] = lists:map(fun(InNode)-> get_result(InNode, Sched) end, InNodes),
	InHeap.
	