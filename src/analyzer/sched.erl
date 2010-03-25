-module (sched).

-include("include/debug.hrl").

-export ([new_empty_schedule/0]).
-export ([new_edge/3, new_edge_no_smartness/3]).
-export ([assert_invariants/2]).
-export ([set_node_info/3, get_node_info/2, has_node_info/2, has_result/2, set_result/3, get_result/2]).
-export ([separate_schedulable_nodes/2, is_schedulable/2]).
-export ([in_neighbours/2, plus/2]).
-export ([compute_incoming_heap/2]).

-record (sched, {node_infos, in_edges, results}).

new_empty_schedule() ->
	#sched{node_infos=dict:new(), in_edges=dict:new(), results=dict:new()}.

% the invariants are:
% all new_nodes and results have to have corresponding node_infos
% there can be more node_infos than that
% all edges must have node_infos
% new_nodes and results must be disjoint
assert_invariants(Worklist, Sched) ->
	%new_nodes and results are disjoint
	true = lists:all(fun(Node)-> not dict:is_key(Node, Sched#sched.results) end, Worklist),
	% we have infos about all nodes used in worklist, in_edges, results
	AllUsedNodes = Worklist ++ dict:fetch_keys(Sched#sched.results) ++ dict:fetch_keys(Sched#sched.in_edges),
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
	true = has_node_info(SourceNodeID, Sched),
	true = has_node_info(TargetNodeID, Sched),
	?f("adding edge ~s -> ~s", [pretty:string(SourceNodeID), pretty:string(TargetNodeID)]),
	Sched#sched{in_edges=dict:append(TargetNodeID, SourceNodeID, Sched#sched.in_edges)}.
			
%uses the union node instead of the split node if necessary for the edge source	
new_edge(SourceNodeID, TargetNodeID, Sched) ->
	new_edge_no_smartness(node:node_as_edge_source(SourceNodeID), TargetNodeID, Sched).
			
set_node_info(NodeID, Node, Sched) ->
	false = has_node_info(NodeID, Sched),
	Sched#sched{node_infos=dict:store(NodeID, Node, Sched#sched.node_infos)}.

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
	true = has_node_info(NodeID, Sched),
	Sched#sched{results=dict:store(NodeID, Heap, Sched#sched.results)}.
	
get_result(NodeID, Sched) ->
	case dict:find(NodeID, Sched#sched.results) of
		{ok, Value} -> 
			Value;
		error ->
			debug:fatal("asked for result of node '~p' that doesn't exist!!!", [NodeID]),
			error
	end.
	
is_schedulable(NodeID, Sched) ->
	true = has_node_info(NodeID, Sched),
	IncomingNodes = in_neighbours(NodeID, Sched),
	%if we have a result for every incoming node, we are good to go...
	lists:all(fun(InNode)-> has_result(InNode, Sched) end, IncomingNodes).

% -> same | concurrent | exclusive | left_hb_right | right_hb_left
%get_relationship(SameNodeID, SameNodeID, _Sched) ->
%	same;
%get_relationship(NodeID1, NodeID2, Sched) ->
%	?!? do something here..
	% if the common parent is a split node, we are exclusive
	% if not, search for a path
	
% all from get_new_nodes wher is_schedulable is true	
% -> {[SchedulableNodeIDs], [NotSchedulable]}
separate_schedulable_nodes(Worklist, Sched) ->
	lists:foldl(fun(NodeID, {Schedulable, Unschedulable})->
			case is_schedulable(NodeID, Sched) of
				true -> {[NodeID|Schedulable], Unschedulable};
				false -> {Schedulable, [NodeID|Unschedulable]}
			end
		end,
		{[], []},
		Worklist
	).
	
in_neighbours(NodeID, Sched) ->
	case dict:find(NodeID, Sched#sched.in_edges) of
		{ok, InNodes} -> InNodes;
		error -> []
	end.
		
plus(S1, S2) ->
	NodeInfos = dict:merge(
					fun(_NodeID, Info1, Info2)-> node:merge(Info1, Info2) end, 
					S1#sched.node_infos, 
					S2#sched.node_infos),
	InEdges = dict:merge(
					fun(_SourceNode, Sources1, Sources2)-> NewIn2 = lists:subtract(Sources2, Sources1), Sources1++NewIn2 end, 
					S1#sched.in_edges, 
					S2#sched.in_edges),
	Results = dict:merge(
					fun(_NodeID, Same, Same)-> 
						Same
					end, 
					S1#sched.results, 
					S2#sched.results),
		
	#sched{
		node_infos = NodeInfos,
		in_edges=InEdges,
		results=Results
	}.
	
compute_incoming_heap(NodeID, Sched) ->
	InNodes = in_neighbours(NodeID, Sched),
	?f("compute incomng heap for ~s: ~w incoming edges found from ~s", [pretty:string(NodeID), length(InNodes), pretty:string(InNodes)]),
	
	[InHeap] = lists:map(fun(InNode)-> get_result(InNode, Sched) end, InNodes),
	InHeap.
	