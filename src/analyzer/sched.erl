-module (sched).

-include("include/heap.hrl").

-export ([new/0]).
-export ([new_edge/3, set_node/3, set_result/3]).
-export ([get_node/2, is_schedulable/2]).
-export ([in_neighbours/2]).

-record (sched, {nodes, edges, results}).

new() ->
	#sched{nodes=dict:new(), edges=[], results=dict:new()}.
	
new_edge(SourceActivationRef, TargetActivationRef, Sched) ->
	Sched#sched{edges=[{SourceActivationRef, TargetActivationRef}|Sched#sched.edges]}.
	
set_node(ActivationRef, Node, Sched) ->
	Sched#sched{nodes=dict:store(ActivationRef, Node, Sched#sched.nodes)}.
	
set_result(ActivationRef, Heap, Sched) ->
	Sched#sched{results=dict:store(ActivationRef, Heap, Sched#sched.results)}.
	
get_node(ActivationRef, Sched) ->
	case dict:find(ActivationRef, Sched#sched.nodes) of
		{ok, Value} ->
			Value;
		error ->
			debug:fatal("asked for node '~p' that doesn't exist!!!", [ActivationRef]),
			error
	end.
	
in_neighbours(ActivationRef, Sched) ->
	lists:foldl(
			fun({Source, Target}, Acc)->
			if
				Target =:= ActivationRef -> [Source|Acc];
				true -> Acc
			end
		end,
		[],
		Sched#sched.edges).
	
is_schedulable(ActivationRef, Sched) ->
	IncomingNodes = in_neighbours(ActivationRef, Sched),
	%if we have a result for every incoming node, we are good to go...
	lists:all(fun(In)-> dict:is_key(In, Sched#sched.results) end, IncomingNodes).