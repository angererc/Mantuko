-module (sched).

-include("include/heap.hrl").

-export ([new/0]).
-export ([new_edge/3, set_node/3, set_result/3]).
-export ([get_node/2, is_schedulable/2]).

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
			debug:fatal("asked for a node that doesn't exist!!!", []),
			error
	end.
	
is_schedulable(_ActivationRef, _Sched) ->
	false.