-module (union_node).

-include("include/debug.hrl").
-include("include/nodes.hrl").

-export ([new/0, analyze/5]).

new() ->
	#union_node{}.
	
analyze(MyNodeID, _ParentSplitNodes, Heap, Sched, _Loader) ->
	?f("analyzing node ~s", [pretty:string(MyNodeID)]),
	Sched2 = sched:set_result(MyNodeID, Heap, Sched),
	{[], [], Sched2}.