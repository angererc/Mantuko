-module (analyzer).

-include("include/debug.hrl").
-include("include/values.hrl").
-include("include/instructions.hrl").

-export ([analyze_file/2, analyze_string/2]).

-spec analyze_file(string(), [{atom(), term()}]) -> sched:sched().
analyze_file(Filename, Options) ->
	analyze(loader:load_from_file(Filename), Options).
	
-spec analyze_string(string(), [{atom(), term()}]) -> sched:sched().
analyze_string(String, Options) ->
	analyze(loader:load_from_string(String), Options).
	
-spec analyze({error, term()}|loader:loader(), [{atom(), term()}]) -> sched:sched().
analyze({error, Reason}, _Options) ->
	debug:fatal("load error: ~p", [Reason]);
analyze(Loader, Options) ->
	global_options:set(Options),
	debug:setup_tracing(),
	%make sure the intrinsics are loaded
	intrinsics:module_info(),
	%create some IDs
	RootNodeID = node:root_node_id(),
	ThisLoc = object:struct_loc(1, RootNodeID),
		
	%create a root branch node with the main block as the only option
	RootNode = split_node:add_closure(
							closure:new(#block_ref{nth=1, name={main}}, ThisLoc), 
							split_node:new()),
	
	%create a schedule and heap
	Sched = sched:set_node_info(RootNodeID, RootNode, sched:new_empty_schedule()),
	{ExitNodeID, Sched2} = split_node:create_union_node(RootNodeID, Sched),
	
	Heap = heap:new_struct(RootNodeID, ThisLoc, heap:new()),
	
	{[], [], Sched3} = node:analyze(RootNodeID, [], Heap, Sched2, Loader),
	ExitHeap = sched:compute_incoming_heap(ExitNodeID, Sched3),
	{[], [], Sched4} = node:analyze(ExitNodeID, [], ExitHeap, Sched3, Loader),
	Sched4.
	
