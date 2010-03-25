-module (analyzer).

-include("include/debug.hrl").
-include("include/values.hrl").
-include("include/instructions.hrl").

-export ([analyze_file/2, analyze_string/2]).

analyze_file(Filename, Options) ->
	analyze(loader:load_from_file(Filename), Options).
	
analyze_string(String, Options) ->
	analyze(loader:load_from_string(String), Options).
	
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
	{ExitNode, Sched2} = split_node:create_union_node(RootNodeID, Sched),
	
	Heap = heap:new_struct(ThisLoc, heap:new()),
	
	node:analyze(RootNodeID, [], Heap, Sched2, Loader),
	node:analyze(ExitNode).
	
