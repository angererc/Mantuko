-module (analyzer).

-include("include/values.hrl").
-include("include/instructions.hrl").
-include("include/nodes.hrl").

-export ([analyze_file/2, analyze_string/2]).

analyze_file(Filename, Options) ->
	analyze(loader:load_from_file(Filename), Options).
	
analyze_string(String, Options) ->
	analyze(loader:load_from_string(String), Options).
	
analyze({error, Reason}, _Options) ->
	events:fatal("load error: ~p", [Reason]);
analyze(Loader, Options) ->
	global_options:set(Options),
	Loader2 = create_init_and_exit_blocks(Loader),
	
	%create some IDs
	RootNodeRef = refs:root_activation_ref(),
	ThisLoc = refs:loc(1, RootNodeRef),
		
	%create a root branch node with the main block as the only option
	RootNode = branch_in_node:add_option(
						#block_ref{nth=2, name={main}}, 
						ThisLoc, 
						branch_in_node:new()),
	
	%create a heap
	Heap1 = heap:new_struct(ThisLoc, heap:new()),
	Heap2 = heap:new_node(RootNodeRef, RootNode, Heap1),
	
	events:log("analysis comes ~w", [here]).
	

	
% *********************
% *********************
create_init_and_exit_blocks(Loader) ->
	ExitBlock = #block{
		name={exit}, filename="<none>", 
		start_line=-99, end_line=-99, 
		body=[]
	},
	loader:add_block(ExitBlock, Loader).