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
analyze(Repository, Options) ->
	global_options:set(Options),
	_Repository2 = create_init_and_exit_blocks(Repository),
	
	%create an option node here and start analyzing it
	InitialNode = #option_node{},
	node:analyze(InitialNode),
	
	events:log("analysis comes ~w", [here]).
	

	
% *********************
% *********************
create_init_and_exit_blocks(Repository) ->
	BootstrapBlock = #block{
		name={init}, filename="<none>", 
		start_line=-1, end_line=-1, 
		body=[#schedule{
				line_no=-1, 
				reg=undefined, 
				block=#block_ref{nth=0, name={main}}, 
				struct=#this{}
		}]
	},
	ExitBlock = #block{
		name={exit}, filename="<none>", 
		start_line=-99, end_line=-99, 
		body=[]
	},
	loader:add_block(BootstrapBlock, loader:add_block(ExitBlock, Repository)).