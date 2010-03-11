-module (analyzer).

-export ([analyze_file/2, analyze_string/2]).

analyze_file(Filename, Options) ->
	analyze(loader:load_from_file(Filename), Options).
	
analyze_string(String, Options) ->
	analyze(loader:load_from_string(String), Options).
	
analyze(_Repository, Options) ->
	global_options:set(Options),
	events:log("analysis comes ~w", [here]).
	