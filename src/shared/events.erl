-module (events).

-export ([fatal/2, warning/2, debug/2]).

-define (FATAL, 1).
-define (WARNING, 2).
-define (DEBUG, 3).

fatal(String, Params) ->
	display_string(?FATAL, String, Params).
	
warning(String, Params) ->
	display_string(?WARNING, String, Params).
	
debug(String, Params) ->
	display_string(?DEBUG, String, Params).
	
display_string(Level, String, Params) ->
	global_options:do_if_option_predicate(
		fun()-> io_lib:format(String, Params) end, 
		trace_verbosity, 
		fun(OptLevel) -> OptLevel >= Level end, -1					
	).