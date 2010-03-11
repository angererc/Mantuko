-module (events).

-export ([fatal/2, warning/2, log/2, debug/2]).

-define (FATAL, 1).
-define (WARNING, 2).
-define (LOG, 3).
-define (DEBUG, 4).

fatal(String, Params) ->
	display_string(?FATAL, String, Params).
	
warning(String, Params) ->
	display_string(?WARNING, String, Params).

log(String, Params) ->
	display_string(?LOG, String, Params).
		
debug(String, Params) ->
	display_string(?DEBUG, String, Params).
	
display_string(Level, String, Params) ->
	global_options:do_if_option_predicate(
		fun()-> erlang:display(lists:flatten(io_lib:format(String, Params))) end, 
		trace_verbosity, 
		fun(OptLevel) -> OptLevel >= Level end, ?FATAL					
	).