-module (debug).

-export ([setup_tracing/0, fatal/2, warning/2, log/2, log_no_indent/2, debug/2]).

-define (FATAL, 1).
-define (WARNING, 2).
-define (LOG, 3).
-define (DEBUG, 4).

setup_tracing() ->
	case global_options:get(trace) of
		undefined ->
			ok;
		MFAs ->
			tracer:start(MFAs)
	end.

get_indentation() ->
	case tracer:is_started() of
		false ->
			"";
		true ->
			tracer:indentation()
	end.	
	
fatal(String, Params) ->
	display_string(?FATAL, String, Params).
	
warning(String, Params) ->
	display_string(?WARNING, String, Params).

log(String, Params) ->
	display_string(?LOG, "~s"++String, [get_indentation()|Params]).
		
log_no_indent(String, Params) ->
	display_string(?LOG, String, Params).
	
debug(String, Params) ->
	display_string(?DEBUG, String, Params).
	
display_string(Level, String, Params) ->
	global_options:do_if_option_predicate(
		fun()-> io:format(user, "~s~n", [lists:flatten(io_lib:format(String, Params))]) end, 
		trace_verbosity, 
		fun(OptLevel) -> OptLevel >= Level end, ?FATAL					
	).