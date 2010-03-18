-module (debug).

-include_lib("stdlib/include/ms_transform.hrl").

-export ([setup_tracing/0, fatal/2, warning/2, log/2, debug/2]).

-define (FATAL, 1).
-define (WARNING, 2).
-define (LOG, 3).
-define (DEBUG, 4).

setup_tracing() ->
	case global_options:get(trace) of
		undefined ->
			ok;
		MFAs ->
			dbg:tracer(),
			lists:foreach(fun setup_tracing/1, MFAs),
			dbg:p(all,[c])
	end.
	
setup_tracing({Module, Function, Arity}) ->
	dbg:tpl(Module, Function, Arity, dbg:fun2ms(fun(_) -> return_trace() end));
setup_tracing({Module, Function}) ->
	dbg:tpl(Module, Function, dbg:fun2ms(fun(_) -> return_trace() end));
setup_tracing({Module}) ->
	dbg:tpl(Module, dbg:fun2ms(fun(_) -> return_trace() end));
setup_tracing(Module) when is_atom(Module) ->
	dbg:tpl(Module, dbg:fun2ms(fun(_) -> return_trace() end)).
	
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