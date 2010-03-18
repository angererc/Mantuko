-module (tracer).

-include("include/debug.hrl").

-export ([is_started/0, start/1, indentation/0]).

is_started() ->
	case whereis(?MODULE) of
		undefined -> false;
		_Else -> true
	end.
	
start([]) ->
	ok;
start(MFAs) ->
	Caller = self(),
	register(?MODULE, spawn(fun()->
		lists:foreach(fun(MFA)->
			add_trace(MFA)
		end, MFAs),
		erlang:trace(Caller, true, [call, procs]),
		Caller ! {self(), started},
		trace_loop("    ")
	end)),
	receive
		{_TracerProcess, started} ->
			ok
	end.
	
indentation() ->
	utils:rpc(?MODULE, indentation).
	
add_trace({M, _F, _A}=MFA) ->
	%force loading of module!
	M:module_info(),
	true = module_loaded(M),
	erlang:trace_pattern(
		MFA, 
		[{'_', [], [{return_trace}]}],
		[local]);
add_trace({M, F}) ->
	add_trace({M, F, '_'});
add_trace({M}) ->
	add_trace({M, '_', '_'});
add_trace(M) ->
	add_trace({M, '_', '_'}).
	
trace_loop([_S1,_S2,_S3,_S4|Rest]=Indent) ->
	receive
		{From, indentation} ->
			From ! {?MODULE, Indent},
			trace_loop(Indent);
		{trace, _, call, {M, F, Args}} ->
			debug:log_no_indent(Indent ++ "Call: \e[1;31m~p:~p/~p\e[0;33m(~s)\e[30m", [M, F, length(Args), debug:args_to_string(Args)]),
			trace_loop("    "++Indent);
		{trace, _, return_from, {M, F, Arity}, Ret} ->
			debug:log_no_indent(Rest ++ "Return: \e[31m~p:~p/~p \e[33m=> ~s\e[30m", [M, F, Arity, debug:val_to_string(Ret)]),
			trace_loop(Rest);
		Other ->
			debug:log_no_indent(Indent ++ "Other = ~p", [Other]),
			trace_loop(Indent)
	end.