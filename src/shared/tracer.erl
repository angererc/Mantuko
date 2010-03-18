-module (tracer).

-include("include/debug.hrl").

-export ([start/1]).

start([]) ->
	ok;
start(MFAs) ->
	Caller = self(),
	spawn(fun()->
		lists:foreach(fun(MFA)->
			add_trace(MFA)
		end, MFAs),
		erlang:trace(Caller, true, [call, procs]),
		Caller ! {self(), started},
		trace_loop()
	end),
	receive
		{_TracerProcess, started} ->
			ok
	end.
	
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
		
trace_loop() ->
	receive
		{trace, _, call, {M, F, _Args}} ->
			debug:log("Call: ~p:~p", [M, F]),
			trace_loop();
		{trace, _, return_from, Call, _Ret} ->
			debug:log("Return From: ~p", [Call]);
		Other ->
			debug:log("Other = ~p", [Other]),
			trace_loop()
	end.