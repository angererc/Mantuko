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
		
args_to_string([]) ->
	"";
args_to_string([Arg]) ->
	val_to_string(1, Arg);
args_to_string([Arg|Rest]) ->
	val_to_string(1, Arg) ++ ", " ++ args_to_string(Rest).
	
val_to_string(Depth, V) when Depth > 2, is_list(V) ->
	io_lib:format("[...]", []);
val_to_string(Depth, V) when Depth > 2, is_tuple(V) ->
	io_lib:format("{...}", []);
val_to_string(_Depth, []) ->
	"[]";
val_to_string(Depth, [One]) ->
	io_lib:format("[~s]", [val_to_string(Depth+1, One)]);
val_to_string(Depth, [One, Two]) ->
	io_lib:format("[~s, ~s]", [val_to_string(Depth+1, One), val_to_string(Depth+1, Two)]);
val_to_string(Depth, [One, Two, Three]) ->
	io_lib:format("[~s, ~s, ~s]", [val_to_string(Depth+1, One), val_to_string(Depth+1, Two), val_to_string(Depth+1, Three)]);
val_to_string(Depth, [One, Two|Rest]) ->
	io_lib:format("[~s, ~s, <+~p>]", [val_to_string(Depth+1, One), val_to_string(Depth+1, Two), length(Rest)]);
val_to_string(Depth, {One}) ->
	io_lib:format("{~s}", [val_to_string(Depth+1, One)]);
val_to_string(Depth, {One, Two}) ->
	io_lib:format("{~s, ~s}", [val_to_string(Depth+1, One), val_to_string(Depth+1, Two)]);
val_to_string(Depth, {One, Two, Three}) ->
	io_lib:format("{~s, ~s, ~s}", [val_to_string(Depth+1, One), val_to_string(Depth+1, Two), val_to_string(Depth+1, Three)]);
val_to_string(Depth, A) when is_tuple(A) ->
	[F|R] = tuple_to_list(A),
	io_lib:format("{~s, <+~p>}", [val_to_string(Depth+1, F), length(R)]);
val_to_string(_Depth, A) ->
	io_lib:format("~p", [A]).
	
trace_loop([_S1,_S2,_S3,_S4|Rest]=Indent) ->
	receive
		{From, indentation} ->
			From ! {?MODULE, Indent},
			trace_loop(Indent);
		{trace, _, call, {M, F, Args}} ->
			debug:log_no_indent(Indent ++ "Call: ~p:~p/~p(~s)", [M, F, length(Args), args_to_string(Args)]),
			trace_loop("    "++Indent);
		{trace, _, return_from, {M, F, Arity}, Ret} ->
			debug:log_no_indent(Rest ++ "Return: ~p:~p/~p => ~s", [M, F, Arity, val_to_string(1, Ret)]),
			trace_loop(Rest);
		Other ->
			debug:log_no_indent(Indent ++ "Other = ~p", [Other]),
			trace_loop(Indent)
	end.