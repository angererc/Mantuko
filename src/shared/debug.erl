-module (debug).

-include("include/debug.hrl").

-export ([setup_tracing/0, fatal/2, warning/2, log/2, debug/2]).
-export ([set_context/1, get_context/0]).
-export ([args_to_string/1, val_to_string/1]).

setup_tracing() ->
	case global_options:get(trace) of
		undefined ->
			tracer:start([]);
		MFAs ->
			tracer:start(MFAs)
	end.

set_context(Key, SomeInfo) ->
	put({debug_info_context, Key}, SomeInfo).
	
get_context(Key) ->
	get({debug_info_context, Key}).
		
fatal(String, Params) ->
	tracer:display_string(?FATAL, String, Params).
	
warning(String, Params) ->
	tracer:display_string(?WARNING, String, Params).

log(String, Params) ->
	tracer:display_string(?LOG, String, Params).
	
debug(String, Params) ->
	tracer:display_string(?DEBUG, String, Params).
	
%use those to format long unreadable things into something shorter and nicer
args_to_string([]) ->
	"";
args_to_string([Arg]) ->
	val_to_string(1, Arg);
args_to_string([Arg|Rest]) ->
	val_to_string(1, Arg) ++ ", " ++ args_to_string(Rest).

val_to_string(V) ->
	val_to_string(1, V).
		
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