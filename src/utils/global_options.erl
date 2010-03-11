-module (global_options).

-export ([set_global_options/1, tear_down_global_options/0]).
-export ([do_if_global_option_equals/4, do_if_global_option_predicate/4]).
-export ([global_option/1, global_option/2]).

% @type global_options() = [global_option()].

% @type global_option() = 
%	{trace_verbosity, integer()} | {trace_level, integer()} | {tracer_stop_after_num_severe, integer()}
%	| {trace_write_to_directory, string()}
% .
% * trace_verbosity defines which entries will be written to the terminal while running
% * trace_level defines which entries will be kept
% FATAL = 1, SEVERE = 2, WARNING = 3, TRACE = 5, TRACE_VERBOSE = 5, LOG = 6, DEBUG = 7

set_global_options(OptionOrOptions) ->
	lazy_ets:insert(global_options, OptionOrOptions).
	
tear_down_global_options() ->
	lazy_ets:delete(global_options).
		
% -> Fun() | false
do_if_global_option_equals(Fun, Option, Value, Default) ->
	OptValue = global_option(Option, Default),
	if
		OptValue =:= Value -> Fun();
		true -> false
	end.
	
do_if_global_option_predicate(Fun, Option, Pred, Default) ->
	case Pred(global_option(Option, Default)) of
		true -> Fun();
		false -> false
	end.

global_option(Option) ->
	global_option(Option, undefined).
		
global_option(Option, Default) ->
	case lazy_ets:lookup(global_options, Option) of
		[{Option, Value}] ->
			Value;
		undefined ->
			Default
	end.
	