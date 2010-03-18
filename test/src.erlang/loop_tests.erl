-module (loop_tests).

-include_lib ("eunit/include/eunit.hrl").
-include("include/values.hrl").

loop_1_test() ->
	%i:im(),
	%i:ii(analyzer),
	%i:iaa([init]),
	%i:ib(analyzer, analyze, 2),
	analyzer:analyze_file("test/src.mantuko/loop_1.masm", 
		[{trace, [
			{node},			
			{branch_in_node},
			{branch_out_node},
			{option_node},
			{heap}
		]},
		{trace_verbosity, 9}]).