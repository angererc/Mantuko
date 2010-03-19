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
			{split_node},
			{union_node},
			{atom_node, analyze},
			{atom_node, analyze_instruction},
			{heap}
		]},
		{trace_verbosity, 9}]).