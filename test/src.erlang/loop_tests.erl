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
			{split_node, analyze},
			{split_node, analyze_till_fixed_point},
			{split_node, analyze_schedulable_nodes},
			{union_node, analyze},
			{atom_node, analyze},
			{intrinsics}
			%{atom_node, analyze_instruction}
		]},
		{trace_verbosity, 9}]).