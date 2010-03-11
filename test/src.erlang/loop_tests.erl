-module (loop_tests).

-include_lib ("eunit/include/eunit.hrl").
-include("include/values.hrl").

loop_1_test() ->
	analyzer:analyze_file("test/src.mantuko/loop_1.masm", [{trace_verbosity, 9}]).