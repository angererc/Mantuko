-module(test_suite).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
  [
	{module, parser_tests},
	{module, sched_tests},
	{module, loop_tests}
  ].