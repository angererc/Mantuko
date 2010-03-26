-module (sched_tests).

-include_lib ("eunit/include/eunit.hrl").
-include("include/values.hrl").

happens_before_test() ->
	Sched1 = sched:new_empty_schedule(),
	Sched2 = sched:set_node_info(a, a, Sched1),
	Sched3 = sched:set_node_info(b, b, Sched2),
	Sched4 = sched:set_node_info(c, c, Sched3),
	Sched5 = sched:set_node_info(d, d, Sched4),
	Sched6 = sched:set_node_info(e, e, Sched5),
	Sched7 = sched:set_node_info(f, f, Sched6),
	Sched8 = sched:set_node_info(g, g, Sched7),
	Sched9 = sched:set_node_info(h, h, Sched8),
	
	Sched10 = sched:new_edge_no_smartness(a, b, Sched9),
	Sched11 = sched:new_edge_no_smartness(a, c, Sched10),
	Sched12 = sched:new_edge_no_smartness(a, d, Sched11),
	Sched13 = sched:new_edge_no_smartness(c, f, Sched12),
	Sched14 = sched:new_edge_no_smartness(e, h, Sched13),
	Sched15 = sched:new_edge_no_smartness(f, h, Sched14),
	Sched16 = sched:new_edge_no_smartness(g, h, Sched15),
	
	true = sched:happens_before(a, h, Sched16),
	false = sched:happens_before(h, a, Sched16),
	false = sched:happens_before(b, g, Sched16).