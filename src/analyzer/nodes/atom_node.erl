-module (atom_node).

-include("include/debug.hrl").
-include("include/loader.hrl").
-include("include/instructions.hrl").
-include("include/values.hrl").

-export ([new/1, analyze/5]).

-record (atom_node, {closure}).

new(Closure) ->
	#atom_node{closure=Closure}.
	
analyze(MyNodeID, _Parents, Heap, Sched, Loader) ->
	#atom_node{closure=Closure} = sched:get_node(MyNodeID, Sched),
	
	#block{name=N, filename=FN, start_line=S, end_line=E, body=Body} = loader:get_block(closure:block_ref(Closure), Loader),
	debug:set_context(block_info, lists:flatten(io_lib:format("block ~w in file ~s:~w-~w", [N, FN, S, E]))),
	?f("analyzing block ~w", [element(3,closure:block_ref(Closure))]),
	
	This = closure:this_loc(Closure),
	lists:foldl(fun(Instruction, Acc)->
			analyze_instruction(Instruction, This, Acc)
		end,
		{dict:new(), sched:new(), Heap},
		Body).
	
analyze_instruction(#move{}, This, {Regs, Sched, Heap}) ->
	tracer:display_string(3, "analyzing move", []),
	?i,?p("move"),
	{Regs, Sched, Heap};
analyze_instruction(#order{line_no=LN}=I, This, {Regs, Sched, Heap}) ->
	debug:set_context(line_no, LN),
	Lhs = slot_or_register(I#order.lhs, This, Regs, Heap),
	Rhs = slot_or_register(I#order.rhs, This, Regs, Heap),
	node:assert_node_id(Lhs),
	node:assert_node_id(Rhs),
	Sched2 = sched:new_edge(Lhs, Rhs, Sched),
	{Regs, Sched2, Heap};
analyze_instruction(#intrinsic{}, This, {Regs, Sched, Heap}) ->
	?p("intrinsic"),
	{Regs, Sched, Heap};
analyze_instruction(#schedule{}, This, {Regs, Sched, Heap}) ->
	?p("schedule"),
	{Regs, Sched, Heap}.
	
% helpers; those functions essentially follow the grammar
reg(Reg, Regs) ->
	case dict:find(Reg, Regs) of
		{ok, Value} ->
			Value;
		error ->
			debug:fatal("Register ~w not set in line ~w, ~s", [Reg, debug:get_context(line_no), debug:get_context(block_info)]),
			error
	end.
		
slot_value(#num{}=V, _) ->
	V;
slot_value(#sym{}=V, _) ->
	V;
slot_value(#reg{}=V, Regs) ->
	reg(V, Regs).
	
struct_value(#reg{}=V, _This, Regs) ->
	reg(V, Regs);
struct_value(#this{}, This, _Regs) ->
	This.
	

slot_or_register(#slot{context=C, slot=S}, This, Regs, Heap) ->
	StructLoc = struct_value(C, This, Regs),
	SlotName = slot_value(S, Regs),
	Struct = heap:get(StructLoc, Heap),
	struct:get(SlotName, Struct);
slot_or_register(#reg{}=V, _This, Regs, _Heap) ->
	reg(V, Regs).