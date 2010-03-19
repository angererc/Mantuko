-module (atom_node).

-include("include/debug.hrl").
-include("include/loader.hrl").
-include("include/instructions.hrl").
-include("include/values.hrl").

-export ([new/1, analyze/5]).
-export ([get_block_ref/2, get_struct_loc/2]).

-record (atom_node, {closure}).

new(Closure) ->
	#atom_node{closure=Closure}.

get_block_ref(MyNodeID, Sched) ->
	#atom_node{closure=Closure} = sched:get_node(MyNodeID, Sched),
	closure:block_ref(Closure).

get_struct_loc(MyNodeID, Sched) ->
	#atom_node{closure=Closure} = sched:get_node(MyNodeID, Sched),
	closure:struct_loc(Closure).
		
analyze(MyNodeID, _Parents, Heap, Sched, Loader) ->
	#atom_node{closure=Closure} = sched:get_node(MyNodeID, Sched),
	
	#block{name=N, filename=FN, start_line=S, end_line=E, body=Body} = loader:get_block(closure:block_ref(Closure), Loader),
	debug:set_context(block_info, lists:flatten(io_lib:format("block ~w in file ~s:~w-~w", [N, FN, S, E]))),
	?f("analyzing block ~w", [element(3,closure:block_ref(Closure))]),
	
	This = closure:struct_loc(Closure),
	lists:foldl(fun(Instruction, Acc)->
			analyze_instruction(Instruction, MyNodeID, This, Acc)
		end,
		{dict:new(), sched:new(), Heap},
		Body).
	
analyze_instruction(#move{line_no=LN}=I, Now, This, {Regs, Sched, _Heap}=RSH) ->
	debug:set_context(line_no, LN),
	?f("Instruction ~w", [I]),
	{Value, Heap2} = value(I#move.value, Now, This, RSH),
	{Regs2, Heap3} = store(I#move.target, Value, This, Regs, Heap2),
	{Regs2, Sched, Heap3};
analyze_instruction(#order{line_no=LN}=I, _Now, This, {Regs, Sched, Heap}) ->
	debug:set_context(line_no, LN),
	?f("Instruction ~w", [I]),
	Lhs = slot_or_register(I#order.lhs, This, Regs, Heap),
	Rhs = slot_or_register(I#order.rhs, This, Regs, Heap),
	node:assert_node_id(Lhs),
	node:assert_node_id(Rhs),
	Sched2 = sched:new_edge(Lhs, Rhs, Sched),
	{Regs, Sched2, Heap};
analyze_instruction(#intrinsic{line_no=LN}=I, Now, This, {Regs, Sched, Heap}) ->
	debug:set_context(line_no, LN),
	?f("Instruction ~w", [I]),
	
	{InValues, Heap2} = lists:mapfoldl(fun(Val, HeapAcc)->
			value(Val, Now, This, {Regs, Sched, HeapAcc})
		end,
		Heap,
		I#intrinsic.in_values
	),
	
	Name = I#intrinsic.name,
	{Regs2, Heap3} = case erlang:function_exported(intrinsics, Name, length(InValues)+1) of
		true ->
			ResultValues = apply(intrinsics, Name, [Sched|InValues]),
			store_values(I#intrinsic.out_lhsides, ResultValues, This, {Regs, Heap2});
		false ->
			debug:fatal("Invalid intrinsic ~w with parameters [*sched*|~w] in line ~w, ~s", [Name, InValues, debug:get_context(line_no), debug:get_context(block_info)]),
			error
	end,
	{Regs2, Sched, Heap3};
analyze_instruction(#schedule{line_no=LN}=I, Now, This, {Regs, Sched, Heap}) ->
	debug:set_context(line_no, LN),
	?f("Instruction ~w", [I]),
	BlockRef = activation_lhs(I#schedule.block, This, Regs, Heap),
	{StructLoc, Heap2} = activation_rhs(I#schedule.struct, Now, This, Regs, Heap),
	
	NewNodeLoc = node:split_node_id(I#schedule.nth, Now),
	NewNode = split_node:add_closure(closure:new(BlockRef, StructLoc), split_node:new()),
	Sched2 = sched:set_node(NewNodeLoc, NewNode, Sched),
	Sched3 = split_node:create_union_node(NewNodeLoc, Sched2),
	
	{Regs2, Heap3} = store(I#schedule.target, NewNodeLoc, This, Regs, Heap2),
	{Regs2, Sched3, Heap3}.
	
% helpers; those functions essentially follow the grammar
store_values([], [], _This, {Regs, Heap}) ->
	{Regs, Heap};
store_values([], _Some, _This, _RH) ->
	debug:fatal("Trying to store more values than available return places in line ~w, ~s", [debug:get_context(line_no), debug:get_context(block_info)]);
store_values(_Some, [], _This, _RH) ->
	debug:fatal("Trying to store less values than available return places in line ~w, ~s", [debug:get_context(line_no), debug:get_context(block_info)]);	
store_values([Place|MorePlaces], [Value|MoreValues], This, {Regs, Heaps}) ->
	store_values(MorePlaces, MoreValues, This, store(Place, Value, This, Regs, Heaps)).
	
store(undefined, _, _, Regs, Heap) ->
	{Regs, Heap};
store(#reg{}=R, Value, _This, Regs, Heap) ->
	{dict:store(R, Value, Regs), Heap};
store(#slot{context=C, slot=S}, Value, This, Regs, Heap) ->
	StructLoc = struct_loc(C, This, Regs),
	SlotName = slot_name(S, Regs),
	Struct = heap:get(StructLoc, Heap),
	Struct2 = struct:set(SlotName, Value, Struct),
	Heap2 = heap:set(StructLoc, Struct2, Heap),
	{Regs, Heap2}.
	
reg(Reg, Regs) ->
	case dict:find(Reg, Regs) of
		{ok, Value} ->
			Value;
		error ->
			debug:fatal("Register ~w not set in line ~w, ~s", [Reg, debug:get_context(line_no), debug:get_context(block_info)]),
			error
	end.
	
potential_set(Type, X) ->
	case sets:is_set(X) of
		true ->
			#one_of{type=Type, value_set=X};
		false ->
			X
	end.
	
activation_lhs(#block_ref{}=V, _This, _Regs, _Heap) ->
	V;
activation_lhs(SlotOrReg, This, Regs, Heap) ->
	slot_or_register(SlotOrReg, This, Regs, Heap).
	
activation_rhs(#this{}, _Now, This, _Regs, Heap) ->
	{This, Heap};
activation_rhs(#new_struct{nth=Nth}, Now, _This, _Regs, Heap) ->
	NewLoc = heap:struct_loc(Nth, Now),
	Heap2 = heap:new_struct(NewLoc, Heap),
	{NewLoc, Heap2};
activation_rhs(SlotOrReg, _Now, This, Regs, Heap) ->
	{slot_or_register(SlotOrReg, This, Regs, Heap), Heap}.
	
% -> {Value, Heap2}
value(#sym{}=V, _Now, _This, {_Regs, _Sched, Heap}) ->
	{V, Heap};
value(#num{}=V, _Now, _This, {_Regs, _Sched, Heap}) ->
	{V, Heap};
value(#block_ref{}=V, _Now, _This, {_Regs, _Sched, Heap}) ->
	{V, Heap};
value(#this{}, _Now, This, {_Regs, _Sched, Heap}) ->
	{This, Heap};
value(#new_struct{nth=Nth}, Now, _This, {_Regs, _Sched, Heap}) ->
	NewLoc = heap:struct_loc(Nth, Now),
	Heap2 = heap:new_struct(NewLoc, Heap),
	{NewLoc, Heap2};
value(#new_array{nth=Nth}, Now, _This, {_Regs, _Sched, Heap}) ->
	NewLoc = heap:array_loc(Nth, Now),
	Heap2 = heap:new_array(NewLoc, Heap),
	{NewLoc, Heap2};
value(#new_lock{nth=Nth}, Now, _This, {_Regs, _Sched, Heap}) ->
	NewLoc = heap:lock_loc(Nth, Now),
	Heap2 = heap:new_lock(NewLoc, Heap),
	{NewLoc, Heap2};
value(#nil{}=V, _Now, _This, {_Regs, _Sched, Heap}) ->
	{V, Heap};
value(#now{}, Now, _This, {_Regs, _Sched, Heap}) ->
	{Now, Heap};
value(#act_block{activation=A}, Now, This, {Regs, Sched, Heap}) ->
	ActId = activation_value(A, Now, This, Regs, Heap),
	Value = potential_set(block_ref, node:get_block_refs(ActId, Sched)),
	{Value, Heap};
value(#act_struct{activation=A}, Now, This, {Regs, Sched, Heap}) ->
	ActID = activation_value(A, Now, This, Regs, Heap),
	Value = potential_set(struct, node:get_struct_locs(ActID, Sched)),
	{Value, Heap};
value(SlotOrReg, _Now, This, {Regs, _Sched, Heap}) ->
	{slot_or_register(SlotOrReg, This, Regs, Heap), Heap}.
	
slot_name(#num{}=V, _) ->
	V;
slot_name(#sym{}=V, _) ->
	V;
slot_name(#reg{}=V, Regs) ->
	reg(V, Regs).

struct_loc(#reg{}=V, _This, Regs) ->
	reg(V, Regs);
struct_loc(#this{}, This, _Regs) ->
	This.
	
activation_value(#now{}, Now, _This, _Regs, _Heap) ->
	Now;
activation_value(SlotOrReg, _Now, This, Regs, Heap) ->
	slot_or_register(SlotOrReg, This, Regs, Heap).
	
slot_or_register(#slot{context=C, slot=S}, This, Regs, Heap) ->
	StructLoc = struct_loc(C, This, Regs),
	SlotName = slot_name(S, Regs),
	Struct = heap:get(StructLoc, Heap),
	struct:get(SlotName, Struct);
slot_or_register(#reg{}=V, _This, Regs, _Heap) ->
	reg(V, Regs).