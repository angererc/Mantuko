-module (atom_node).

-include("include/debug.hrl").
-include("include/loader.hrl").
-include("include/instructions.hrl").
-include("include/values.hrl").

-export ([new/1, merge/2, analyze/4]).
-export ([get_block_ref/2, get_struct_loc/2]).

-record (atom_node, {closure}).

new(Closure) ->
	#atom_node{closure=Closure}.

merge(#atom_node{} = SameNode, SameNode) ->
	SameNode;
merge(Else, Other) ->
	debug:fatal("tried to merge two atom nodes with different closures; this should never happen! ~w and ~w", [Else, Other]).
	
get_block_ref(MyNodeID, Sched) ->
	#atom_node{closure=Closure} = sched:get_node_info(MyNodeID, Sched),
	closure:block_ref(Closure).

get_struct_loc(MyNodeID, Sched) ->
	#atom_node{closure=Closure} = sched:get_node_info(MyNodeID, Sched),
	closure:struct_loc(Closure).
		
analyze(MyNodeID, Heap, Sched, Loader) ->
	#atom_node{closure=Closure} = sched:get_node_info(MyNodeID, Sched),	
	#block{name=N, filename=FN, start_line=S, end_line=E, body=Body} = loader:get_block(closure:block_ref(Closure), Loader),

	debug:set_context(block_info, lists:flatten(io_lib:format("block ~w in file ~s:~w-~w", [N, FN, S, E]))),
	?f("analyzing node ~s, block ~w", [pretty:string(MyNodeID), element(3,closure:block_ref(Closure))]),
	
	This = closure:struct_loc(Closure),
	{_Regs, NewNodes, Sched2, Heap2} = lists:foldl(fun(Instruction, Acc)->
			analyze_instruction(Instruction, MyNodeID, This, Acc)
		end,
		{dict:new(), [], Sched, Heap},
		Body),
		
	%store the result heap
	debug:clear_context(block_info),
	Sched3 = sched:set_result(MyNodeID, Heap2, Sched2),
	{NewNodes, [], Sched3}.
	
analyze_instruction(#move{line_no=LN}=I, Now, This, {Regs, NewNodes, Sched, Heap}) ->
	debug:set_context(line_no, LN),
	{Value, Heap2} = value(I#move.value, Now, This, {Regs, Sched, Heap}),
	?f("Instruction ~w; value to store is ~w", [I, Value]),
	{Regs2, Heap3} = store(I#move.target, Value, This, Regs, Heap2),
	debug:clear_context(line_no),
	{Regs2, NewNodes, Sched, Heap3};
analyze_instruction(#order{line_no=LN}=I, _Now, This, {Regs, NewNodes, Sched, Heap}) ->
	debug:set_context(line_no, LN),
	?f("Instruction ~w", [I]),
	Lhs = slot_or_register(I#order.lhs, This, Regs, Heap),
	Rhs = slot_or_register(I#order.rhs, This, Regs, Heap),
	node:assert_node_id(Lhs),
	node:assert_node_id(Rhs),
	Sched2 = sched:new_edge(Lhs, Rhs, Sched),
	debug:clear_context(line_no),
	{Regs, NewNodes, Sched2, Heap};
analyze_instruction(#intrinsic{line_no=LN, nth=Nth}=I, Now, This, {Regs, NewNodes, Sched, Heap}) ->
	debug:set_context(line_no, LN),
	?f("Instruction ~w; regs are: ~w", [I, Regs]),
	
	{InValues, Heap2} = lists:mapfoldl(fun(Val, HeapAcc)->
			value(Val, Now, This, {Regs, Sched, HeapAcc})
		end,
		Heap,
		I#intrinsic.in_values
	),
	
	Name = I#intrinsic.name,
	Result = case erlang:function_exported(intrinsics, Name, length(InValues)+4) of
		true ->
			{ResNewNodes, ResSched, ResHeap, ResValues} = apply(intrinsics, Name, [Sched, Heap2, Nth, Now | InValues]),
			{Regs2, ResHeap2} = store_values(I#intrinsic.out_lhsides, ResValues, This, {Regs, ResHeap}),
			{Regs2, ResNewNodes++NewNodes, ResSched, ResHeap2};
		false ->
			debug:fatal("Invalid intrinsic ~w with parameters ~w in line ~w, ~s", [Name, InValues, debug:get_context(line_no), debug:get_context(block_info)]),
			error
	end,
	debug:clear_context(line_no),
	Result;
analyze_instruction(#schedule{line_no=LN}=I, Now, This, {Regs, NewNodes, Sched, Heap}) ->
	debug:set_context(line_no, LN),
	?f("Instruction ~w", [I]),
	BlockRef = activation_lhs(I#schedule.block, This, Regs, Heap),
	{StructLoc, Heap2} = activation_rhs(I#schedule.struct, Now, This, Regs, Heap),
	
	NewNodeID = node:split_node_id(I#schedule.nth, Now),
	NewNode = split_node:add_closure(closure:new(BlockRef, StructLoc), split_node:new()),
	
	Sched2 = sched:set_node_info(NewNodeID, NewNode, Sched),
	Sched3 = sched:new_edge(Now, NewNodeID, Sched2),
	{UnionNodeID, Sched4} = split_node:create_union_node(NewNodeID, Sched3),
	
	{Regs2, Heap3} = store(I#schedule.target, NewNodeID, This, Regs, Heap2),
	debug:clear_context(line_no),
	{Regs2, [NewNodeID, UnionNodeID|NewNodes], Sched4, Heap3}.

%****************************************	
% Store helpers
%****************************************

store_values([], [], _This, {Regs, Heap}) ->
	{Regs, Heap};
store_values([], _Some, _This, _RSH) ->
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
	ObjectLoc = object_loc(C, This, Regs),
	SlotName = slot_name(S, Regs),
	Object = heap:get(ObjectLoc, Heap),
	Object2 = object:set(SlotName, Value, Object),
	Heap2 = heap:set(ObjectLoc, Object2, Heap),
	{Regs, Heap2}.
	
%****************************************
% read helpers
%****************************************
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

%****************************************
% interpreting the AST; this part follows 
% the grammar pretty closely
%****************************************	
activation_lhs(#block_ref{}=V, _This, _Regs, _Heap) ->
	V;
activation_lhs(SlotOrReg, This, Regs, Heap) ->
	slot_or_register(SlotOrReg, This, Regs, Heap).
	
activation_rhs(#this{}, _Now, This, _Regs, Heap) ->
	{This, Heap};
activation_rhs(#new_struct{nth=Nth}, Now, _This, _Regs, Heap) ->
	NewLoc = object:struct_loc(Nth, Now),
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
	NewLoc = object:struct_loc(Nth, Now),
	Heap2 = heap:new_struct(NewLoc, Heap),
	{NewLoc, Heap2};
value(#new_array{nth=Nth}, Now, _This, {_Regs, _Sched, Heap}) ->
	NewLoc = object:array_loc(Nth, Now),
	Heap2 = heap:new_array(NewLoc, Heap),
	{NewLoc, Heap2};
value(#new_lock{nth=Nth}, Now, _This, {_Regs, _Sched, Heap}) ->
	NewLoc = object:lock_loc(Nth, Now),
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

object_loc(#reg{}=V, _This, Regs) ->
	reg(V, Regs);
object_loc(#this{}, This, _Regs) ->
	This.
	
activation_value(#now{}, Now, _This, _Regs, _Heap) ->
	Now;
activation_value(SlotOrReg, _Now, This, Regs, Heap) ->
	slot_or_register(SlotOrReg, This, Regs, Heap).
	
slot_or_register(#slot{context=C, slot=S}, This, Regs, Heap) ->
	ObjectLoc = object_loc(C, This, Regs),
	SlotName = slot_name(S, Regs),
	Object = heap:get(ObjectLoc, Heap),
	object:get(SlotName, Object);
slot_or_register(#reg{}=V, _This, Regs, _Heap) ->
	reg(V, Regs).