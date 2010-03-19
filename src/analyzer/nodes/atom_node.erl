-module (atom_node).

-include("include/debug.hrl").
-include("include/heap.hrl").
-include("include/nodes.hrl").

-export ([new/1, analyze/6]).

new(ActivationOption) ->
	#atom_node{activation_option=ActivationOption}.
	
analyze(_ActivationRef, #atom_node{activation_option=Option}, _Parents, _Heap, _Sched, Loader) ->
	_Block = loader:get_block(refs:activation_option_block_ref(Option), Loader),
	[].