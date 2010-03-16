-module (option_node).

-include("include/heap.hrl").

-export ([new/1, analyze/5]).

new(ActivationOption) ->
	#option_node{activation_option=ActivationOption}.
	
analyze(_ActivationRef, #option_node{activation_option=Option}, _Parents, _Heap, Loader) ->
	_Block = loader:get_block(refs:activation_option_block_ref(Option), Loader),
	ok.