-module (option_node).

-include("include/heap.hrl").

-export ([analyze/5]).

analyze(_ActivationRef, #option_node{activation_option=Option}, _Parents, _Heap, Loader) ->
	_Block = loader:get_block(refs:activation_option_block_ref(Option), Loader),
	ok.