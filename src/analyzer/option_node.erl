-module (option_node).

-include("include/nodes.hrl").

-export ([analyze/3]).

analyze(#option_node{activation_option=Option}, _InputHeap, Loader) ->
	Block = loader:get_block(refs:activation_option_block_ref(Option), Loader),
	ok.