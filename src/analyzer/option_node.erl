-module (option_node).

-include("include/nodes.hrl").

-export ([analyze/3]).

analyze(#option_node{block=BlockID, this=This}, _InputHeap, Loader) ->
	Block = loader:get_block(BlockID, Loader),
	ok.