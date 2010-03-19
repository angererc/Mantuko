-module (atom_node).

-include("include/debug.hrl").

-export ([new/1, analyze/5]).

-record (atom_node, {closure}).

new(Closure) ->
	#atom_node{closure=Closure}.
	
analyze(_NodeID, _Parents, _Heap, _Sched, _Loader) ->
	%_Block = loader:get_block(refs:closure_block_ref(Option), Loader),
	[].