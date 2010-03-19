-module (closure).

-include("include/values.hrl").

-export ([new/2, block_ref/1, this_loc/1, extract_blocks/1]).

-record (closure, {block_ref, this_loc}).

new(#block_ref{}=BlockRef, ThisLoc) ->
	#closure{block_ref=BlockRef, this_loc=ThisLoc}.

block_ref(Closure) ->
	Closure#closure.block_ref.
	
this_loc(Closure) ->
	Closure#closure.this_loc.

% -> set()
extract_blocks(Closures) ->
	lists:foldl(fun(Closure, Set)-> sets:add_element(Closure#closure.block_ref, Set) end, sets:new(), Closures).
	