-module (closure).

-include("include/values.hrl").

-export ([new/2, block_ref/1, struct_loc/1, extract_blocks/1, extract_structs/1]).

-record (closure, {block_ref, struct_loc}).

new(#block_ref{}=BlockRef, ThisLoc) ->
	#closure{block_ref=BlockRef, struct_loc=ThisLoc}.

block_ref(Closure) ->
	Closure#closure.block_ref.
	
struct_loc(Closure) ->
	Closure#closure.struct_loc.

% set() -> set(block_ref())
extract_blocks(Closures) ->
	sets:fold(
		fun(Closure, Set)-> 
			sets:add_element(Closure#closure.block_ref, Set) 
		end, 
		sets:new(), 
		Closures).

% set -> set(struct_loc())
extract_structs(Closures) ->
	sets:fold(
		fun(Closure, Set)-> 
			sets:add_element(Closure#closure.struct_loc, Set) 
		end, 
		sets:new(), 
		Closures).
	