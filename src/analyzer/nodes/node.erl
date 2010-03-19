-module (node).

-include("include/debug.hrl").

-export ([analyze/5]).
-export ([root_node_id/0, exit_node_id/0, split_node_id/2, union_node_id/1, atom_node_id/2]).
-export ([assert_node_id/1]).

-record (split_node_id, {path}).
-record (union_node_id, {path}).
-record (atom_node_id, {path}).

% faking some virtual method call and inheritance here
analyze(#split_node_id{}=Loc, Parents, Heap, Sched, Loader) ->
	split_node:analyze(Loc, Parents, Heap, Sched, Loader);
analyze(#union_node_id{}=Loc, Parents, Heap, Sched, Loader) ->
	union_node:analyze(Loc, Parents, Heap, Sched, Loader);
analyze(#atom_node_id{}=Loc, Parents, Heap, Sched, Loader) ->
	atom_node:analyze(Loc, Parents, Heap, Sched, Loader).
		
root_node_id() ->
	#split_node_id{path=[]}.
	
exit_node_id() ->
	#union_node_id{path=[union_node]}.
	
split_node_id(Int, ParentActLoc) when is_integer(Int) ->
	#split_node_id{path=[Int|ParentActLoc#atom_node_id.path]}.
	
union_node_id(ParentActLoc) ->
	#union_node_id{path=[union_node|ParentActLoc#split_node_id.path]}.

atom_node_id(Closure, ParentActLoc) ->
	#atom_node_id{path=[Closure|ParentActLoc#split_node_id.path]}.
	
assert_node_id(#split_node_id{})->
	ok;
assert_node_id(#union_node_id{}) ->
	ok;
assert_node_id(#atom_node_id{}) ->
	ok.