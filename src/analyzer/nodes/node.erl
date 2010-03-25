-module (node).

-include("include/debug.hrl").

-export ([analyze/5]).
-export ([root_node_id/0, exit_node_id/0, split_node_id/2, union_node_id/1, atom_node_id/2]).
-export ([parent_split_node_id/1]).
-export ([node_as_edge_source/1]).
-export ([get_block_refs/2, get_struct_locs/2]).
-export ([assert_node_id/1]).
-export ([merge/2]).
-export ([get_common_ancestor/2]).

-record (split_node_id, {path}).
-record (union_node_id, {path}).
-record (atom_node_id, {path}).

%helper
find_parent(Same, Same) ->
	Same;
find_parent([_Head1|Tail1], [_Head2|Tail2]) ->
	find_parent(Tail1, Tail2).
	
%walt up the creation tree and find the common ancestor
% (NodeID1, NodeID2) -> NodeID3
get_common_ancestor({_, Path1}, {_, Path2}) ->
	Len1 = length(Path1),
	Len2 = length(Path2),
	%cut the longer path to the size of the shorter one
	find_parent(
		lists:nthtail(erlang:max(0, Len1-Len2), Path1), 
		lists:nthtail(erlang:max(0, Len2-Len1), Path2)).
	
% faking some virtual method call and inheritance here
% -> {[NewNodes], [LoopMarkers], sched()}
analyze(#split_node_id{}=Loc, ParentSplitNodes, Heap, Sched, Loader) ->
	split_node:analyze(Loc, ParentSplitNodes, Heap, Sched, Loader);
analyze(#union_node_id{}=Loc, ParentSplitNodes, Heap, Sched, Loader) ->
	union_node:analyze(Loc, ParentSplitNodes, Heap, Sched, Loader);
analyze(#atom_node_id{}=Loc, _ParentSplitNodes, Heap, Sched, Loader) ->
	atom_node:analyze(Loc, Heap, Sched, Loader).
	
% -> set() | Value
get_block_refs(#split_node_id{}=Loc, Sched) ->
	split_node:get_block_refs(Loc, Sched);
get_block_refs(#union_node_id{}=Loc, Sched) ->
	split_node:get_block_refs(parent_split_node_id(Loc), Sched);
get_block_refs(#atom_node_id{}=Loc, Sched) ->
	atom_node:get_block(Loc, Sched), sets:new().
		
% -> set() | Value
get_struct_locs(#split_node_id{}=Loc, Sched) ->
	split_node:get_struct_locs(Loc, Sched);
get_struct_locs(#union_node_id{}=Loc, Sched) ->
	split_node:get_struct_locs(parent_split_node_id(Loc), Sched);
get_struct_locs(#atom_node_id{}=Loc, Sched) ->
	atom_node:get_struct_loc(Loc, Sched), sets:new().

% for a split node it returns the union node ID
node_as_edge_source(#split_node_id{}=ID) ->
	union_node_id(ID);
node_as_edge_source(#union_node_id{}=ID) ->
	?f("union nodes shouldn't be out there as references! ~s (i think)", [pretty:string(ID)]),
	error;
node_as_edge_source(Else) ->
	Else.
		
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
	
parent_split_node_id(#union_node_id{path=[union_node_id|Rest]}) ->
	#split_node_id{path=Rest};
parent_split_node_id(#atom_node_id{path=[_Closure|Rest]}) ->
	#split_node_id{path=Rest}.
	
assert_node_id(#split_node_id{})->
	ok;
assert_node_id(#union_node_id{}) ->
	ok;
assert_node_id(#atom_node_id{}) ->
	ok.
	
%dispatch merge to the concrete node implementations; merge is used when we add two schedules together
merge(N1, N2) ->
	%assert that the types are the same; this should always be the case but for reasons of sanity I do it here
	NodeType = element(1, N1),
	NodeType = element(1, N2),
	NodeType:merge(N1, N2).