-module (refs).

-include("include/values.hrl").

-export ([root_activation_ref/0, exit_activation_ref/0, activation_ref/2]).
-export ([activation_option/2, block_ref/2]).
-export ([struct_loc/2, array_loc/2, lock_loc/2]).
-export ([activation_option_block_ref/1, activation_option_this_loc/1]).
-export ([branch_out_node/0]).

% @type activation_option(BlockRef, ThisLoc)
%	BlockRef = block_ref()
%	ThisLoc = loc().
-record (activation_option, {block_ref, this_loc}).
	
root_activation_ref() ->
	#activation_ref{path_components=[]}.
	
%the global exit node is the branch out from our initial branch
exit_activation_ref() ->
	#activation_ref{path_components=[branch_out_node()]}.
	
%Component is either a values:creation_statement() or a branch_out_path() or an activation_option()
activation_ref(Component, #activation_ref{path_components=PCs}=Ref) ->
	ok = case Component of
		#activation_option{} -> ok;
		#new_struct{} -> ok;
		#new_array{} -> ok;
		#new_lock{} -> ok;
		{branch_out_node} -> ok
	end,
	Ref#activation_ref{path_components=[Component|PCs]}.
	
activation_option(BlockRef, ThisLoc) ->
	#activation_option{block_ref=BlockRef, this_loc=ThisLoc}.
	
activation_option_block_ref(#activation_option{block_ref=Ref}) ->
	Ref.
	
activation_option_this_loc(#activation_option{this_loc=ThisLoc}) ->
	ThisLoc.
	
block_ref(Nth, Name) ->
	#block_ref{nth=Nth, name=Name}.
	
struct_loc(Nth, ActivationRef) ->
	#loc{creation_statement=#new_struct{nth=Nth}, activation_ref=ActivationRef}.
	
array_loc(Nth, ActivationRef) ->
	#loc{creation_statement=#new_array{nth=Nth}, activation_ref=ActivationRef}.
	
lock_loc(Nth, ActivationRef) ->
	#loc{creation_statement=#new_lock{nth=Nth}, activation_ref=ActivationRef}.
	
% this is used in activation_refs as a path component naming the out-node of a branch node
branch_out_node() ->
	{branch_out_node}.