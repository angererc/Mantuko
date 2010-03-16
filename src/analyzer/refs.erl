-module (refs).

-include("include/values.hrl").

-export ([root_activation_ref/0, activation_ref_add_component/2]).
-export ([activation_option/2, block_ref/2, loc/2]).

% @type activation_option(BlockRef, ThisLoc)
%	BlockRef = block_ref()
%	ThisLoc = loc().
-record (activation_option, {block_ref, this_loc}).
	
root_activation_ref() ->
	#activation_ref{path_components=[]}.
	
activation_ref_add_component(Component, #activation_ref{path_components=PCs}=Ref) ->
	Ref#activation_ref{path_components=[Component|PCs]}.
	
activation_option(BlockRef, ThisLoc) ->
	#activation_option{block_ref=BlockRef, this_loc=ThisLoc}.
	
activation_option_block_ref(#activation_option{block_ref=Ref}) ->
	Ref.
	
activation_option_this_loc(#activation_option{this_loc=ThisLoc}) ->
	ThisLoc.
	
block_ref(Nth, Name) ->
	#block_ref{nth=Nth, name=Name}.
	
loc(Nth, ActivationRef) ->
	#loc{nth=Nth, activation_ref=ActivationRef}.