-module (loader).

-include("include/values.hrl").

-export ([new/0, load_from_string/1, load_from_file/1]).
-export ([get_block/2, contains_block/2, add_block/2, add_all_blocks/2]).

-define (BLOCK_ID_POS, 2).

% @type code_repository().
% For now, the code_repository is a list of blocks.

new() ->
	[].

%internal helper
parse(Filename, String) ->
	case parser:parse_from_string(Filename, String) of
		{ok, Repository} ->	
			Repository;
		{error, Reason} ->
			events:fatal("Parser error in program parsed from ~80p. Reason: ~80p", [Filename, Reason]),
			{error, Reason}
	end.
				
% @spec load_from_string(string()) -> code_repository()
load_from_string(String) ->
	parse("<parsed from string>", String).

% @spec load_from_file(string()) -> code_repository()
load_from_file(Filename) ->
	case file:read_file(Filename) of
		{ok, Binary} ->
    		parse(Filename, erlang:binary_to_list(Binary));
		{error, Reason} -> 
			events:fatal("Cannot load file ~s. Reason: ~p", [Filename, Reason]),
			{error, Reason}
	end.
	
% @spec get_block(atom(), code_repository()) -> values:block() | false
get_block(BlockID, Repository) ->
	case lists:keysearch(BlockID, ?BLOCK_ID_POS, Repository) of
		{value, Block} -> Block;
		false -> false
	end.
	
% @spec contains_block(atom(), code_repository()) -> true | false
contains_block(BlockID, Repository) ->
	case lists:keysearch(BlockID, ?BLOCK_ID_POS, Repository) of
		false -> false;
		_Else -> true
	end.
	
% returns false and logs a fatal error if block does already exist
% @spec add_block(atom(), code_repository()) -> code_repository() | false
add_block(#block{name=Name}=B, Repository) ->
	case contains_block(Name, Repository) of
		false -> [B|Repository];
		true -> 
			events:fatal("A block with name '~w' already exists", [Name]),
			false
	end.
	
add_all_blocks([], Repository) ->
	Repository;
add_all_blocks([B|Rest], Repository) ->
	add_all_blocks(Rest, add_block(B, Repository)).