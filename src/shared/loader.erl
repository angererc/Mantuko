-module (loader).

-include("include/values.hrl").

-export ([new/0, load_from_string/1, load_from_file/1]).
-export ([get_block/2, contains_block/2, add_block/2, add_all_blocks/2]).

-define (BLOCK_ID_POS, 2).

% @type loader().
% For now, the loader is a list of blocks.

new() ->
	[].

%internal helper
parse(Filename, String) ->
	case parser:parse_from_string(Filename, String) of
		{ok, Loader} ->	
			Loader;
		{error, Reason} ->
			events:fatal("Parser error in program parsed from ~80p. Reason: ~80p", [Filename, Reason]),
			{error, Reason}
	end.
				
% @spec load_from_string(string()) -> loader()
load_from_string(String) ->
	parse("<parsed from string>", String).

% @spec load_from_file(string()) -> loader()
load_from_file(Filename) ->
	case file:read_file(Filename) of
		{ok, Binary} ->
    		parse(Filename, erlang:binary_to_list(Binary));
		{error, Reason} -> 
			events:fatal("Cannot load file ~s. Reason: ~p", [Filename, Reason]),
			{error, Reason}
	end.
	
% @spec get_block(atom(), loader()) -> values:block() | false
get_block(BlockID, Loader) ->
	case lists:keysearch(BlockID, ?BLOCK_ID_POS, Loader) of
		{value, Block} -> Block;
		false -> false
	end.
	
% @spec contains_block(atom(), loader()) -> true | false
contains_block(BlockID, Loader) ->
	case lists:keysearch(BlockID, ?BLOCK_ID_POS, Loader) of
		false -> false;
		_Else -> true
	end.
	
% returns false and logs a fatal error if block does already exist
% @spec add_block(atom(), loader()) -> loader() | false
add_block(#block{name=Name}=B, Loader) ->
	case contains_block(Name, Loader) of
		false -> [B|Loader];
		true -> 
			events:fatal("A block with name '~w' already exists", [Name]),
			false
	end.
	
add_all_blocks([], Loader) ->
	Loader;
add_all_blocks([B|Rest], Loader) ->
	add_all_blocks(Rest, add_block(B, Loader)).