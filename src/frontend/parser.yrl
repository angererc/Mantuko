Nonterminals 
file
opt_named_block_list named_block
body
literal_block block_ref
instruction value 
register symbol number 
slot slot_or_reg
slot_name struct_loc
value_list opt_value_list
slot_or_register_list
activation_lhs activation_rhs
activation_value.

Terminals
comma equals
lparen rparen lcurl rcurl lsquare rsquare
reg_ident sym_ident
ident num
this nil now
new struct array lock
sched
act_block act_struct
colon
arrow.

Rootsymbol file.

%we use {main} as the id for the main block so that the atom main can be used as a macro name
% the first element in the returned block array will be the main block
file -> opt_named_block_list literal_block opt_named_block_list	: 	
			%can't call save_block here because that returns undefined
			loader:add_block('$2'#block{name={main}}, get(repository)).

opt_named_block_list -> '$empty'		: undefined.
opt_named_block_list -> named_block opt_named_block_list : undefined.

named_block -> ident colon literal_block : 
			save_block('$3'#block{name=list_to_atom(unwrap('$1'))}). 

literal_block -> lcurl body rcurl	: 
			{_,Start} = '$1',
			{_, End} = '$3',
			%reset counters for next block
			reset(nth),
			#block{filename=?FILENAME, start_line=Start, end_line=End, body='$2'}.
literal_block -> lcurl rcurl	: 
			{_,Start} = '$1',
			{_, End} = '$2',
			%reset counters for next block; probably not needed here but hey...
			reset(nth),
			#block{filename=?FILENAME, start_line=Start, end_line=End, body=[]}.
			
block_ref -> literal_block :
	Name = {anonymous, inc(anonymous_blocks)},
	save_block('$1'#block{name=Name}),
	#block_ref{nth=inc(nth), name=Name}.	
block_ref -> colon ident : 
	#block_ref{nth=inc(nth), name=list_to_atom(unwrap('$2'))}.

body -> instruction					: ['$1'].
body -> instruction body	: case '$1' of
									none -> '$2';
									Else -> [Else|'$2']
								  end.

%%
%% INSTRUCTIONS
%%
instruction ->  register equals value	
					: #move{line_no=line_no('$2'), target='$1', value='$3'}.
instruction -> slot equals value	
					: #move{line_no=line_no('$2'), target='$1', value='$3'}.
% NOTE: it doesn't make sense to add a now -> %reg variant
% because: this only succeeds if there is an edge from now to %reg already 
% and then the edge would be redundant
instruction -> slot_or_reg arrow slot_or_reg	
					: #order{line_no=line_no('$2'), lhs='$1', rhs='$3'}.
instruction -> sched activation_lhs lparen activation_rhs rparen
					: #schedule{line_no=line_no('$1'), nth=inc(nth), target=undefined, block='$2', struct='$4'}.
instruction -> register equals sched activation_lhs lparen activation_rhs rparen
					: #schedule{line_no=line_no('$2'), nth=inc(nth), target='$1', block='$4', struct='$6'}.
instruction -> slot equals sched activation_lhs lparen activation_rhs rparen
					: #schedule{line_no=line_no('$2'), nth=inc(nth), target='$1', block='$4', struct='$6'}.

%%
%% PRIMITIVES
%%
%% primitive forms: %foo v1, v2; r1 = %foo v2; r1, r2 = %foo v4
instruction -> ident lparen opt_value_list rparen
					: #intrinsic{line_no=line_no('$1'), name=list_to_atom(unwrap('$1')), out_lhsides=[], in_values='$3'}.
instruction -> register equals ident lparen opt_value_list rparen
					: #intrinsic{line_no=line_no('$2'), name=list_to_atom(unwrap('$3')), out_lhsides=['$1'], in_values='$5'}.
instruction -> slot equals ident lparen opt_value_list rparen
					: #intrinsic{line_no=line_no('$2'), name=list_to_atom(unwrap('$3')), out_lhsides=['$1'], in_values='$5'}.
instruction -> slot_or_reg comma slot_or_register_list equals ident lparen opt_value_list rparen
					: #intrinsic{line_no=line_no('$4'), name=list_to_atom(unwrap('$5')), out_lhsides=['$1'|'$3'], in_values='$7'}.

opt_value_list -> '$empty' : [].
opt_value_list -> value_list : '$1'.

value_list -> value : ['$1'].
value_list -> value comma value_list : ['$1'|'$3'].

slot_or_register_list -> slot_or_reg : ['$1'].
slot_or_register_list -> slot_or_reg comma slot_or_register_list : ['$1'|'$3'].

%%
%% INDIRECT READS/WRITES
%%

%we don't allow a slot to be used as a slot value because that allows recursion and we don't want that
% in the assembly language (e.g., this[%reg[this['foo]]])
slot_name -> register : '$1'.
slot_name -> symbol : '$1'.
slot_name -> number : '$1'.

%same here, we don't want to have this['foo]['bar]['blubb] in the assembly even though it would work easily
struct_loc -> register : '$1'.
struct_loc -> this : #this{}.

%%
%% VALUES
%%
value -> slot : '$1'.
value -> register : '$1'.
value -> symbol : '$1'.
value -> number : '$1'.
value -> block_ref : '$1'.
value -> this : #this{}.
value -> new struct : #new_struct{nth=inc(nth)}.
value -> new array : #new_array{nth=inc(nth)}.
value -> new lock : #new_lock{nth=inc(nth)}.
value -> nil : #nil{}.
value -> now : #now{}.
value -> activation_value act_block : #act_block{activation='$1'}.
value -> activation_value act_struct : #act_struct{activation='$1'}.

activation_value -> now : '$1'.
activation_value -> slot_or_reg : '$1'.

% registers in read-position can also be slots because that doesn't require any special handling by the interpreter
slot_or_reg -> register : '$1'.
slot_or_reg -> slot : '$1'.

slot -> struct_loc lsquare slot_name rsquare : #slot{context='$1', slot='$3'}.
register -> reg_ident : #reg{name=list_to_atom(lists:nthtail(1,unwrap('$1')))}.
symbol -> sym_ident : #sym{nth=inc(nth), name=list_to_atom(lists:nthtail(1, unwrap('$1')))}.
number -> num : #num{nth=inc(nth), value=nummeric_value_from_string_token('$1')}.

%%
%% ACTIVATIONS
%%
activation_lhs -> slot_or_reg : '$1'.
activation_lhs -> block_ref : '$1'.

activation_rhs -> slot_or_reg : '$1'.
activation_rhs -> new struct : #new_struct{nth=inc(nth)}.
activation_rhs -> this : #this{}.

Erlang code.

-include("include/instructions.hrl").
-include("include/values.hrl").
-include("include/loader.hrl").
-export([parse_from_string/2]).

%*********************************
%helpers used in the grammar
%*********************************
unwrap({_,_,V}) -> V.

line_no({_,LineNo,_}) -> LineNo;
line_no({_,LineNo}) -> LineNo.

reset(Sym) ->
	put(Sym, 0).
	
inc(Sym) ->
	put(Sym, get(Sym)+1).
	
save_block(Block) ->
	put(repository, loader:add_block(Block, get(repository))),
	%return undefined because put returns the old repository without the new block
	undefined.

nummeric_value_from_string_token({_, _, V}) ->
	case erl_scan:string(V) of
		{ok, [{integer, _, Value}], _} ->
			Value;
		Some ->
			tracer:fatal(?MODULE, nummeric_value_from_string_token, "Illegal nummeric value: ~p", [Some])
	end.

%*********************************
%extended interface for the parser
%*********************************
-define(FILENAME, get(filename)).

% @spec parse_from_string(Filename::string(), Program::string())-> loader:code_repository() | {error, Reason}
% @end
% returns a list of blocks; the first block has id {main}
parse_from_string(Filename, String) ->
	put(filename, Filename),
	%global variables
	reset(nth),
	reset(anonymous_blocks),
	put(repository, loader:new()),
	{ok,Tokens,_} = lexer:string(String),	
	parse(Tokens).
	