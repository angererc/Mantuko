-module (pretty).

-compile(export_all).

string(Atom) when is_atom(Atom) ->
	atom_to_list(Atom);
string(List) when is_list(List) ->
	list(List);
string(Tuple) when is_tuple(Tuple) ->
	Type = element(1, Tuple),
	case erlang:function_exported(pretty, Type, 1) of
		true ->
			pretty:Type(Tuple);
		false -> 
			lists:flatten(io_lib:format("(a ~w)", [Type]))
	end.

list(List) when is_list(List) ->
	"[" ++ lists:map(fun(L)->string(L) ++ "," end, List) ++ "]".

split_node_id(NodeID) ->
	lists:flatten(io_lib:format("<split ~p>", [node_id(NodeID)])).
	
union_node_id(NodeID) ->
	lists:flatten(io_lib:format("<union ~w>", [node_id(NodeID)])).
	
atom_node_id(NodeID) ->
	lists:flatten(io_lib:format("<atom ~w>", [node_id(NodeID)])).

node_id(NodeID) ->
	case get(NodeID) of
		undefined ->
			ID = case get(node_id_counter) of
				undefined -> 0;
				Else -> Else
			end,
			put(node_id_counter, ID+1),
			put(NodeID, ID),
			get(NodeID);
		SomeShortNodeID ->
			SomeShortNodeID
	end.

