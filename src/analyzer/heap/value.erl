-module (value).

-include("include/values.hrl").

-export ([merge/2]).

%any with anything else
merge(#any{}=Any, _Value) ->
	Any;
merge(_Value, #any{}=Any) ->
	Any;
	
%some with some, some with option, some with value
merge(#some{type=Type}, #some{type=Type}=V) ->
	V;
merge(#some{type=_Type1}, #some{type=_Type2}) ->
	#any{};
merge(#some{type=Type}=V, #one_of{type=Type}) ->
	V;
merge(#one_of{type=Type}, #some{type=Type}=V) ->
	V;
merge(#some{type=_Type1}, #one_of{type=_Type2}) ->
	#any{};
merge(#one_of{type=_Type2}, #some{type=_Type}) ->
	#any{};
merge(#some{type=Type}=V, Value) ->
	if
		Type =:= element(1, Value) ->
			V;
		true ->
			#any{}
	end;
merge(Value, #some{type=Type}=V) ->
	if
		Type =:= element(1, Value) ->
			V;
		true ->
			#any{}
	end;
merge(#one_of{type=Type}, #some{type=Type}=V) ->
	V;

%option with option, option with value
merge(#one_of{type=Type, value_set=Options1}, #one_of{type=Type, value_set=Options2}) ->
	#one_of{type=Type, value_set=sets:union(Options1, Options2)};
merge(#one_of{type=Type, value_set=Options1}, Value) ->
	if
		Type =:= element(1, Value) ->
			#one_of{type=Type, value_set=sets:add_element(Value, Options1)};
		true ->
			#any{}
	end;
merge(Value, #one_of{type=Type, value_set=Options1}) ->
	if
		Type =:= element(1, Value) ->
			#one_of{type=Type, value_set=sets:add_element(Value, Options1)};
		true ->
			#any{}
	end;

%now, the only unhandled possibilities are only values sym, num, block, nil, loc, act
merge(Value, Value) -> 
	Value;
merge(Value, #nil{}) ->
	#one_of{type=element(1, Value), value_set=sets:from_list([Value, #nil{}])};
merge(#nil{}, Value) ->
	#one_of{type=element(1, Value), value_set=sets:from_list([Value, #nil{}])};
%two sym, num, block, nil, loc, act in any combination
merge(Value1, Value2) ->
	Type1 = element(1, Value1),
	Type2 = element(1, Value2),
	if 
		Type1 =:= Type2 ->
			#one_of{type=Type1, value_set=sets:from_list([Value1, Value2])};
		true ->
			#any{}
	end.