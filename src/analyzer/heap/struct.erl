-module (struct).

-include("include/values.hrl").
-include("include/debug.hrl").

-export ([new/0]).
-export ([is_struct/1]).
-export ([set/3, get/2]).

-record (struct, {slots}).

new() ->
	#struct{slots=dict:new()}.
	
is_struct(#struct{}) ->
	true;
is_struct(_Else) ->
	false.

%note: we cannot use the whole #sym{} record because the same sym might
% occur with different nth fields (not sure if that will stay, but that's how it is right now!)
set(#sym{name=Slot}, #nil{}, Struct) ->	
	?f("setting struct slot ~w to nil", [Slot]),
	Struct#struct{slots=dict:erase(Slot, Struct#struct.slots)};
set(#sym{name=Slot}, Value, Struct) ->
	?f("setting struct slot ~w to ~s", [Slot, pretty:string(Value)]),
	Struct#struct{slots=dict:store(Slot, Value, Struct#struct.slots)}.
	
get(#sym{name=Slot}, Struct) ->
	case dict:find(Slot, Struct#struct.slots) of
		{ok, Value} -> 
			?f("getting struct slot ~w => ~s", [Slot, pretty:string(Value)]),
			Value;
		error -> 
			?f("getting struct slot ~w => nil", [Slot]),
			#nil{}
	end.