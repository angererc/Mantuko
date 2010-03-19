-module (struct).

-include("include/values.hrl").

-export ([new/0]).
-export ([set/3, get/2]).

-record (struct, {slots}).

new() ->
	#struct{slots=dict:new()}.

set(#sym{}=Slot, #nil{}, Struct) ->	
	Struct#struct{slots=dict:erase(Slot, Struct#struct.slots)};
set(#sym{}=Slot, Value, Struct) ->
	Struct#struct{slots=dict:store(Slot, Value, Struct#struct.slots)}.
	
get(#sym{}=Slot, Struct) ->
	case dict:find(Slot, Struct#struct.slots) of
		{ok, Value} -> Value;
		error -> #nil{}
	end.