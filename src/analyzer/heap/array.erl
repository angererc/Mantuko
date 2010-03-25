-module (array).

-export ([new/0]).
-export ([set/3, get/2]).

-record (array, {values}).

new() ->
	#array{values=[]}.
	
set(Index, Value, #array{}=Arr) ->
	Arr#array{values=[{Index, Value}|Arr#array.values]}.
	
get(Index, not_yet_implemented) ->
	ok.