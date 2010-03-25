-module (array).

-export ([new/0]).
-export ([write/4, read/3]).

-record (array, {values}).

new() ->
	#array{values=[]}.
	
write(_WritingNodeID, Index, Value, #array{}=Arr) ->
	Arr#array{values=[{Index, Value}|Arr#array.values]}.
	
read(_WritingNodeID, Index, not_yet_implemented) ->
	ok.