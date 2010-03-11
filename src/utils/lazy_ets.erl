-module (lazy_ets).

-export ([open/1, delete/1, lookup/2, insert/2]).

open(Tab) ->
	case ets:info(Tab) of
		undefined -> ets:new(Tab, [set, protected, named_table]);
		_Else -> Tab
	end.
	
delete(Tab) ->
	case ets:info(Tab) of
		undefined -> ok;
		_Else -> 
			ets:delete(Tab)
	end.

lookup(Tab, Key) ->
	case ets:info(Tab) of
		undefined -> undefined;
		_Else -> 
			ets:lookup(Tab, Key)
	end.
	
insert(Tab, ObjectOrObjects) ->
	Tab = open(Tab),
	ets:insert(Tab, ObjectOrObjects).