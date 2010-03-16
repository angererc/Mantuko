-module (utils).

-compile(export_all).

dict_store_if_not_present(Key, InitialFun, Dict) ->
	case dict:find(Key, Dict) of 
		{ok, _Value} ->
			Dict;
		error ->
			dict:store(Key, InitialFun(), Dict)
	end.

rpc(PID, Request) ->
	PID ! {self(), Request},
	receive
		{_Pid, Response} ->
			Response
	end.
