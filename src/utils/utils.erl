-module (utils).

-compile(export_all).

rpc(PID, Request) ->
	PID ! {self(), Request},
	receive
		{_Pid, Response} ->
			Response
	end.
