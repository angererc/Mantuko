% f as in format
-define (f (Format, Params), (begin
	T = lists:flatten(io_lib:format("~p ~p: ~s", [?MODULE, ?LINE, io_lib:format(Format, Params)])),
	erlang:display(T)
end)).

% p as in print
-define (p (String), (begin
	T = lists:flatten(io_lib:format("~p ~p: ~s", [?MODULE, ?LINE, String])),
	erlang:display(T)
end)).

% i as in info
-define (i, (begin
	T = lists:flatten(io_lib:format("~p ~p", [?MODULE, ?LINE])),
	erlang:display(T)
end)).