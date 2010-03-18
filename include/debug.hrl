% f as in format
-define (f (Format, Params), debug:log(Format, Params)).

% p as in print
-define (p (String), debug:log(String, [])).

% i as in info
-define (i, debug:log("~p ~p", [?MODULE, ?LINE])).