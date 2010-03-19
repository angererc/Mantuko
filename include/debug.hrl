-define (FATAL, 1).
-define (WARNING, 2).
-define (LOG, 3).
-define (DEBUG, 4).

% f as in format
-define (f (Format, Params), debug:log(Format, Params)).

% p as in print
-define (p (String), debug:log(String, [])).

% i as in info
-define (i, debug:log("~p ~p", [?MODULE, ?LINE])).