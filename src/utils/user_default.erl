%this file is loaded automatically into the erlang shell 
-module(user_default).

-export([la/0]).

%reload all modules
la() ->
    Modules = [M || {M, P} <- code:all_loaded(), is_list(P) andalso string:str(P, "angererc") > 0],
    [shell_default:l(M) || M <- Modules].