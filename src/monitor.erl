-module(monitor). 
-export([start/0]). 

logfile() -> 
    "out/rudy.log".

start() -> 
    spawn(fun() -> init() end).

init() -> 
    Pid = self(),
    Sync = spawn(fun() -> synch(Pid) end), 
    receive
        {echo, P} -> 
            P ! echo;
        listen -> 
            Sync ! start, 
            listen();
        quit -> 
            ok
    end. 

listen() -> 
    listen(0). 

listen(Count) -> 
    receive 
        reset -> 
            case Count of 
                0 -> 
                    listen(0);
                _ -> 
                    file:write_file(logfile(),  io_lib:fwrite("~p Requests per second: ~p~n", [os:timestamp(), Count]), [append]),
                    listen(0)
            end
    after 0 -> 
        receive 
            request_accepted -> 
                listen(Count + 1);
            stop_listening -> 
                ok
        after
            50 -> 
                listen(Count)
        end
    end.

synch(Pid) -> 
    receive 
        start -> 
            synchronizer(Pid)
    end.

synchronizer(Pid) ->
    Pid ! reset, 
    receive 
        quit ->
            ok
    after 
        1000 -> 
            synchronizer(Pid)
    end. 

     
    
    




