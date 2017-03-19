-module(monitor). 
-export([start/0]). 

logfile() -> 
    "out/rudy.log".

start() -> 
    spawn(fun() -> init() end).

init() -> 
    receive
        {echo, P} -> 
            P ! echo;
        listen -> 
            listen();
        quit -> 
            ok
    end. 

listen() -> 
    erlang:send_after(1000,self(),reset),
    listen(0). 

listen(Count) -> 
    receive 
        reset -> 
            case Count of 
                0 -> 
                    erlang:send_after(1000,self(),reset),
                    listen(0);
                _ -> 
                    erlang:send_after(1000,self(),reset),
                    file:write_file(logfile(),  io_lib:fwrite("~p Requests per second: ~p~n", [os:timestamp(), Count div 2]), [append]),
                    io:format("Request per second: ~p~n",[Count div 2]),
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
    
    




