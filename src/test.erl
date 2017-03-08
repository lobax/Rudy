-module(test). 
-export([bench/3]). 


bench(N, T, Port) -> 
    Start = erlang:system_time(micro_seconds),
    End = thread_spawn(N, T, Port), 
    End - Start. 

thread_spawn(_, 0, _) -> 
    0;

thread_spawn(N, T, Port) -> 
    Pid = self(),
    spawn(fun() -> thread(Pid, N ,Port) end), 
    Best_Time = thread_spawn(N, T-1, Port),
    receive 
        {ok, Time} -> 
            if Time > Best_Time -> 
                    Time; 
                true -> 
                    Best_Time
            end; 
        Error -> 
            io:format("~p", [Error])
    end. 

thread(Pid,N,Port) -> 
    run(N, localhost, Port), 
    Finish = erlang:system_time(micro_seconds), 
    Pid ! {ok, Finish}.



run(0, _, _) -> 
    ok;
run(N, Host, Port) -> 
    request(Host, Port), 
    run(N-1, Host, Port). 

request(Host, Port) -> 
    Opt = [list, {active, false}, {reuseaddr, true}],
    Connect = gen_tcp:connect(Host, Port, Opt),
    case Connect of 
        {ok, Server} -> 
            gen_tcp:send(Server, requests:get("/foo")),
            Recv = gen_tcp:recv(Server, 0),
            case Recv of
               {ok, _} ->
                   ok;
               {error, Error} ->
                   io:format("test: error: ~w~n", [Error])
            end,
            gen_tcp:close(Server); 
        _Error -> 
            io:format("Connection refuesed~n")
    end. 
            
