-module(test). 
-export([bench/1]). 

bench(T) ->
    file:write_file("output.dat",[""]),
    N = 100,
    Port = 8080,
    bench(N,T,Port).

bench(_,0,_) ->
    ok;
bench(N, T, Port) -> 
    Start = erlang:system_time(micro_seconds),
    {End, {Disconnects, Dropped}} = thread_spawn(N, T, Port),
    file:write_file("output.dat",  io_lib:fwrite("~p ~p ~p ~p ~p~n", [N, T, End - Start, Disconnects, Dropped]), [append]),
    io:format("The execution time is: ~p, disconnects: ~p, drops: ~p~n", [End - Start, Disconnects, Dropped]),
    bench(N, T-1, Port).

thread_spawn(_, 0, _) -> 
    {0, {0,0}};

thread_spawn(N, T, Port) -> 
    Pid = self(),
    spawn(fun() -> thread(Pid, N ,Port) end), 
    {Best_Time, {Disconnects, Dropped}} = thread_spawn(N, T-1, Port),
    receive 
        {ok, Time, {Dis, Drop}} -> 
            if Time > Best_Time -> 
                    {Time, {Disconnects+Dis,Dropped+Drop}}; 
                true -> 
                    {Best_Time,{Disconnects+Dis,Dropped+Drop}} 
            end; 
        Error -> 
            io:format("~p", [Error])
    end. 

thread(Pid,N,Port) -> 
    Fails = run(N, localhost, Port), 
    Finish = erlang:system_time(micro_seconds), 
    Pid ! {ok, Finish, Fails}.

run(N, localhost, Port) -> 
    run(N, localhost, Port, {0,0}). 

run(0, _, _, {Disconnects, Dropped}) -> 
    {Disconnects, Dropped};
run(N, Host, Port, {Disconnects, Dropped}) -> 
    {Dis, Drop} = request(Host, Port), 
    run(N-1, Host, Port, {Disconnects+Dis,Dropped+Drop}). 


request(Host, Port) -> 
    request(Host, Port, {0, 0}). 

request(Host, Port, {Disconnects, Dropped}) -> 
    Opt = [list, {active, false}, {reuseaddr, true}],
    Connect = gen_tcp:connect(Host, Port, Opt),
    case Connect of 
        {ok, Server} -> 
            gen_tcp:send(Server, requests:get("/foo")),
            Recv = gen_tcp:recv(Server, 0),
            case Recv of
               {ok, _} ->
                   Failures = {Disconnects, Dropped},
                   ok;
               {error, Error} ->
                   io:format("Connection droped: ~w~n", [Error]), 
                   Failures = request(Host, Port, {Disconnects, Dropped + 1})
            end,
            gen_tcp:close(Server), 
            Failures;
        _Error -> 
            io:format("Connection refuesed~n"), 
            request(Host, Port, {Disconnects + 1, Dropped})
    end. 
            
