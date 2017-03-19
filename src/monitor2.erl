-module(monitor2). 
-export([start/1]). 

logfile() -> 
    "out/rudy.log".

min_number_of_threads() -> 
    4. 

%start(Listener) -> 
%    spawn(fun() -> listen(Listener) end).

start(Listener) -> 
    erlang:send_after(1000,self(),print),
    spawn_handlers(min_number_of_threads(), Listener),
    listen(0, {4, [inactive, inactive, inactive, inactive]}, Listener). 

listen(Count, Active, Listener) -> 
    receive 
        print -> 
            erlang:send_after(1000,self(),print),
            {TotalThreads, _} = Active,
            io:format("[Snapshot] Total threads: ~p Active threads: ~p~n ",[TotalThreads, Count]),
            listen(Count, Active, Listener)
    after 0 -> 
        receive 
            {request_accepted, Id} -> 
                NewCount = Count+1,
                NewActive = active_thread(Id, scale(NewCount, Active, Listener)),
                listen(NewCount, NewActive, Listener);
            {request_handled, Id} -> 
                NewCount = Count -1,
                NewActive = inactive_thread(Id, scale(NewCount, Active, Listener)),
                listen(NewCount, NewActive, Listener); 

            stop_listening -> 
                ok
        after
            50 -> 
                listen(Count, Active, Listener)
        end
    end.


scale(ActiveThreads, {Threads, ActiveList}, Listener) when ActiveThreads >= 3 * Threads div 4 ->
    io:format("Active threads: ~p Incrementing total threads to ~p~n ",[ActiveThreads, Threads * 2]),
    add_handlers(Threads, {Threads * 2, ActiveList}, Listener); 
scale(_, Active, _) -> 
    Active. 

add_handlers(Threads, {Threads, ActiveList}, Listener) -> 
    Pid = self(),
    spawn_link(fun() -> rudy2:handler(Threads, Listener, Pid) end),
    {Threads, [inactive|ActiveList]};

add_handlers(Id, {Threads, ActiveList}, Listener) -> 
    Pid = self(),
    spawn_link(fun() -> rudy2:handler(Id, Listener, Pid) end), 
    add_handlers(Id + 1, {Threads, [inactive|ActiveList]}, Listener).

spawn_handlers(0, _Listen) -> 
    ok;
spawn_handlers(N, Listen) -> 
    Pid = self(),
    spawn_link(fun() -> rudy2:handler(N, Listen, Pid) end), 
    spawn_handlers(N-1, Listen). 

active_thread(Id, {Threads, ThreadList}) -> 
   modify_thread_list(Threads - Id, active, {Threads, ThreadList}).  

inactive_thread(Id, {Threads, ThreadList}) -> 
   modify_thread_list(Threads - Id, inactive, {Threads, ThreadList}).  

modify_thread_list(Id, Activity, {Threads, ThreadList}) -> 
    {Threads, modify_thread_list(Id, Activity, ThreadList)}; 
modify_thread_list(0, Activity, [_|T]) -> 
    [Activity|T];
modify_thread_list(Id, Activity, [H|T]) ->
    [H|modify_thread_list(Id-1, Activity, T)]. 
