-module(rudy2). 
-export([start/1, stop/0]). 

start(Port) -> 
    register(rudy, spawn(fun() -> 
                    init(Port), % Starts listening to a port and spawns processes that accept requests
                    timer:sleep(infinity)   % We need to idle to keep the listening port active
            end)). 

stop() -> 
    exit(whereis(rudy), "time to die"). 

%% Constants
number_of_handlers() -> 
    4.

%%%%%%%%%%%%%%%%%%
%%% INIT PORT %%%
%%%%%%%%%%%%%%%%%%

init(Port) -> 
    Monitor = monitor:start(), % Start monitor process 
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of 
        {ok, Listen} -> 
            Monitor ! listen, % Prepare server monitor
            spawn_handlers(Listen, Monitor), 
            ok; 
        {error, _Error} -> 
            error
    end. 

%%%%%%%%%%%%%%%%%%
%%%  HANDLER   %%%
%%%%%%%%%%%%%%%%%%

spawn_handlers(Listen, Monitor) -> 
    N = number_of_handlers(), 
    spawn_handlers(N, Listen, Monitor). 

spawn_handlers(0, _Listen, _Monitor) -> 
    io:format("Four handlers initiated~n"),
    ok;
spawn_handlers(N, Listen, Monitor) -> 
    spawn_link(fun() -> handler(Listen, Monitor) end), 
    spawn_handlers(N-1, Listen, Monitor). 

handler(Listen, Monitor) -> 
    case gen_tcp:accept(Listen) of
        {ok, Client} -> 
            Monitor ! request_accepted, 
            request(Client),
            handler(Listen, Monitor);
        {error, Error} -> 
            io:format("ERROR: ~p~n", [Error]),
            error
    end. 

request(Client) -> 
    Recv = gen_tcp:recv(Client, 0), 
    io:format("~n"),
    case Recv of
        {ok, Str} -> 
            Request = http:parse_request(Str),
            Response = reply(Request), 
            gen_tcp:send(Client, Response); 
        {error, Error} -> 
            io:format("rudy: error: ~w~n", [Error]) 
    end, 
    gen_tcp:close(Client). 

reply({error, Reason}) -> 
    case Reason of 
        bad_req -> 
            http:bad_req()
    end; 

reply({{get, [$/ |URI], _}, _, _}) -> 
    io:format("[Request] ~p ~n[Response] ",[URI]),
    case file:read_file(URI) of
        {ok, File} -> 
            io:format("~p ~n",[File]), 
            http:ok([File]);
        {error, _Reason} -> 
            io:format("404 Not found~n"),
            http:fnf() 
    end. 
