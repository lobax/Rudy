-module(rudy). 
-export([start/1, stop/0]). 

start(Port) -> 
    register(rudy, spawn(fun() -> init(Port) end)). 


stop() -> 
    exit(whereis(rudy), "time to die"). 

%%%%%%%%%%%%%%%%%%
%%% INIT PORTS %%%
%%%%%%%%%%%%%%%%%%

init(Port) -> 
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of 
        {ok, Listen} -> 
            handler(Listen), 
            gen_tcp:close(Listen), 
            ok; 
        {error, _Error} -> 
            error
    end. 

%%%%%%%%%%%%%%%%%%
%%%  HANDLER   %%%
%%%%%%%%%%%%%%%%%%

handler(Listen) -> 
    case gen_tcp:accept(Listen) of
        {ok, Client} -> 
            request(Client),
            handler(Listen);
        {error, _Error} -> 
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
