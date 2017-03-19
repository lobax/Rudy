-module(rudy2). 
-export([start/1, stop/0, handler/3]). 

start(Port) -> 
    register(rudy, spawn(fun() -> 
                    init(Port), % Starts listening to a port and spawns processes that accept requests
                    timer:sleep(infinity)   % We need to idle to keep the listening port active
            end)). 

stop() -> 
    exit(whereis(rudy), "time to die"). 


%%%%%%%%%%%%%%%%%%
%%%    INIT    %%%
%%%%%%%%%%%%%%%%%%

init(Port) -> 
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of 
        {ok, Listen} -> 
            monitor2:start(Listen), % Start monitor process 
            ok; 
        {error, _Error} -> 
            error
    end. 

%%%%%%%%%%%%%%%%%%
%%%  HANDLER   %%%
%%%%%%%%%%%%%%%%%%


handler(Id, Listen, Monitor) -> 
    case gen_tcp:accept(Listen) of
        {ok, Client} -> 
            Monitor ! {request_accepted, Id}, 
            request(Client),
            Monitor ! {request_handled, Id}, 
            handler(Id, Listen, Monitor);
        {error, Error} -> 
            io:format("ERROR: ~p~n", [Error]),
            error
    end. 

request(Client) -> 
    Recv = gen_tcp:recv(Client, 0), 
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


reply({{get, URI, _}, _, _}) -> 
    Start = erlang:system_time(micro_seconds),
    timer:sleep(20),
    case file:read_file( "web_html" ++ URI) of
        {ok, File} -> 
            End = erlang:system_time(micro_seconds),
            %io:format("[Request] ~p ~n[Response (processed in ~p)] ~n",[URI, End - Start]),
            http:ok([File]);
        {error, _Reason} -> 
            End = erlang:system_time(micro_seconds),
            %io:format("[Request] ~p ~n[Response (processed in ~p)] ~n",[URI, End - Start]),
            http:fnf() 
    end. 

