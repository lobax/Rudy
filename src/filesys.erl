-module(filesys). 
-export([start/0]). 

web_path() -> 
    "web_html/". 

start() -> 
    Dict = dict:new(),
    spawn( fun() -> wait(Dict) end). 


wait(Dict) ->
    receive 
        {retrieve, Src, Pid} -> 
            case retrieve( web_path() ++ Src, Dict) of
                {error, fnf} -> 
                    Pid ! fnf, 
                    wait(Dict);
                {File, NewDict} ->
                    Pid ! File, 
                    wait(NewDict)
            end;
        quit -> 
            ok
    end. 

retrieve(Src, Dict) ->
    case Dict:find(Src) of
        {ok, File} ->
            {File, Dict}; 
        _ -> 
            case file:read_file(Src) of
                {ok, File} -> 
                    NewDict = dict:store(Src, File, Dict), 
                    {File, NewDict};
                {error, _Reason} -> 
                    {error, fnf}
            end
    end. 
                

