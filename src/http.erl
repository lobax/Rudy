-module(http). 
-export([parse_request/1,ok/1,fnf/0,bad_req/0]). 

fnf() -> 
    "HTTP/1.1 404 Not Found\r\n" ++ "\r\n".

ok(Body) -> 
    "HTTP/1.1 200 OK\r\n" ++ "\r\n" ++ Body. 

bad_req() -> 
    "HTTP/1.1 400 Bad Request\r\n\r\n". 

parse_request(R0) ->
    Request_Line   = request_line(R0),
    case Request_Line of
        {error, Reason} -> 
            {error, Reason}; 
        _ -> 
            {Request, R1}   = request_line(R0),
            {Headers, R2}   = headers(R1), 
            {Body, _}       = message_body(R2), 
            {Request, Headers, Body}
    end. 

request_line(R0) -> 
    Method_String    = method_string(R0),
    case Method_String of 
        {error, bad_req} -> 
            io:format("Bad request ~w~n",[R0]),
            {error, bad_req};
        {Method, R1} -> 
            {URI, R2}       = request_uri(R1), 
            {Ver, R3}       = http_version(R2), 
            [13, 10|R4]  = R3, 
            {{Method, URI, Ver}, R4} 
    end. 

method_string([$G, $E, $T, 32|R0]) -> 
    {get, R0}; 
method_string([$H, $E, $A, $D, 32|R0]) -> 
    {head, R0}; 
method_string([$P, $O, $S, $T, 32|R0]) -> 
    {post, R0}; 
method_string(_) -> 
    {error, bad_req}. 

request_uri([32|R0]) -> 
    {[], R0}; 
request_uri([C|R0]) -> 
    {Rest, R1} = request_uri(R0), 
    {[C|Rest], R1}. 

http_version([$H, $T, $T, $P, $/, $1, $., $1 | R0]) ->
    {v11, R0}; 

http_version([$H, $T, $T, $P, $/, $1, $., $0 | R0]) ->
    {v10, R0}. 

headers([13,10|R0]) -> 
    {[], R0};

headers(R0) -> 
    {Header, R1}    = header(R0),
    {Rest, R2}      = headers(R1), 
    {[Header|Rest], R2}. 

header([13, 10|R0]) -> 
    {[], R0}; 
header([C|R0]) -> 
    {Rest, R1} = header(R0), 
    {[C|Rest], R1}. 

message_body(R) -> 
    {R, []}. 

