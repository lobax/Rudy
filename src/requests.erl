-module(requests). 
-compile(export_all). 

get(URI) -> 
    "GET " ++ URI ++ " HTTP/1.1\r\n" ++ "\r\n". 

post(URI, BODY) -> 
    "POST " ++ URI ++ " HTTP/1.1\r\n" ++ "\r\n" ++ BODY. 
