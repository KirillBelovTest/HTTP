#!/usr/bin/env wolframscript
(* ::Package:: *)

<<KirillBelov`CSocketListener`
<<KirillBelov`Internal`
<<KirillBelov`TCPServer`
<<KirillBelov`HTTPHandler`
<<KirillBelov`HTTPHandler`Extensions`

SetDirectory[FileNameJoin[{DirectoryName[$InputFileName], "Website"}]]

tcp = TCPServer[]
tcp["CompleteHandler", "HTTP"] = Function[True] -> Function[Length[#2]]
tcp["MessageHandler", "HTTP"]  = Function[True] -> Function[
    Module[{path, body}, 
        path = StringTrim[StringSplit[StringSplit[ByteArrayToString[#2], "\r\n"][[1]], " "][[2]], "/"]; 
        body = Import[Echo@path]; 
        StringToByteArray["HTTP/1.1 200 OK\r\nConnection: Keep-Alive\r\nContent-Type: text/html\r\nContent-Length: " <> ToString[StringLength[body]] <> "\r\n\r\n" <> body]
    ]
]

listener = CSocketListen[8000, tcp@#&]

UsingFrontEnd[SystemOpen["http://localhost:8000/file.txt"]]

While[True, Pause[0.01]]
