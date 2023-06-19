#!/usr/bin/env wolframscript
(* ::Package:: *)

<<KirillBelov`Internal`
<<KirillBelov`TCPServer`
<<KirillBelov`HTTPHandler`


tcp = TCPServer[]


tcp["CompleteHandler", "HTTP"] = HTTPQ -> HTTPLength
tcp["MessageHandler", "HTTP"] = HTTPQ -> http


http = HTTPHandler[]


http["MessageHandler", "Index"] = AssocMatchQ[<|"Path" -> "/"|>] -> indexPage
http["MessageHandler", "Image"] = AssocMatchQ[<|"Path" -> __ ~~ ".png"|>] -> image
http["MessageHandler", "HTML"] = AssocMatchQ[<|"Path" -> __ ~~ ".html"|>] -> html


indexPage[_] := "<h1>Index</h1>"


image[request_Association] := Import[FileNameJoin[Flatten[{Directory[], FileNameSplit[request["Path"]]}]]]


html[request_Association] := "<html><body><h2>" <> request["Path"] <> "</h2></html></body>"


listener = SocketListen[8000, tcp@#&]


While[True, Pause[0.001]]
