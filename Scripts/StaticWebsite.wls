#!/usr/bin/env wolframscript
(* ::Package:: *)

(*Init*)


<<KirillBelov`Internal`
<<KirillBelov`TCPServer`
<<KirillBelov`HTTPHandler`


(*Internal*)


pathToFile[path_String] := FileNameJoin[Flatten[{Directory[], FileNameSplit[path]}]]


loadWSP[file_String, query_Association] := StringTemplate[Import[file, "String"]][query]


(*TCP*)


tcp = TCPServer[]


tcp["CompleteHandler", "HTTP"] = HTTPQ -> HTTPLength
tcp["MessageHandler", "HTTP"]  = HTTPQ -> http


(*HTTP*)


http = HTTPHandler[]


http["MessageHandler", "Index"]  = AssocMatchQ[<|"Path" -> "/"|>]           -> index
http["MessageHandler", "Image"]  = AssocMatchQ[<|"Path" -> __ ~~ ".png"|>]  -> image
http["MessageHandler", "Script"] = AssocMatchQ[<|"Path" -> __ ~~ ".js"|>]   -> script
http["MessageHandler", "HTML"]   = AssocMatchQ[<|"Path" -> __ ~~ ".html"|>] -> html
http["MessageHandler", "WSP"]    = AssocMatchQ[<|"Path" -> __ ~~ ".wsp"|>]  -> wsp
http["MessageHandler", "Folder"] = AssocMatchQ[<|"Path" -> "/" ~~ __|>]     -> folder


index[request_Association] := loadWSP[pathToFile["/index.wsp"], request["Query"]]
image[request_Association] := Import[pathToFile[request["Path"]]]
script[request_Association] := Import[pathToFile[request["Path"]], "String"]
html[request_Association] := Import[pathToFile[request["Path"]], "String"]
wsp[request_Association] := loadWSP[pathToFile[request["Path"]], request["Query"]]
folder[request_Association] := StringRiffle[Map["<a>" <> # <> "</a>"&] @ FileNames[pathToFile[request["Path"]]], "\n"]


(*Start*)


listener = SocketListen[8000, tcp@#&]
While[True, Pause[0.001]]
