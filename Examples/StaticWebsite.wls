#!/usr/bin/env wolframscript
(* ::Package:: *)

<<KirillBelov`Internal`
<<KirillBelov`TCPServer`

SetDirectory[FileNameJoin[{DirectoryName[$InputFileName], "StaticWebsite"}]]

Get[FileNameJoin[{ParentDirectory[ParentDirectory[]], "Kernel", "HTTPHandler.wl"}]]
Get[FileNameJoin[{ParentDirectory[ParentDirectory[]], "Kernel", "Extensions.wl"}]]

Print["Starting HTTP server"];

tcp = TCPServer[]
tcp["CompleteHandler", "HTTP"] = HTTPPacketQ -> HTTPPacketLength
tcp["MessageHandler", "HTTP"]  = HTTPPacketQ -> http

Print["Starting HTTP handler"];

http = HTTPHandler[]
http["MessageHandler", "Index"]  = AssocMatchQ[<|"Path" -> "/"|>] -> Function[ImportFileAsText["index.html"]]
http["MessageHandler", "File"]  = GetFileRequestQ[{"html", "css", "js", "png", "jpg", "svg"}] -> ImportFileAsText

listener = SocketListen[8000, tcp@#&]

Print["Done!"];

While[True, Pause[0.001]]
