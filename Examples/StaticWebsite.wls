#!/usr/bin/env wolframscript
(* ::Package:: *)

<<KirillBelov`Internal`
<<KirillBelov`TCPServer`
<<KirillBelov`HTTPHandler`
<<KirillBelov`HTTPHandler`Extensions`

SetDirectory[FileNameJoin[{DirectoryName[$InputFileName], "StaticWebsite"}]]

tcp = TCPServer[]
tcp["CompleteHandler", "HTTP"] = HTTPPacketQ -> HTTPPacketLength
tcp["MessageHandler", "HTTP"]  = HTTPPacketQ -> http

http = HTTPHandler[]
http["MessageHandler", "Index"]  = AssocMatchQ[<|"Path" -> "/"|>] -> Function[ImportFileAsText["index.html"]]
http["MessageHandler", "File"]  = GetFileRequestQ[{"html", "css", "js", "png", "jpg", "svg"}] -> ImportFileAsText

listener = SocketListen[8000, tcp@#&]
While[True, Pause[0.001]]
