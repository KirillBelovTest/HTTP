#!/usr/bin/env wolframscript
(* ::Package:: *)

<<KirillBelov`Internal`
<<KirillBelov`TCPServer`

SetDirectory[FileNameJoin[{DirectoryName[$InputFileName], "StaticWebsite"}]]

Get[FileNameJoin[{ParentDirectory[ParentDirectory[]], "Kernel", "HTTPHandler.wl"}]]
Get[FileNameJoin[{ParentDirectory[ParentDirectory[]], "Kernel", "Extensions.wl"}]]

tcp = TCPServer[]
tcp["CompleteHandler", "HTTP"] = HTTPPacketQ -> HTTPPacketLength
tcp["MessageHandler", "HTTP"]  = HTTPPacketQ -> http

Print["Starting HTTP handler"];

http = HTTPHandler[]
http["MessageHandler", "Index"]  = AssocMatchQ[<|"Path" -> "/"|>] -> ((StringTemplate[ImportFile["index.html", "StringOutput"->True]][#])&)
http["MessageHandler", "File"]  = GetFileRequestQ[{"html", "txt", "css", "js", "png", "jpg", "svg"}] -> (ImportFile[#, "Base"->{Directory[], "secret"}]&)

listener = SocketListen[9000, tcp@Echo@#&]

StringTemplate["Open your browser on ``"][URLBuild[<|"Scheme" -> "http", 
  "Port" -> listener["Socket"]["DestinationPort"], 
  "Domain" -> First[listener["Socket"]["DestinationIPAddress"]]|>]] // Print;

While[True, Pause[0.001]]
