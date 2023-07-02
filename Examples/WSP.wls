#!/usr/bin/env wolframscript
(* ::Package:: *)

<<KirillBelov`Internal`
<<KirillBelov`TCPServer`
<<KirillBelov`WebSocketHandler`

SetDirectory[FileNameJoin[{DirectoryName[$InputFileName], "WSP"}]]
$PublicDirectory = ""

Get["https://raw.githubusercontent.com/JerryI/wl-wsp/main/Kernel/WSP.wl"]

Get[FileNameJoin[{ParentDirectory[ParentDirectory[]], "Kernel", "HTTPHandler.wl"}]]
Get[FileNameJoin[{ParentDirectory[ParentDirectory[]], "Kernel", "Extensions.wl"}]]
Get[FileNameJoin[{ParentDirectory[ParentDirectory[]], "Kernel", "WSPAdapter.wl"}]]

Print["Staring HTTP server..."];

tcp = TCPServer[]
tcp["CompleteHandler", "HTTP"] = HTTPPacketQ -> HTTPPacketLength
tcp["MessageHandler", "HTTP"]  = HTTPPacketQ -> http

http = HTTPHandler[]
http["MessageHandler", "File"]  = GetFileRequestQ[{"html", "css", "js", "png", "jpg", "svg"}] -> (
  ImportFileAsText[#, "Base"->$PublicDirectory] &
)
http["MessageHandler", "WSP"] = GetFileRequestQ[{"wsp"}] -> (HypertextProcess[#, "Base"->$PublicDirectory]&)
http["MessageHandler", "Index"]  = AssocMatchQ[<|"Path" -> "/"|>] ->  (HypertextProcess[#, "index.wsp", "Base"->$PublicDirectory]&)

httplistener = SocketListen[8000, tcp@#&]

SetOptions[WSPEngine, "Cache"->False];

StringTemplate["Open your browser on ``"][URLBuild[<|"Scheme" -> "http", 
  "Port" -> httplistener["Socket"]["DestinationPort"], 
  "Domain" -> First[httplistener["Socket"]["DestinationIPAddress"]]|>]] // Print;

While[True, Pause[1]];