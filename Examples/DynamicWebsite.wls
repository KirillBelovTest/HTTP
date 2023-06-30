#!/usr/bin/env wolframscript
(* ::Package:: *)

<<KirillBelov`Internal`
<<KirillBelov`TCPServer`
<<KirillBelov`WebSocketHandler`

SetDirectory[FileNameJoin[{DirectoryName[$InputFileName], "DynamicWebsite"}]]
$PublicDirectory = "public"

RandomWord[]

Get["https://raw.githubusercontent.com/JerryI/tinyweb-mathematica/master/WSP/WSP.wl"]

Get[FileNameJoin[{ParentDirectory[ParentDirectory[]], "Kernel", "HTTPHandler.wl"}]]
Get[FileNameJoin[{ParentDirectory[ParentDirectory[]], "Kernel", "Extensions.wl"}]]
Get[FileNameJoin[{ParentDirectory[ParentDirectory[]], "Kernel", "WSPAdapter.wl"}]]

Get["modules/Hydrator.wl"]
Get["modules/Events.wl"]
Get["modules/Polyfill.wl"]

Print["Staring HTTP server..."];

tcp = TCPServer[]
tcp["CompleteHandler", "HTTP"] = HTTPPacketQ -> HTTPPacketLength
tcp["MessageHandler", "HTTP"]  = HTTPPacketQ -> http

http = HTTPHandler[]
http["MessageHandler", "File"]  = GetFileRequestQ[{"html", "css", "js", "png", "jpg", "svg"}] -> (ImportFileAsText[#, "Base"->$PublicDirectory]&)
http["MessageHandler", "WSP"] = GetFileRequestQ[{"wsp"}] -> (HypertextProcess[#, "Base"->$PublicDirectory]&)
http["MessageHandler", "Index"]  = AssocMatchQ[<|"Path" -> "/"|>] ->  (HypertextProcess[#, "index.wsp", "Base"->$PublicDirectory]&)

httplistener = SocketListen[8000, tcp@#&]

Print["Staring WS/HTTP server..."];

wcp = TCPServer[]
wcp["CompleteHandler", "WebSocket"] = WebSocketPacketQ -> WebSocketPacketLength
wcp["MessageHandler", "WebSocket"]  = WebSocketPacketQ -> ws

ws = WebSocketHandler[]

WebSocketBroadcast[data_] := Map[WebSocketSend[#, data]&, (ws@Connections)["Elements"]]; 
WebSocketBroadcast[ws_, data_] := WebSocketBroadcast[data]

ws["MessageHandler", "Evaluate"]  = Function[True] -> evaluate



evaluate[cl_SocketObject, data_ByteArray] := Block[{Global`client = cl},
    ToExpression[data//ByteArrayToString];
]

SocketListen[8005, wcp@#&]

StringTemplate["Open your browser on ``"][URLBuild[<|"Scheme" -> "http", 
  "Port" -> httplistener["Socket"]["DestinationPort"], 
  "Domain" -> First[httplistener["Socket"]["DestinationIPAddress"]]|>]] // Print;

Get /@ FileNames["server.wl", "public", Infinity];

While[True, Pause[1]];