#!/usr/bin/env wolframscript
(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]]

$PublicDirectory = "Plotter2"

Once[If[PacletFind["KirillBelov/Internal"] === {}, PacletInstall["KirillBelov/Internal"]]]; 
<<KirillBelov`Internal`;
Once[If[PacletFind["KirillBelov/TCPServer"] === {}, PacletInstall["KirillBelov/TCPServer"]]]; 
<<KirillBelov`TCPServer`;
Once[If[PacletFind["JerryI/WSP"] === {}, PacletInstall["JerryI/WSP"]]]; 
<<JerryI`WSP`;

Get[FileNameJoin[{ParentDirectory[], "Kernel", "HTTPHandler.wl"}]];
Get[FileNameJoin[{ParentDirectory[], "Kernel", "Extensions.wl"}]];
Get[FileNameJoin[{ParentDirectory[], "Kernel", "WSPAdapter.wl"}]];

Get["https://raw.githubusercontent.com/JerryI/wl-misc/main/Kernel/Channels.wl"]
Get["https://raw.githubusercontent.com/JerryI/wl-misc/main/Kernel/Events.wl"]

Get["Plotter2/modules/Hydrator.wl"]
Get["Plotter2/modules/Polyfill.wl"]

Print["Staring HTTP server..."];

tcp = TCPServer[];
tcp["CompleteHandler", "HTTP"] = HTTPPacketQ -> HTTPPacketLength;
tcp["MessageHandler", "HTTP"]  = HTTPPacketQ -> http;

http = HTTPHandler[];

http["MessageHandler", "Index"] =AssocMatchQ[<|"Path" -> "/"|>] -> (HypertextProcess[#, "index.wsp", "Base"->$PublicDirectory]&)
http["MessageHandler", "WSP"] = GetFileRequestQ[{"wsp"}] -> (HypertextProcess[#, "Base"->$PublicDirectory]&)

(* ::End:: *)

httplistener = SocketListen[8000, tcp@#&]

Print["Staring WS/HTTP server..."];

wcp = TCPServer[]
wcp["CompleteHandler", "WebSocket"] = WebSocketPacketQ -> WebSocketPacketLength
wcp["MessageHandler", "WebSocket"]  = WebSocketPacketQ -> ws

ws = WebSocketHandler[]

ws["MessageHandler", "Evaluate"]  = Function[True] -> evaluate

evaluate[cl_SocketObject, data_ByteArray] := Block[{Global`client = cl},
    Print["--------------------------------"];
    Print[data//ByteArrayToString];
    ToExpression[data//ByteArrayToString];
]

SocketListen[8005, wcp@#&]

SetOptions[WSPEngine, "Cache"->False];

WebSocketChannel[Automatic]["Serializer"] = ExportByteArray[#, "ExpressionJSON"]&
WebSocketChannel["jsio"]["Serializer"] = ExportByteArray[#, "ExpressionJSON"]&

StringTemplate["Open your browser on ``"][URLBuild[<|"Scheme" -> "http", 
  "Port" -> httplistener["Socket"]["DestinationPort"], 
  "Domain" -> First[httplistener["Socket"]["DestinationIPAddress"]]|>]] // Print;

While[True, Pause[1]];