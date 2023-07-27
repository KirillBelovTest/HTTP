#!/usr/bin/env wolframscript
(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]]

$PublicDirectory = FileNameJoin[{Directory[], "Plotter2"}]

Once[If[PacletFind["KirillBelov/Internal"] === {}, PacletInstall["KirillBelov/Internal"]]]; 
<<KirillBelov`Internal`;
Once[If[PacletFind["KirillBelov/TCPServer"] === {}, PacletInstall["KirillBelov/TCPServer"]]]; 
<<KirillBelov`TCPServer`;
Once[If[PacletFind["JerryI/WSP"] === {}, PacletInstall["JerryI/WSP"]]]; 
<<JerryI`WSP`;


Get[FileNameJoin[{ParentDirectory[], "Kernel", "HTTPHandler.wl"}]];
Get[FileNameJoin[{ParentDirectory[], "Kernel", "Extensions.wl"}]];

Once[If[PacletFind["KirillBelov/WebSocketHandler"] === {}, PacletInstall["KirillBelov/WebSocketHandler"]]]; 
<<KirillBelov`WebSocketHandler`;

Get["https://raw.githubusercontent.com/JerryI/wl-misc/main/Kernel/Events.wl"]
Get["https://raw.githubusercontent.com/JerryI/wl-wsp/main/Kernel/Hydrator.wl"]

Print["Staring HTTP server..."];

tcp = TCPServer[];
tcp["CompleteHandler", "HTTP"] = HTTPPacketQ -> HTTPPacketLength;
tcp["MessageHandler", "HTTP"]  = HTTPPacketQ -> http;

http = HTTPHandler[];

http["MessageHandler", "Index"] =AssocMatchQ[<|"Path" -> "/"|>] -> (HypertextProcess[#, "index.wsp", "Base"->$PublicDirectory]&)
http["MessageHandler", "WSP"] = GetFileRequestQ[{"wsp"}] -> (HypertextProcess[#, "Base"->$PublicDirectory]&)

(* ::End:: *)

httplistener = CSocketListen[8000, tcp@#&]

Print["Staring WS/HTTP server..."];

wcp = TCPServer[]
wcp["CompleteHandler", "WebSocket"] = WebSocketPacketQ -> WebSocketPacketLength
wcp["MessageHandler", "WebSocket"]  = WebSocketPacketQ -> ws

ws = WebSocketHandler[]

ws["MessageHandler", "Evaluate"]  = Function[True] -> evaluate

evaluate[cl_, data_ByteArray] := Block[{Global`client = cl},
    Print["--------------------------------"];
    Print[data//ByteArrayToString];
    ToExpression[data//ByteArrayToString];
]

CSocketListen[8005, wcp@#&]

SetOptions[WSPEngine, "Cache"->False];

StringTemplate["Open your browser on ``"][URLBuild[<|"Scheme" -> "http", 
  "Port" -> httplistener["Socket"]["DestinationPort"], 
  "Domain" -> First[httplistener["Socket"]["DestinationIPAddress"]]|>]] // Print;

While[True, Pause[1]];