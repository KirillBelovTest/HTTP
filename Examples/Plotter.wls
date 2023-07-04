#!/usr/bin/env wolframscript
(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]]

$PublicDirectory = "Plotter"

Once[If[PacletFind["KirillBelov/Internal"] === {}, PacletInstall["KirillBelov/Internal"]]]; 
<<KirillBelov`Internal`;
Once[If[PacletFind["KirillBelov/TCPServer"] === {}, PacletInstall["KirillBelov/TCPServer"]]]; 
<<KirillBelov`TCPServer`;
Once[If[PacletFind["JerryI/WSP"] === {}, PacletInstall["JerryI/WSP"]]]; 
<<JerryI`WSP`;

Get[FileNameJoin[{ParentDirectory[], "Kernel", "HTTPHandler.wl"}]];
Get[FileNameJoin[{ParentDirectory[], "Kernel", "Extensions.wl"}]];
Get[FileNameJoin[{ParentDirectory[], "Kernel", "WSPAdapter.wl"}]];

Print["Staring HTTP server..."];

tcp = TCPServer[];
tcp["CompleteHandler", "HTTP"] = HTTPPacketQ -> HTTPPacketLength;
tcp["MessageHandler", "HTTP"]  = HTTPPacketQ -> http;

http = HTTPHandler[];

http["MessageHandler", "Index"] =AssocMatchQ[<|"Path" -> "/"|>] -> (HypertextProcess[#, "index.wsp", "Base"->$PublicDirectory]&)
http["MessageHandler", "WSP"] = GetFileRequestQ[{"wsp"}] -> (HypertextProcess[#, "Base"->$PublicDirectory]&)

(* ::End:: *)

httplistener = SocketListen[8000, tcp@#&]

SetOptions[WSPEngine, "Cache"->True];

StringTemplate["Open your browser on ``"][URLBuild[<|"Scheme" -> "http", 
  "Port" -> httplistener["Socket"]["DestinationPort"], 
  "Domain" -> First[httplistener["Socket"]["DestinationIPAddress"]]|>]] // Print;

While[True, Pause[1]];