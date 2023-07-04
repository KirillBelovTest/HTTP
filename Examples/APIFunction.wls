#!/usr/bin/env wolframscript
(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]]

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


http["MessageHandler", "Example1"]  = AssocMatchQ[<|"Path" -> "/cellular"|>] -> cellularAutomata

cellularAutomata[request_Association] := APIFunction[{
   "rule" -> Restricted["Number", {0, 255, 1}] -> 30, 
   "step" -> Restricted["Number", {0, Infinity, 1}] -> 50
   }, 
  ExportString[ArrayPlot[CellularAutomaton[#rule, {{1}, 0}, #step], 
    Frame -> False], "PNG"] &
][request["Query"]]


http["MessageHandler", "Example2"]  = AssocMatchQ[<|"Path" -> "/bars"|>] -> barDiagram

barDiagram[request_Association] := APIFunction[
  "v" -> DelimitedSequence["Number"], 
  ExportString[BarChart[#v], "PNG"] &
][request["Query"]]

(* ::End:: *)

httplistener = SocketListen[8000, tcp@#&]

SetOptions[WSPEngine, "Cache"->False];

StringTemplate["Open your browser on ``cellular?rule=69&step=500"][URLBuild[<|"Scheme" -> "http", 
  "Port" -> httplistener["Socket"]["DestinationPort"], 
  "Domain" -> First[httplistener["Socket"]["DestinationIPAddress"]]|>]] // Print;

StringTemplate["Open your browser on ``bars?v=1,2,3,4,5,6"][URLBuild[<|"Scheme" -> "http", 
  "Port" -> httplistener["Socket"]["DestinationPort"], 
  "Domain" -> First[httplistener["Socket"]["DestinationIPAddress"]]|>]] // Print;  

While[True, Pause[1]];