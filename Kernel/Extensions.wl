(* ::Package:: *)

(* ::Chapter:: *)
(*Extensions*)


BeginPackage["KirillBelov`HTTPHandler`Extensions`", {"KirillBelov`Internal`"}]; 


GetFileRequestQ::usage = 
"GetFileRequestQ[fileType] returns function for checking that is a Get request and path contatins file."; 


URLPathToFileName::usage = 
"URLPathToFileName[request] to file path"; 


FileNameToURLPath::usage = 
"FileNameToURLPath[file] to url."; 


ImportFileAsText::usage = 
"ImportFileAsText[request] import file from request as text data."; 


Begin["`Private`"];


GetFileRequestQ[fileType: _String | {__String} | _StringExpression] := 
AssocMatchQ[<|"Method" -> "GET", "Path" -> "/" ~~ ___ ~~ "." ~~ fileType|>]; 


URLPathToFileName[urlPath_String] := 
FileNameJoin[FileNameSplit[StringTrim[urlPath, "/"]]]; 


URLPathToFileName[request_Association] := 
URLPathToFileName[request["Path"]]; 


FileNameToURLPath[fileName_String] := 
URLBuild[FileNameSplit[StringTrim[fileName, StartOfString ~~ Directory[]]]]; 


ImportFileAsText[file_String] := 
Import[file, "String"]; 


ImportFileAsText[request_Association] := 
ImportFileAsText[URLPathToFileName[request["Path"]]]; 


End[]; 


EndPackage[]; 