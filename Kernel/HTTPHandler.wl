(* ::Package:: *)

(* ::Chapter:: *)
(*HTTP Handler*)


(*
	message - ByteArray passed from TCPServer handler
	request - Association parsed from message
	response - Null | String | ByteArray for further sending to TCPServer handler
*)


(*+-------------------------------------------------+
  |                 HTTP HANDLER                    |
  |                                                 |
  |               (reseive request)                 |
  |                       |                         |
  |            [parse request to assoc]             |
  |                       |                         |
  |               <select pipeline>                 |
  |      /       /        |        \         \      |
  |     ..   [get..]  [post..]  [delete..]   ..     |
  |              \        |        /                |
  |           [create string response]              |
  |                       |                         |
  |                {return to tcp}                  |
  +-------------------------------------------------+*)


(* ::Section::Closed:: *)
(*Requarements*)


Once[If[PacletFind["KirillBelov/Internal"] === {}, PacletInstall["KirillBelov/Internal"]]]; 
Once[If[PacletFind["KirillBelov/Objects"] === {}, PacletInstall["KirillBelov/Objects"]]]; 
Once[If[PacletFind["KirillBelov/TCPServer"] === {}, PacletInstall["KirillBelov/TCPServer"]]]; 


(* ::Section::Closed:: *)
(*Begin packge*)


BeginPackage["KirillBelov`HTTPHandler`", {"KirillBelov`Objects`", "KirillBelov`Internal`"}]; 


(* ::Section::Closed:: *)
(*Names*)


ClearAll["`*"]


HTTPQ::usage = 
"HTTPQ[client, message] check that message was sent via HTTP protocol."; 


HTTPLength::usage = 
"HTTPLength[client, message] returns expected message length."; 


HTTPHandler::usage = 
"HTTPHandler[opts] mutable type for the handling HTTP request."; 


(* ::Section::Closed:: *)
(*Begin private context*)


Begin["`Private`"]; 


(* ::Section::Closed:: *)
(*HTTPQ*)


HTTPQ[client_SocketObject, message_ByteArray] := 
Module[{head}, 
	head = ByteArrayToString[BytesSplit[message, $httpEndOfHead -> 1][[1]]]; 
	
	(*Return: True | False*)
	And[
		StringLength[head] != Length[message], (* equivalent of the StringContainsQ[message, $httpEndOfHead] *)
		StringContainsQ[head, StartOfString ~~ $httpMethods], 
		Or[
			StringContainsQ[head, StartOfLine ~~ "Connection: keep-alive", IgnoreCase -> True], 
			StringContainsQ[head, StartOfLine ~~ "Connection: close", IgnoreCase -> True]
		]
	]
]; 


(* ::Section::Closed:: *)
(*HTTPLength*)


HTTPLength[client_SocketObject, message_ByteArray] := 
Module[{head}, 
	head = ByteArrayToString[BytesSplit[message, $httpEndOfHead -> 1][[1]]]; 

	(*Return: _Integer*)
	Which[
		StringContainsQ[head, "Content-Length: ", IgnoreCase -> True], 
			StringLength[head] + 4 + ToExpression[StringExtract[head, {"Content-Length: ", "content-length: "} -> 2, "\r\n" -> 1]], 
		True, 
			Length[message]
	]
]; 


(* ::Section::Closed:: *)
(*HTTPHandler*)


CreateType[HTTPHandler, {
	"Handler" -> <||>, 
	"Default" -> Function[$errorResponse], 
	"Deserializer" -> $deserializers, 
	"Serializer" -> $serializers
}]; 


handler_HTTPHandler[client_SocketObject, message_ByteArray] := 
Module[{request, pipeline, result}, 
	(*Reuest: _Association*)
	request = parseRequest[message, handler["Deserializer"]]; 

	(*Result: _String | _Association*)
	result = ConditionApply[handler["Handler"], handler["Default"]][request]; 

	(*Return: _String | _ByteArray*)
	createResponse[result, handler["Serializer"]]
]; 


(* ::Section::Closed:: *)
(*Internal*)


$httpMethods = {"GET", "PUT", "DELETE", "HEAD", "POST", "CONNECT", "OPTIONS", "TRACE", "PATCH"}; 


$httpEndOfHead = StringToByteArray["\r\n\r\n"]; 


$errorResponse = <|"Code" -> 404, "Body" -> "Not found"|>; 


$deserializers = <|
	"JSON" -> 
		Function[AssocMatchQ[#1, <|"Content-Type" -> "application/json"|>] -> 
			Function[ImportByteArray[#2, "RawJSON"]]], 
	"QueryEncoded" -> 
		Function[AssocMatchQ[#1, <|"Content-Type" -> "application/x-www-form-urlencoded"|>] -> 
			Function[Association[URLQueryDecode[ByteArrayToString[#2]]]]]
|>; 


$serializers = <|
	"Graphic" -> Function[Head[#] == Graphics] -> Function[ExportString[#, "SVG"]]
|>; 


parseRequest[message_ByteArray, deserializer_] := 
Module[{headBytes, head, headline, headers, body, bodyBytes}, 
	{headBytes, bodyBytes} = BytesSplit[message, $httpEndOfHead -> 1]; 
	head = ByteArrayToString[headBytes]; 
	
	headline = First @ StringCases[
		StringExtract[head, "\r\n" -> 1], 
		method__ ~~ " " ~~ url__ ~~ " " ~~ version__ :> Join[
			<|"Method" -> method|>, 
			MapAt[Association, Key["Query"]] @ 
			MapAt[URLBuild, Key["Path"]] @ 
			<|URLParse[url]|>[[{"Path", "Query"}]], 
			<|"Version" -> version|>
		], 
		IgnoreCase -> True
	]; 

	headers = Association[
		Map[Rule[#1, StringRiffle[{##2}, ":"]]& @@ Map[StringTrim]@StringSplit[#, ":"] &]@
  		StringExtract[head, "\r\n\r\n" -> 1, "\r\n" -> 2 ;; ]
	]; 

	body = ConditionApply[deserializer, #2&][headers, bodyBytes]; 

	(*Return: _Association*)
	Join[
		headline, 
		<|"Headers" -> headers, "Body" -> body|>
	]
]; 


createResponse[body_, serializer_] := 
createResponse[
	<|
		"Code" -> 200, 
		"Body" -> body
	|>, 
	serializer
]; 


createResponse[assoc_Association, serializer_] := 
Module[{response = assoc, body, headers}, 
	body = ConditionApply[serializer, ToString][response["Body"]]; 

	If[Not[KeyExistsQ[response, "Message"]], response["Message"] = "OK"]; 
	If[Not[KeyExistsQ[response, "Headers"]], response["Headers"] = <|
		"Content-Length" -> StringLength[body]
	|>];  

	(*Return: _String*)
	StringTemplate["HTTP/1.1 `Code` `Message`\r\n"][response] <> 
	StringRiffle[KeyValueMap[StringRiffle[{#1, #2}, ": "]&] @ response["Headers"], "\r\n"] <> 
	"\r\n\r\n" <> 
	body
]; 


(* ::Section::Closed:: *)
(*End private context*)


End[]; 


(* ::Section::Closed:: *)
(*End packet*)


EndPackage[]; 
