(* ::Package:: *)

(* ::Chapter:: *)
(*HTTP*)


(*
	message - ByteArray passed from TCPServer handler
	request - Association parsed from message
	response - Null | String | ByteArray for further sending to TCPServer handler
*)


(* ::Program:: *)
(*+-----------------------------------------------+*)
(*|                HTTP HANDLER                   |*)
(*|                                               |*)
(*|              (reseive request)                |*)
(*|                      |                        |*)
(*|           [parse request to assoc]            |*)
(*|                      |                        |*)
(*|              <select pipeline>                |*)
(*|     /       /        |        \         \     |*)
(*|    ..   [get..]  [post..]  [delete..]   ..    |*)
(*|             \        |        /               |*)
(*|          [create string response]             |*)
(*|                      |                        |*)
(*|               {return to tcp}                 |*)
(*+-----------------------------------------------+*)


(* ::Section::Closed:: *)
(*Requarements*)


Once[Map[If[Length[PacletFind[#]] === 0, PacletInstall[#]]&][{
	"KirillBelov/Objects", 
	"KirillBelov/Internal", 
	"KirillBelov/TCP"
}]]; 


(* ::Section:: *)
(*Begin packge*)


BeginPackage["KirillBelov`HTTP`", {
	"KirillBelov`Objects`", 
	"KirillBelov`Internal`", 
	"KirillBelov`TCP`"
}]; 


(* ::Section::Closed:: *)
(*Names*)


ClearAll["`*"]


HTTPPacketQ::usage = 
"HTTPPacketQ[packet] check that message was sent via HTTP protocol."; 


HTTPPacketLength::usage = 
"HTTPPacketLength[packet] returns expected message length."; 


HTTPHandler::usage = 
"HTTPHandler[opts] mutable type for the handling HTTP request."; 


(* ::Section::Closed:: *)
(*Begin private context*)


Begin["`Private`"]; 


(* ::Section::Closed:: *)
(*HTTPPacketQ*)


HTTPPacketQ[___] := False; 


HTTPPacketQ[packet_Association?AssociationQ] /; packet["Event"] === "Received" := 
With[{message = packet["DataByteArray"]}, 
	Module[{head}, 
		head = ByteArrayToString[BytesSplit[message, $httpEndOfHead -> 1][[1]]]; 
		
		(*Return: True | False*)
		And[
			StringLength[head] != Length[message], (* equivalent of the StringContainsQ[message, $httpEndOfHead] *)
			StringContainsQ[head, StartOfString ~~ $httpMethods ~~ " /"]
		]
	]
]; 


(* ::Section::Closed:: *)
(*HTTPPacketLength*)


HTTPPacketLength[packet_Association] := 
With[{message = packet["DataByteArray"]}, 
	Module[{head}, 
		head = ByteArrayToString[BytesSplit[message, $httpEndOfHead -> 1][[1]]]; 

		(*Return: _Integer*)
		Which[
			StringContainsQ[head, "Content-Length: ", IgnoreCase -> True], 
				StringLength[head] + 4 + 
				ToExpression[StringExtract[ToLowerCase[head], "content-length: " -> 2, "\r\n" -> 1]], 
			True, 
				Length[message]
		]
	]
]; 


(* ::Section:: *)
(*HTTPHandler*)


(* ::Section::Closed:: *)
(*Default message handler*)


CreateType[HTTPHandler, {
	"MessageHandler" -> <||>, 
	"DefaultMessageHandler" -> Function[<|"Code" -> 404, "Body" -> "NotFound"|>], 
	"Deserializer" -> <||>, 
	"DefaultDeserailizer" -> $deserializer
	"Serializer" -> <||>, 
	"DefaultSerializer" -> $serializer
	"Logger" -> None
}]; 


handler_HTTPHandler[packet_Association] := 
With[{message = packet["DataByteArray"]}, 
	Module[{request, response, pipeline, result, deserializer, serializer, messageHandler, defaultMessageHandler}, 
		deserializer = handler["Deserializer"]; 
		serializer = handler["Serializer"]; 
		messageHandler = handler["MessageHandler"]; 
		defaultMessageHandler = handler["DefaultMessageHandler"]; 
		
		(*Request: _Association*)
		request = parseRequest[message, deserializer]; 

		(*Result: _String | _Association*)
		result = ConditionApply[messageHandler, defaultMessageHandler][request]; 

		(*Result: HTTPResponse[]*)
		response = createResponse[result, serializer]; 

		(*Return: _String | ByteArray[]*)
		Which[
			StringQ @ response["Body"], ExportString[response, "HTTPResponse", CharacterEncoding -> "UTF-8"], 
			True, ExportByteArray[response, "HTTPResponse", CharacterEncoding -> "UTF-8"]
		]
	]
]; 


(* ::Section::Closed:: *)
(*Add HTTPHandler*)


HTTPHandler /: AddTo[tcp_, http_HTTPHandler] := (
	tcp["CompleteHandler", "HTTP"] = HTTPPacketQ -> HTTPPacketLength; 
	tcp["MessageHandler", "HTTP"] = HTTPPacketQ -> http; 
	tcp
); 


(* ::Section::Closed:: *)
(*Internal*)


$httpMethods = {"GET", "PUT", "DELETE", "HEAD", "POST", "CONNECT", "OPTIONS", "TRACE", "PATCH"}; 


$httpEndOfHead = StringToByteArray["\r\n\r\n"]; 


$errorResponse = <|"Code" -> 404, "Body" -> "Not found"|>; 


parseRequest[message_ByteArray, deserializer_] := 
Module[{request, headBytes, head, headers, body, bodyBytes}, 
	{headBytes, bodyBytes} = BytesSplit[message, $httpEndOfHead -> 1]; 
	head = ByteArrayToString[headBytes]; 
	
	request = First @ StringCases[
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

	request["Headers"] = Association[
		Map[Rule[#1, StringRiffle[{##2}, ":"]]& @@ Map[StringTrim]@StringSplit[#, ":"] &]@
		StringExtract[head, "\r\n\r\n" -> 1, "\r\n" -> 2 ;; ]
	]; 

	request["BodyByteArray"] = bodyBytes; 
	request["Body"] = ConditionApply[deserializer, HTTPDeserialize][request]; 

	(*Return: Association[
		Metod, 
		Path, 
		Query, 
		Version, 
		Headers, 
		BodyByteArray, 
		Body
	]*)
	request
]; 


createResponse[assoc_Association, serializer_] := 
Module[{data, body, headers, metadata}, 
	data = ConditionApply[serializer, HTTPSerialize][assoc]; 

	metadata = <|
		"ContentType" -> "text/html; charset=utf-8", 
		"Headers" -> <|
			"Content-Length" -> Which[
				AssociationQ[data] && KeyExistsQ[data, "Body"] && ByteArrayQ[data["Body"]], Length[data["Body"]], 
				AssociationQ[data] && KeyExistsQ[data, "Body"] && StringQ[data["Body"]], StringLength[data["Body"]], 
				StringQ[data], StringLength[data], 
				ByteArrayQ[data], Length[data]
			]
		|>, 
		"StatusCode" -> 200
	|>; 

	If[AssociationQ[data], 
		If[KeyExistsQ[data, "ContentType"], metadata["ContentType"] = data["ContentType"]]; 
		If[KeyExistsQ[data, "Headers"], metadata["Headers"] = data["Headers"] ~ Join ~ metadata["Headers"]]; 
		If[KeyExistsQ[data, "StatusCode"], metadata["StatusCode"] = data["StatusCode"]]; 
		If[KeyExistsQ[data, "Body"], body = data["Body"]]; 
	]; 

	If[StringQ[data] || ByteArrayQ[data], 
		body = data
	]; 

	(*Return: HTTPResponse[]*)
	HTTPResponse[body, metadata]
]; 


(* ::Section::Closed:: *)
(*Serialization*)


$deserializer[headers_Association, body_ByteArray] := 
body; 


$serializer[expr_] := 
ExportString[expr, "ExpressionJSON"]; 


$serializer[assoc_Association] := 
ExportString[assoc, "RawJSON"]; 


$serializer[list_List] := 
ExportString[list, "RawJSON"]; 


$serializer[image_Image] := 
ExportString[image, "PNG"]; 


$serializer[image_Graphics] := 
ExportString[image, "SVG"]; 


$serializer[text_String] := 
text; 

$serializer[bytes_ByteArray] := 
bytes


(* ::Section::Closed:: *)
(*End private context*)


End[(*`Private`*)]; 


(* ::Section::Closed:: *)
(*End packet*)


EndPackage[(*Kirill`HTTP`*)]; 
