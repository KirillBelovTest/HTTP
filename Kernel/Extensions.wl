(* ::Package:: *)

(* ::Chapter:: *)
(*Extensions*)


BeginPackage["KirillBelov`HTTPHandler`Extensions`", {
    "KirillBelov`Internal`", 
    "KirillBelov`HTTPHandler`", 
    "KirillBelov`TCPServer`", 
    "JerryI`WSP`"
}]; 


AddHTTPHandler::usage = 
"AddHTTPHandler[tcp, http] adds HTTP Handler to TCP Server."; 


HypertextProcess::usage = 
"HypertextProcess[request] import file as WSP and process it."; 


GetFileRequestQ::usage = 
"GetFileRequestQ[fileType] returns function for checking that is a Get request and path contatins file."; 


URLPathToFileName::usage = 
"URLPathToFileName[request] to file path."; 


FileNameToURLPath::usage = 
"FileNameToURLPath[file] to url."; 


ImportFile::usage = 
"ImportFileAsText[request] import file from request as text data."; 


GetMIMEType::usage = 
"GetMIMEType[request] returns mime type based on a file extension."; 


GetPOSTRequestQ::usage = 
"GetPOSTRequestQ[fileType] returns function for checking that is a Post request and path contatins file."; 


ProcessMultipart::usage = 
"ProcessMultipart[request] returns processed request with a POST data decrypted."; 


Begin["`Private`"]; 


AddHTTPHandler[tcp_TCPServer, key_String: "HTTP", http_HTTPHandler] := (
    tcp["CompleteHandler", key] = HTTPPacketQ -> HTTPPacketLength; 
    tcp["MessageHandler", key] = HTTPPacketQ -> http; 
);


GetFileRequestQ[fileType: _String | {__String} | _StringExpression] := 
AssocMatchQ[<|"Method" -> "GET", "Path" -> "/" ~~ ___ ~~ "." ~~ fileType|>]; 

GetPOSTRequestQ[fileType: _String | {__String} | _StringExpression] := 
AssocMatchQ[<|"Method" -> "POST", "Path" -> "/" ~~ ___ ~~ "." ~~ fileType|>];

URLPathToFileName[urlPath_String] := 
FileNameJoin[FileNameSplit[StringTrim[urlPath, "/"]]]; 


URLPathToFileName[request_Association] := 
URLPathToFileName[request["Path"]]; 


FileNameToURLPath[fileName_String] := 
URLBuild[FileNameSplit[StringTrim[fileName, StartOfString ~~ Directory[]]]]; 


Options[ImportFileAsText] = {"Base" :> {Directory[]}}


ImportFileAsText[file_String] := 
If[FileExistsQ[file], 
    With[{body = Import[file, "String"]},
        <|
            "Body" -> body, 
            "Code" -> 200, 
            "Headers" -> <|
                "Content-Type" -> GetMIMEType[file], 
                "Content-Length" -> StringLength[body], 
                "Connection"-> "Keep-Alive", 
                "Keep-Alive" -> "timeout=5, max=1000", 
                "Cache-Control" -> "max-age=60480"
            |>
        |> 
    ], 

        (*Else*)
            <|"Code" -> 404|>
        ]
    ]
]; 


ImportFile[request_Association, opts: OptionsPattern[]] := 
ImportFile[URLPathToFileName[request["Path"]], opts]


ImportFileAsText[request_Association, OptionsPattern[]] := (
ImportFileAsText[OptionValue["Base"], URLPathToFileName[request["Path"]]])

ImportFileAsText[name_String] := 
ImportFileAsText[URLPathToFileName[name], ""]


MIMETypes = Uncompress["1:eJytWFlv3DYQTlsH6N0kva+HPhdaxw1co30r0AMF+hT+gIJLURJtUWRIrsT6J/\
RXd0iudzfirKS1+2Lt0B+HM8M5+d1avaz+\
ffTokT2DP38J66o376iXm5aTQFFBPgsfrVvBqBOqO9fKOsuM0GP8WxFfkffDd1MKde4LoK\
sR7CzB2ELcLL94rGXkXfg67t25bqnoUFBFPoRvL0oeeElbwBIK1OTtO25AoBA/\
zysacEPe28m/\
plYwjFsvyDs7bkAhmLXUESMkrfk5UBjGJkwUvHGyxTAuWSHx0cau1k5gtg/\
L88g34MeM7QPFFtwPq+WBhkBhGK0XMLJ2f39AIJCSmcyxfVEKw5lTBtvADfkq2+Avn/\
9UMFowbrBoKBUjz0abpB2UKRF7A1iS5yN035Ur8K2wZQWAjeSdW0nKjOIdXbe8XF38gPPy\
5A+El9K88+AXykjqbKGqSjC+YxyO0UYxbq3oatnujkR1c6fo5qZ1c1zCbTq+SDf3v+\
h2dySmmyv3DuTRMCohZJ/mDoSGLtf2tCxaaU8+2EVC0A1WEFwNEbqPmDoLzkDVt9k1+aK+\
FeP8EQK5mQ/kZgYTxGpaTT4enTmIruEtlrSaJSENyWwys50lTDub/\
gRTByYDCsXYaP6UP2jLu5JiOUHwQ/\
MDhQglam6jelKVvD0PJMapXgLqxLyhrqkhX4xsf017WlDDGtFj3n6teeSb1ACqRvQIy9Oo\
xGoGFKhrm0noiyDj0Xi4nqlrj+FHCGWfuZ0v4jqikOS2ObB5IJGTpSjjyal+\
A4VxEqWYBCVGFflonC6zgI1I1UdkagZebQS7cUJiFyf1i6jB9lz8SuT2dhM7BHSWQPU0Kr\
Gq4s3Fa9hmb8lLQTVlN3Dh6JZFfHVWW7fsIWNfQ0lGN/\
l7bFpy56qk2VXBGoLUVzJP7DfMXhUSvzF9ZcnX6AYr6o66jUF3lbnvwBoSWrpFkr3mBsuE\
uqsPshdQiF/oULd/PGJlNXCjlYDaekL11qF6/4ZwnK7e2nAL3wifrt1aU/\
LtnMSYYJpK8mJWVVqWopvRM4lh7ymGleRqVgzbQkDZRg1LTK5hWPn9oSbfnYgq6+\
6pLLjXz7PKHkqySF9wsT8fqu8hiap8e7LKMUJPaAUD9cotKAaBMpR8sqsFEPRdYThtI4mI\
b8DZT4HX6zjGplzhC6AxEU7j6fKsBmso0u/7MSNYE34gOEs1+\
TJLf7AKbmsc26B7YOj6PN/DDOcdo+\
MRNPywolXzRcQKifoHSPP3zbASboPPmVY48mkuj9uAs2L+YdtoxF1JHqiGUMUYd+\
WClwgLFSQfc6tNqEzwP4rr2tcH4wpQ3+\
Mzkx3oaUO3HarMiGCMRrGbgfa8qI4I5KAJzoczhzbxoWtEsFnPGLGN23fg7sgYEtanUY+\
3qHYaFk+EtnHfUDv8lSwsT6MiK1tlffc2Y3HPOHq6d/MTR8/\
SE10aloue0ewJIMEsefIa7Ohc1Zfq4DkMKAwj+\
oMHuKAHrIyB4UdvtnZO4RpIjJ0ts7BLTC2avMD9DqLJF0AjZw/h0e7pODwH/\
PFukPn9hIdFyR2tRIv1OgOo9iRjj3olrKbKM8YeHboGfZmlgviIwk0FAXu5usA2mQXG9pC\
nkbAWbQ+dqqgbV8B/sH1rGZPdXRXya+\
EkRZGvPV0GbT0eOr6FQriaiIrFDZ9v87F2IrzS4XZNLqcOtw3nbrUWHTX/\
LOh8gOGMNonhIk6e/\
IJwmu6hLHRNtIyHyO1hKHPIkBdTYp4wUAAzT359oKQT8wSwmHsL9Hb2udDbWS63QpNvsqi\
A1YIpGbpRy7G8OozfRv4DUoPZxg=="]//Association


GetMIMEType[file_String] := Module[{type},
    type = MIMETypes[file // FileExtension];
	If[!StringQ[type], type = "application/octet-stream"];
    type
]; 


$directory = 
DirectoryName[$InputFileName];


$mimeTypes = 
Get[FileNameJoin[{$directory, "MIMETypes.wl"}]];


GetMIMEType[request_Association] := 
GetMIMEType[URLPathToFileName[request["Path"]]]; 


$DeserializeUrlencoded[request_Association, body_ByteArray] := (
    <|<|URLParse["/?" <> (body//ByteArrayToString)]|>["Query"]|>
); 


DecryptField[line_String] := 
 Module[{stream = StringToStream[line], header, body}, 
  (* Convert a large string to a stream due to performance isssues *)

    With[{extracted = StringExtract[
      (* Read only the header *)
      ReadString[stream, "\r\n\r\n"],
       "\r\n\r\n" -> 1, "\r\n" -> 2 ;;]},
   
   (* Extract parameters *)
   
   header = (Association[
      Map[
        Rule[#1, StringRiffle[{##2}, ":"]] & @@ 
          Map[StringTrim]@StringSplit[#, ":"] &]@extracted]);
   
   header["Content-Disposition"] = 
    Join @@ (Map[
        StringCases[#, 
          RegularExpression["(\\w*)=\"(.*)\""] :> <|
            "$1" -> "$2"|>] &, (StringTrim /@ 
          StringSplit[header["Content-Disposition"], ";"][[2 ;;]])] //
        Flatten);
   
   (* Skip gaps *)
   ReadString[stream, "\n"];
   ReadString[stream, "\n"];
   
   (* The rest is our payload *)
   
   body = ReadString[stream, EndOfFile];

   (* remove \r\n in the beginng and in the end *)
   body = StringDrop[StringDrop[body, -4], 1];

   Close[stream];
   
   <|"Body" -> body, "Headers" -> header|>
]]; 


$DeserializeMultipart[request_Association, body_ByteArray] := Module[{boundary, stream, buffer, field, data},
    boundary = StringCases[request["Headers"]["Content-Type"], RegularExpression["boundary=(.*)"] :> "$1"] // First;

    stream = StringToStream[body//ByteArrayToString];
    buffer = {};

    field = ReadString[stream, boundary];
    
    While[StringQ[field = ReadString[stream, boundary]],
        If[StringTake[field, 2] === "--", Break[]];
        buffer = {buffer, field // DecryptField};
    ];

    buffer = buffer // Flatten;

    (* Association, where all fields will be stored *)
    data = <||>;

    Function[assoc, 
        
        If[KeyExistsQ[assoc["Headers"], "Content-Type"],
        (* 90% probabillity that this is a file *)
        
            If[!KeyExistsQ[data, assoc["Headers", "Content-Disposition", "name"]],
                data[assoc["Headers", "Content-Disposition", "name"]] = <||>
            ];

            (* name -> filename -> data *)
            data[assoc["Headers", "Content-Disposition", "name"]][assoc["Headers", "Content-Disposition", "filename"]] = assoc["Body"];
        ,
        (* 90% probabillity that this is a regualar field *)

            (* name -> value *)
            data[assoc["Headers", "Content-Disposition", "name"]] = assoc["Body"]
        ]
    ] /@ buffer;

    data
]; 


ProcessMultipart[request_Association, OptionsPattern[]] := Module[{},
    (* Return request_Association *)

    If[StringMatchQ[request["Headers"]["Content-Type"], "application/x-www-form-urlencoded" ~~ ___],
        Print["URL Encoded detected"];
        Return[Join[request, <|"Data"->$DeserializeUrlencoded[request, request["Body"]], "Body"->{}|>]]
    ];

    If[StringMatchQ[request["Headers"]["Content-Type"], "multipart/form-data" ~~ ___],
        Print["Multipart detected"];
        Return[Join[request, <|"Data"->$DeserializeMultipart[request, request["Body"]], "Body"->{}|>]]
    ];

]; 


Options[HypertextProcess] = {
    "Base" :> {Directory[]}
}; 


HypertextProcess[request_Association, OptionsPattern[]] := Module[{body},
With[{file = URLPathToFileName[request]}, 
    Block[{$CurrentRequest = request},
        body = LoadPage[file, {}, "Base" -> First@Flatten@{OptionValue["Base"]}];

        (* handle special case for redirect *)

        If[KeyExistsQ[$CurrentRequest, "Redirect"],
            Print["Redirecting to "<>Global`$CurrentRequest["Redirect"]];
            <|  "Code"->201, "Body"->body, 
                "Headers"-> <|"Content-Location" -> Global`$CurrentRequest["Redirect"], "Content-Length" -> StringLength[body]|>
            |> // Return
        ];

        body
    ]
]]


HypertextProcess[request_Association, filename_String, OptionsPattern[]] := Module[{body},
With[{file = filename},
    Block[{Global`$CurrentRequest = <||>},
        Global`$CurrentRequest = request;
        body = LoadPage[file, {}, "Base"->First@Flatten@{OptionValue["Base"]}];

        (* handle special case for redirect *)

        If[KeyExistsQ[Global`$CurrentRequest, "Redirect"],
            Print["Redirecting to "<>Global`$CurrentRequest["Redirect"]];
            <|  "Code"->201, "Body"->body, 
                "Headers"-> <|"Content-Location" -> Global`$CurrentRequest["Redirect"], "Content-Length" -> StringLength[body]|>
            |> // Return
        ];

        body
    ]
]]


End[]; 


EndPackage[]; 