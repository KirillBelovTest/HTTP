BeginPackage["KirillBelov`HTTPHandler`WSPAdapter`", {"WSP`", "KirillBelov`HTTPHandler`Extensions`"}]; 

HypertextProcess::usage = 
"HypertextProcess[request] import file as WSP and process it."; 

session::usage = "session[\"Method\"], session[\"Query\"] ... global access to a session variable inside each request"

Begin["`Private`"];

HypertextProcess[request_Association, OptionsPattern[]] := Module[{body},
With[{file = URLPathToFileName[request]},
    Block[{session = <||>, $publicpath = FileNameJoin[{Directory[], OptionValue["Base"]}]},
        session = request;
        body = LoadPage[file];

        (* handle special case for redirect *)

        If[KeyExistsQ[session, "Redirect"],
            <|  "Code"->303, "Body"->body, 
                "Headers"-> <|"Content-Type" -> GetMIMEType["html"], "Content-Length" -> StringLength[body], "Location" -> session["Redirect"]|>
            |> // Return
        ];

        body
    ]
]]

(* не придумал, как передать опции ;()

HypertextProcess[request_Association, OptionsPattern[]] :=
    With[{file = URLPathToFileName[request]},
        HypertextProcess[request, file, ...]
    ]

 *)

HypertextProcess[request_Association, filename_String, OptionsPattern[]] := Module[{body},
With[{file = filename},
    Block[{session = <||>, $publicpath = FileNameJoin[{Directory[], OptionValue["Base"]}]},
        session = request;
        body = LoadPage[file];

        (* handle special case for redirect *)

        If[KeyExistsQ[session, "Redirect"],
            <|  "Code"->303, "Body"->body, 
                "Headers"-> <|"Content-Type" -> GetMIMEType["html"], "Content-Length" -> StringLength[body], "Location" -> session["Redirect"]|>
            |> // Return
        ];

        body
    ]
]]

Options[HypertextProcess] = {"Base"->""}

End[]

EndPackage[]