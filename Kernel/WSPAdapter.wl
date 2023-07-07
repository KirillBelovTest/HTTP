BeginPackage["KirillBelov`HTTPHandler`WSPAdapter`", {"JerryI`WSP`", "KirillBelov`HTTPHandler`Extensions`"}]; 

HypertextProcess::usage = 
"HypertextProcess[request] import file as WSP and process it."; 

Begin["`Private`"];

HypertextProcess[request_Association, OptionsPattern[]] := Module[{body},
With[{file = URLPathToFileName[request]},

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

(* не придумал, как передать опции ;()

HypertextProcess[request_Association, opts: OptionsPattern[]] :=
    With[{file = URLPathToFileName[request]},
        HypertextProcess[request, file, ..., opts]
    ]

 *)

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

Options[HypertextProcess] = {"Base"->""}

End[]

EndPackage[]