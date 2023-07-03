BeginPackage["KirillBelov`HTTPHandler`WSPAdapter`", {"JerryI`WSP`", "KirillBelov`HTTPHandler`Extensions`"}]; 

HypertextProcess::usage = 
"HypertextProcess[request] import file as WSP and process it."; 

Begin["`Private`"];

HypertextProcess[request_Association, OptionsPattern[]] := Module[{body},
With[{file = URLPathToFileName[request]},

    Block[{Global`session = <||>},
        Global`session = request;
        body = LoadPage[file, {}, "Base"->FileNameJoin[{Directory[], OptionValue["Base"]}]];

        (* handle special case for redirect *)

        If[KeyExistsQ[Global`session, "Redirect"],
            Print["Redirecting to "<>Global`session["Redirect"]];
            <|  "Code"->201, "Body"->body, 
                "Headers"-> <|"Content-Location" -> Global`session["Redirect"], "Content-Length" -> StringLength[body]|>
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
    Block[{Global`session = <||>},
        Global`session = request;
        body = LoadPage[file, {}, "Base"->FileNameJoin[{Directory[], OptionValue["Base"]}]];

        (* handle special case for redirect *)

        If[KeyExistsQ[Global`session, "Redirect"],
            Print["Redirecting to "<>Global`session["Redirect"]];
            <|  "Code"->201, "Body"->body, 
                "Headers"-> <|"Content-Location" -> Global`session["Redirect"], "Content-Length" -> StringLength[body]|>
            |> // Return
        ];

        body
    ]
]]

Options[HypertextProcess] = {"Base"->""}

End[]

EndPackage[]