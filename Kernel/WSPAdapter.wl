BeginPackage["KirillBelov`HTTPHandler`WSPAdapter`", {"WSP`", "KirillBelov`HTTPHandler`Extensions`"}]; 

HypertextProcess::usage = 
"HypertextProcess[request] import file as WSP and process it."; 

session::usage = "internal"

Begin["`Private`"];

HypertextProcess[request_Association, OptionsPattern[]] := 
With[{file = URLPathToFileName[request]},
    Block[{session = <||>, $publicpath = FileNameJoin[{Directory[], OptionValue["Base"]}]},
        session = request;
        LoadPage[file]
    ]
]

HypertextProcess[request_Association, filename_String, OptionsPattern[]] := 
With[{file = filename},
    Block[{session = <||>, $publicpath = FileNameJoin[{Directory[], OptionValue["Base"]}]},
        session = request;
        LoadPage[file]
    ]
]

Options[HypertextProcess] = {"Base"->""}

End[]

EndPackage[]