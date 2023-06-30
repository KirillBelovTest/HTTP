(* polyfills from frontend *)
NotebookPromise[uid_, params_][expr_] := With[{},
    WebSocketSend[Global`client, Global`PromiseResolve[uid, expr]];
];

NotebookPromiseKernel[uid_, params_][expr_] := With[{cli = Global`client},
    With[{result = expr // ReleaseHold},
        Print["side evaluating on the Kernel"];
        WebSocketSend[cli, Global`PromiseResolve[uid, result]] 
    ]
];

NotebookEmitt[expr_] := ReleaseHold[expr]

(* polyfills from frontend *)
FrontSubmit[expr_] := WebSocketBroadcast[server, expr];