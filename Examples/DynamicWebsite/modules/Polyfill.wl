(* polyfills from frontend *)
NotebookPromise[uid_, params_][expr_] := With[{},

    WebSocketSend[Global`client, ExportByteArray[Global`PromiseResolve[uid, expr], "ExpressionJSON"]];
    
];

NotebookAddTracking[symbol_] := With[{cli = Global`client, name = SymbolName[Unevaluated[symbol]]},
    Print["Add tracking... for "<>name];
    Experimental`ValueFunction[Unevaluated[symbol]] = Function[{y,x}, WebSocketSend[cli, ExportByteArray[FrontUpdateSymbol[name, x], "ExpressionJSON"]]]
]

SetAttributes[NotebookAddTracking, HoldFirst]


NotebookPromiseKernel[uid_, params_][expr_] := With[{cli = Global`client},
    With[{result = expr // ReleaseHold},
        Print["side evaluating on the Kernel"];
        WebSocketSend[cli, ExportByteArray[Global`PromiseResolve[uid, result], "ExpressionJSON"]];
        
    ]
];

NotebookEmitt[expr_] := ReleaseHold[expr]

(* polyfills from frontend *)
FrontSubmit[expr_] := WebSocketBroadcast[ExportByteArray[expr, "ExpressionJSON"]];