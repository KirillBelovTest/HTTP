plotData = {{0,0}};

EventBind["DroppedFile", Function[file,
    Print[BaseDecode[file["data"]]//ByteArrayToString];
    plotData = Drop[ImportString[file["data"] // BaseDecode // ByteArrayToString, "TSV"], 3];
]]