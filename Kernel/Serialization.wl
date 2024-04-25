(* ::Package:: *)

BeginPackage["KirillBelov`HTTPHandler`Serialization`", {
    "KirillBelov`Internal`"
}]; 


HTTPSerialize::usage = 
"HTTPSerialize[expr] serialize expr to byte array."; 


HTTPDeserialize::usage = 
"HTTPDeserialize[request] deserialize byte array to expr."; 


$HTTPMIMETypes::usage = 
"MIME types."; 


$HTTPDeserializers::usage = 
"List of HTTP deserializers."; 


Begin["`Private`"]; 


HTTPDeserialize[request_Association?AssociationQ] := 
Module[{contentType, position, format}, 
    With[{bytes = request["BodyByteArray"], headers = request["Headers"]}, 
        If[Length[bytes] === 0, Return[bytes]]; 
        
        contentType = getContentType[headers]; 
        
        If[MissingQ[contentType], Return[bytes]]; 

        contentDeserialize[bytes, contentType]
    ]
]; 


$HTTPDeserializers = <|
    "application/x-www-form-urlencoded" -> formUrlEncodedQ -> formUrlEncodedDeseralize, 
    "application/json" -> jsonQ -> jsonDeserialize, 
    "multipart/form-data" -> multipartFormDataQ -> multipartFormDataDeserialize
|>; 


$MIMETypes = <|
    "ai" -> "application/postscript",
    "aif" -> "audio/x-aiff", 
    "aifc" -> "audio/x-aiff",
    "aiff" -> "audio/x-aiff",
    "asc" -> "text/plain",
    "asf" -> "video/x-ms-asf",
    "asp" -> "text/asp",
    "asx" -> "video/x-ms-asf",
    "au" -> "audio/basic",
    "avi" -> "video/avi",
    "bmp" -> "image/bmp",
    "bsp" -> "text/html",
    "btf" -> "image/prs.btif",
    "btif" -> "image/prs.btif",
    "c" -> "text/plain",
    "cc" -> "text/plain",
    "cgm" -> "image/cgm",
    "cpp" -> "text/plain",
    "css" -> "text/css",
    "dcr" -> "application/x-director",
    "der" -> "application/x-x509-ca-cert",
    "doc" -> "application/msword",
    "docm" -> "application/vnd.ms-word.document.macroenabled.12",
    "docx" -> "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    "dot" -> "application/msword",
    "dotm" -> "application/vnd.ms-word.template.macroenabled.12",
    "dotx" -> "application/vnd.openxmlformats-officedocument.wordprocessingml.template",
    "dtd" -> "text/xml",
    "dvi" -> "application/x-dvi",
    "eps" -> "application/postscript",
    "fpx" -> "image/vnd.fpx",
    "gif" -> "image/gif",
    "gz" -> "application/x-gzip",
    "h" -> "text/plain",
    "hh" -> "text/plain",
    "hlp" -> "application/winhelp",
    "hpp" -> "text/plain",
    "htm" -> "text/html",
    "html" -> "text/html",
    "ico" -> "image/ico",
    "ics" -> "text/calendar",
    "ief" -> "image/ief",
    "iges" -> "model/iges",
    "igs" -> "model/iges",
    "ini" -> "text/plain",
    "jar" -> "application/java-archive",
    "jpe" -> "image/jpeg",
    "jpeg" -> "image/jpeg",
    "jpg" -> "image/jpeg",
    "js" -> "application/x-javascript",
    "jsp" -> "text/html",
    "latex" -> "application/x-latex",
    "mesh" -> "model/mesh",
    "mid" -> "audio/mid",
    "midi" -> "audio/mid",
    "mif" -> "application/mif",
    "mov" -> "video/quicktime",
    "mp3" -> "audio/mpeg",
    "mpe" -> "video/mpeg",
    "mpeg" -> "video/mpeg",
    "mpf" -> "text/vnd.ms-mediapackage",
    "mpg" -> "video/mpeg",
    "mpp" -> "application/vnd.ms-project",
    "mpx" -> "application/vnd.ms-project",
    "msh" -> "model/mesh",
    "oda" -> "application/oda",
    "p7m" -> "application/pkcs7-mime",
    "p7s" -> "application/pkcs7-signature",
    "pdf" -> "application/pdf",
    "pl" -> "application/x-perl",
    "png" -> "image/png",
    "potm" -> "application/vnd.ms-powerpoint.template.macroenabled.12",
    "potx" -> "application/vnd.openxmlformats-officedocument.presentationml.template",
    "ppa" -> "application/vnd.ms-powerpoint",
    "ppam" -> "application/vnd.ms-powerpoint.addin.macroenabled.12",
    "pps" -> "application/vnd.ms-powerpoint",
    "ppsm" -> "application/vnd.ms-powerpoint.slideshow.macroenabled.12",
    "ppsx" -> "application/vnd.openxmlformats-officedocument.presentationml.slideshow",
    "ppt" -> "application/vnd.ms-powerpoint",
    "pptm" -> "application/vnd.ms-powerpoint.presentation.macroenabled.12",
    "pptx" -> "application/vnd.openxmlformats-officedocument.presentationml.presentation",
    "ppz" -> "application/vnd.ms-powerpoint",
    "ps" -> "application/postscript",
    "qt" -> "video/quicktime",
    "ra" -> "audio/x-pn-realaudio",
    "ram" -> "audio/x-pn-realaudio",
    "rgb" -> "image/x-rgb",
    "rm" -> "audio/x-pn-realaudio",
    "rtf" -> "application/rtf",
    "rtx" -> "text/richtext",
    "sap" -> "application/x-sapshortcut",
    "scm" -> "application/x-screencam",
    "silo" -> "model/mesh",
    "sim" -> "application/vnd.sap_kw.itutor",
    "sit" -> "application/x-stuffit",
    "sl" -> "text/vnd.wap.sl",
    "snd" -> "audio/basic",
    "spl" -> "application/x-futuresplash",
    "svg" -> "image/svg+xml",
    "swa" -> "application/x-director",
    "swf" -> "application/x-shockwave-flash",
    "tar" -> "application/x-tar",
    "tex" -> "application/x-tex",
    "tht" -> "text/thtml",
    "thtm" -> "text/thtml",
    "thtml" -> "text/thtml",
    "tif" -> "image/tiff",
    "tiff" -> "image/tiff",
    "tsf" -> "application/vnd.ms-excel",
    "txt" -> "text/plain",
    "vcf" -> "text/x-vcard",
    "vcs" -> "text/x-vcalendar",
    "vdo" -> "video/vdo",
    "viv" -> "video/vnd.vivo",
    "vrml" -> "model/vrml",
    "vsd" -> "application/vnd.visio",
    "wav" -> "audio/x-wav",
    "wbmp" -> "text/vnd.wap.wbmp",
    "wmf" -> "application/x-msmetafile",
    "wml" -> "text/vnd.wap.wml",
    "wmls" -> "text/vnd.wap.wmlscript",
    "wp5" -> "application/wordperfect5.1",
    "wrl" -> "model/vrml",
    "xap" -> "application/x-silverlight-app",
    "xbm" -> "image/x-xbitmap",
    "xif" -> "image/vnd.xiff",
    "xlam" -> "application/vnd.ms-excel.addin.macroenabled.12",
    "xls" -> "application/vnd.ms-excel",
    "xlsb" -> "application/vnd.ms-excel.sheet.binary.macroenabled.12",
    "xlsm" -> "application/vnd.ms-excel.sheet.macroenabled.12",
    "xlsx" -> "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    "xltm" -> "application/vnd.ms-excel.template.macroenabled.12",
    "xltx" -> "application/vnd.openxmlformats-officedocument.spreadsheetml.template",
    "xml" -> "text/xml",
    "xsd" -> "text/xml",
    "xsl" -> "text/xml",
    "zip" -> "application/x-zip-compressed",
    "wsp" -> "text/html"
|>; 


HTTPSerialize[file_File] := 
Module[{extension, contentType, contentLength}, 
    extension = ToLowerCase[FileExtension[file]]; 
    contentType = $MIMETypes[extension]; 

    <|
        "Headers" -> <|
            "Content-Type" -> contentType
        |>, 
        "Body" -> ReadByteArray[file]
    |>
]; 


HTTPSerialize[body_String] := 
StringToByteArray[body]; 


HTTPSerialize[body_ByteArray] := 
body; 


HTTPSerialize[data_Association] /; 
KeyExistsQ[data, "Body"] := 
Module[{$data = data}, 
    $data["Body"] = HTTPSerialize[$data["Body"]]; 
    $data
]; 


getContentType[headers_Association?AssociationQ] := 
With[{key = SelectFirst[Keys[headers], StringMatchQ[#, "Content-Type", IgnoreCase -> True]&]}, 
    If[MissingQ[key], key, 
        <|
            "ContentType" -> headers[key], (**)
            "MIMEType" -> StringSplit[headers[key],";"][[1]], 
            "MediaType" -> StringSplit[headers[key],"/"][[1]], 
            "Subtype" -> StringSplit[StringSplit[headers[key],"/"][[2]],";"][[1]], 
            "Charset" -> If[# === {}, None, #[[1]]]& @ StringCases[headers[key], Shortest["; charset=" ~~ $c__ ~~ {";", EndOfLine}] :> $c, 1],
            "Boundary" -> If[# === {}, None, #[[1]]]& @ StringCases[headers[key], Shortest["; boundary=" ~~ $b__ ~~ {";", EndOfLine}] :> $b, 1]
        |>
    ]
]; 


contentDeserialize[bytes_ByteArray, contentType_Association?AssociationQ] := 
SelectFirst[$HTTPDeserializers, #[[1]][contentType]&][[2]][bytes, contentType]; 


formUrlEncodedQ[contentType_Association] := 
StringMatchQ[contentType["MIMEType"], "application/x-www-form-urlencoded", IgnoreCase -> True]; 


formUrlEncodedDeseralize[bytes_ByteArray, _] := 
URLQueryDecode[ByteArrayToString[bytes]]; 


jsonQ[contentType_Association] := 
StringMatchQ[contentType["MIMEType"], "application/json", IgnoreCase -> True]; 


jsonDeserialize[bytes_ByteArray, _] := 
ImportByteArray[bytes, "RawJSON", CharacterEncoding -> "UTF-8"]; 


multipartFormDataQ[contentType_Association] := 
StringMatchQ[contentType["MIMEType"], "multipart/form-data", IgnoreCase -> True]; 


multipartFormDataDeserialize[bytes_ByteArray, contentType_Association] := 
With[{boundary = StringToByteArray[contentType["Boundary"]]}, 
    Module[{contentParts, from = 0, to = 0, part}, 
        contentParts = Reap[Do[
            If[
                bytes[[i ;; i - 1 + Length[boundary]]] === boundary, 
                If[from =!= 0 && to === 0, 
                    to = i - 3
                ]; 
                If[from === 0 && to === 0, 
                    from = i + Length[boundary]
                ]; 
                If[to > 0, 
                    to = i - 3; 
                    part = Sow[bytes[[from ;; to]]]; 
                    from = i + Length[boundary]
                ]
            ], 
            {i, 1, Length[bytes] - Length[boundary]}
        ]][[-1, -1]]; 

        (*Return: <|parts|>*)
        partDataDeserialize /@ contentParts
    ]
]; 


partDataDeserialize[part_ByteArray] := 
Module[{head, data, headers, result = <||>}, 
    {head, data} = BytesSplit[part, StringToByteArray["\r\n\r"] -> 1]; 
    result["Data"] = data; 
    headers = 
        Association @ 
        KeyValueMap[StringReplace[#, "-" -> ""] -> #2&] @ 
        Association @ 
        Map[Rule @@ StringSplit[#, ": "]&] @ 
        Map[StringTrim] @ 
        StringSplit[StringTrim[ByteArrayToString[head]], EndOfLine]; 
    result = Join[
        headers, 
        result, 
        With[{import = dataImporter[headers], $data = data}, 
            <|"Expression" :> import[$data]|>
        ]
    ]
]; 


dataImporter[_] := 
Identity; 


dataImporter[headers_Association?AssociationQ] /; 
KeyExistsQ[headers, "ContentType"] && 
KeyExistsQ[$HTTPMultipartFormats, headers["ContentType"]] := 
With[{format = getFormat[headers]}, 
    If[StringQ[format], ImportByteArray[#, format], format[#]]&
]; 


getFormat[headers_Association] := 
$HTTPMultipartFormats[headers["ContentType"]]; 


$HTTPMultipartFormats = <|
    "text/plain" -> "String", 
    "text/html" -> "String", 
    "image/png" -> "PNG", 
    "image/jpeg" -> "JPG", 
    "text/csv" -> Function[Normal[ImportByteArray[#, "Dataset", HeaderLines -> 1]]]
|>; 


End[(*`Private`*)]; 


EndPackage[(*KirillBelov`HTTPHandler`Serialization`*)]; 
