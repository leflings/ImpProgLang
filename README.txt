If you need to update lexer/parser, use the following commands in Project "Properties" -> "Build Events" -> "Prebuild event command line":

fslex "$(ProjectDir)Lexer.fsl"
fsyacc --module Parser "$(ProjectDir)Parser.fsy"

If F# PowerPack is not in PATH environment variable, try to use the absolute paths

"C:\Program Files (x86)\FSharpPowerPack-4.0.0.0\bin\fslex" "$(ProjectDir)Lexer.fsl"
"C:\Program Files (x86)\FSharpPowerPack-4.0.0.0\bin\fsyacc" --module Parser "$(ProjectDir)Parser.fsy"
