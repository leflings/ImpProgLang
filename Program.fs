module Program

open AST
open System
open ParserUtil
open Interpreter
open BasisEnv

let run s = printfn "%s program:" s
            printfn "-----------"
            stm (parseFromFile (sprintf "%s.while" s)) basisEnv basisStore |> ignore
            printfn "-----------\n"
            ()

let _ = run "Factorial1"
let _ = run "Factorial2"
let _ = run "Factorial3"
let _ = run "Factorial4"
let _ = run "Factorial5"
//let _ = run "ArrayUtil"
let _ = run "ArrayProg1"
let _ = run "ArrayProg2"
let _ = run "ArrayProg2"
let _ = run "ArrayOfProcedures"
let _ = run "ArrayTest"
let _ = run "ProcedurePass"
let _ = run "ProcedureReturn"
let _ = run "TryCatch"
let _ = run "TryCatchFinally"
let _ = run "TryFinally"
let _ = run "ProcedureArguments"


let _ = System.Console.ReadLine() |> ignore;;