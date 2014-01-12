module Program

open AST
open System
open ParserUtil
open Interpreter
open BasisEnv


// Create an initial environment

// Parse a program in a file  
//let fac = parseFromFile "Factorial2.while"
//
//// Interpret the program 
//let _  = stm fac initEnv Map.empty;;

//let ap1 = parseFromFile"ArrayProg1.while";; 
//let _ = ignore (stm ap1 basisEnv basisStore);;

//let ap2 = parseFromFile"ArrayProg2.while";; 
//let _ = ignore (stm ap2 basisEnv basisStore);;

//printfn ""
//
//let tc = parseFromFile "TryCatch.while";;
//let _ = ignore (stm tc basisEnv basisStore);;
//
//printfn ""
//
//let tf = parseFromFile "TryFinally.while";;
//let _ = ignore (stm tf basisEnv basisStore);;
//
//printfn ""
//
//let tcf = parseFromFile "TryCatchFinally.while";;
//let _ = ignore (stm tcf basisEnv basisStore);;

printfn "ArrayTest program:"
let at = parseFromFile "ArrayTest.while";;
let _ = ignore (stm at basisEnv basisStore);;

printfn "\nArrayOfProcedures program:"
let aop = parseFromFile "ArrayOfProcedures.while";;
let _ = ignore (stm aop basisEnv basisStore);;

let _ = System.Console.ReadLine() |> ignore;;