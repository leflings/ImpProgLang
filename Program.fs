﻿module Program

open AST
open System
open ParserUtil
open Interpreter


// Create an initial environment

let plusInt = Primitive( fun [IntVal i1; IntVal i2] -> IntVal(i1+i2) );;
let minusInt = Primitive( fun [IntVal i1; IntVal i2] -> IntVal(i1-i2) );;
let multInt = Primitive( fun [IntVal i1; IntVal i2] -> IntVal(i1*i2) );;
let eqInt = Primitive( fun [IntVal i1; IntVal i2] -> BoolVal(i1=i2) );;
let neqInt = Primitive( fun [IntVal i1; IntVal i2] -> BoolVal(i1<>i2) );;
let lessEqInt = Primitive( fun [IntVal i1; IntVal i2] -> BoolVal(i1<=i2) );;
let lessInt = Primitive( fun [IntVal i1; IntVal i2] -> BoolVal(i1<i2) );;
let gen = let generator = new System.Random()
          generator.Next;;   
let randomInt = Primitive( fun [IntVal rng] -> IntVal (gen rng) );; 
let toString = let f vs =  match vs with 
                           | [IntVal v] -> StringVal(string v)
                           | [BoolVal v] -> StringVal(string v)
                           | [StringVal v] -> StringVal v
                           | _          -> failwith "error"
               Primitive f;;

let initEnv = Map.ofList [("+",plusInt); ("-",minusInt); ("*",multInt); ("=",eqInt); ("<>",neqInt); ("<=",lessEqInt); ("<",lessInt); 
                           ("randomInt", randomInt); ("toString",toString)  ];;


// Parse a program in a file  
//let fac = parseFromFile "Factorial2.while"
//
//// Interpret the program 
//let _  = stm fac initEnv Map.empty;;


let arrayUtilDecs = parseDecListFromFile "ArrayUtil.while";;

let (basisEnv, basisStore) = decList arrayUtilDecs initEnv Map.empty;; 

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

let at = parseFromFile "ArrayTest.while";;
let _ = ignore (stm at basisEnv basisStore);;

let _ = System.Console.ReadLine() |> ignore;;