// Michael R. Hansen 03-01-2014

(* Load the parser and interpreter *)
#r "FSharp.PowerPack.dll"

#load "AST.fs"
#load "Parser.fs"
#load "Lexer.fs"
#load "ParserUtil.fs"
#load "Interpreter.fs"
#load "BasisEnv.fs"

open System
open Interpreter
open AST
open ParserUtil
open BasisEnv

// Parsing strings
let s1 = parseStm "while <>(!n,0)
                   do y := *(!n,!y);
                      n := -(!n,1)
                   od";;

let s2 = parseStm "let n: 4; y: 1
                   in while <>(!n,0)
                      do y := *(!n,!y);
                         n := -(!n,1)
                      od
                   end";;

// Parsing from files
// Set current directory
System.IO.Directory.SetCurrentDirectory __SOURCE_DIRECTORY__;;
//
let p3 = parseFromFile "Factorial1.while";;
// Interpret the statement
let _ = ignore (stm p3 initEnv Map.empty);;

let p4 = parseFromFile "Factorial2.while";;
let _ = ignore (stm p4 initEnv Map.empty);;

let p5 = parseFromFile "Factorial3.while";;
let _ = ignore (stm p5 initEnv Map.empty);;

let p6 = parseFromFile "Factorial4.while";;
let _ = ignore (stm p6 initEnv Map.empty);;

let p7 = parseFromFile "Factorial5.while";;
let _ = ignore (stm p7 initEnv Map.empty);;


// Parsing and interpreting programs with arrays
let randomArray = parseDec "proc randomArray(rng, lng) 
                               let a[!lng]: 0;
                                   i: 0
                               in while <(!i,a.length)
                               do a[!i] := randomInt(rng);
                                  i    := +(!i,1)
                               od;
                               return a
                               end";;

// Auxiliary procedures on arrays are in the file "ArrayUtil.while"
// They are used to built up a basic environment and a basic store
let arrayUtilDecs = parseDecListFromFile "ArrayUtil.while";;

let (basisEnv, basisStore) = decList arrayUtilDecs initEnv Map.empty;; 

let ap1 = parseFromFile"ArrayProg1.while";; 
let _ = ignore (stm ap1 basisEnv basisStore);;

let ap2 = parseFromFile"ArrayProg2.while";; 
let _ = ignore (stm ap2 basisEnv basisStore);;

// OWN TEST CASES
let aop = parseFromFile "ArrayOfProcedures.while";;
let _ = ignore (stm aop  basisEnv basisStore);;

let at = parseFromFile "ArrayTest.while";;
let _ = ignore (stm at  basisEnv basisStore);;

let pp = parseFromFile "ProcedurePass.while";;
let _ = ignore (stm pp  basisEnv basisStore);;

let pr = parseFromFile "ProcedureReturn.while";;
let _ = ignore (stm pr  basisEnv basisStore);;

let tc = parseFromFile "TryCatch.while";;
let _ = ignore (stm tc  basisEnv basisStore);;

let tcf = parseFromFile "TryCatchFinally.while";;
let _ = ignore (stm tcf  basisEnv basisStore);;

let tf = parseFromFile "TryFinally.while";;
let _ = ignore (stm tf  basisEnv basisStore);;

let pa = parseFromFile "ProcedureArguments.while";;
let _ = ignore (stm pa  basisEnv basisStore);;

let fmt = parseFromFile "FoldMapTest.while";;
let _ = ignore (stm fmt  basisEnv basisStore);;

let st = parseFromFile "ScopeTest.while";;
let _ = ignore (stm st  basisEnv basisStore);;
