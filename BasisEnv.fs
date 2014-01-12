module BasisEnv

open System
open AST
open Interpreter
open ParserUtil

//let plusInt = Primitive( fun [IntVal i1; IntVal i2] -> IntVal(i1+i2) );;
//let plusInt = Primitive( fun xs ->
//                            match List.head xs with
//                            | IntVal _ -> List.fold (fun acc (IntVal e) -> acc + e) 0 xs |> IntVal
//                            | StringVal _ -> List.fold (fun acc (StringVal e) -> acc + e) "" xs |> StringVal
//                            | _ -> failwith "unsupported type")
let plusInt = Primitive(function | [] -> failwith "Cant apply plus to zero elements"
                                 | x::_ as xs -> match x with
                                                 | IntVal _ -> List.fold (fun acc (IntVal e) -> acc + e) 0 xs |> IntVal
                                                 | StringVal _ -> List.fold (fun acc (StringVal e) -> acc + e) "" xs |> StringVal
                                                 | _ -> failwith "unsupported type")
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
                           | [Reference v] -> failwithf "attemptingt to print reference: %d" v
                           | _          -> failwith "error"
               Primitive f;;

let initEnv = Map.ofList [("+",plusInt); ("-",minusInt); ("*",multInt); ("=",eqInt); ("<>",neqInt); ("<=",lessEqInt); ("<",lessInt); 
                           ("randomInt", randomInt); ("toString",toString)  ];;

System.IO.Directory.SetCurrentDirectory __SOURCE_DIRECTORY__;;
let arrayUtilDecs = parseDecListFromFile "ArrayUtil.while";;

let (basisEnv, basisStore) = decList arrayUtilDecs initEnv Map.empty;; 