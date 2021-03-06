﻿(* Interpreter for a simple WHILE-language. Michael R. Hansen 03-01-2014 *)
(* Based on a natural semantics of WHILE                                 *)

(* Remember to regenerate the parser and the lexer using the commands 
   in README.txt if you modified the parser and lexer                    *)

module Interpreter 

open System
open AST

type Location = int
type Value    = | IntVal of int 
                | BoolVal of bool 
                | StringVal of string 
                | Reference of Location 
                | Primitive of (List<Value> -> Value)
and Env       = Map<string,Value> // Environment maps names to their meanings


type Closure =  List<string> * Env * Stm // A closure denotes the meaning of a procedure declaration

type Content = SimpVal of Value | Proc of Closure |  ArrayCnt of Value [];;

type Store  = Map<Location,Content> // Store maps locations to contents
  
let closureOf(ps,st) env = (ps, env, st)

// nextLoc() generates the next available location
let nextLoc: unit -> int =  let n = ref 0
                            let f x = (n := !n+1; !n)
                            f
let printenv (env:Env) =
    env
    |> Map.iter (fun k v ->
            let s = match v with
                    | Primitive _ -> "primitive"
                    | s -> s |> string
            printf "'%s' -> %s; " k s)
    printfn ""

                


// exp: Exp -> Env -> Store -> Value * Store 
let rec exp e (env:Env) (store:Store) = 
    match e with
    | Var v       -> match Map.find v env with
                     | Reference _ as x -> (x,store)
                     | Primitive _ as x -> (x,store)
                     | IntVal i              -> printfn "%s" (string i) ; failwith "errorXXX"
                     | _                     -> failwith "errorYYY"
    | ContOf er    -> match exp er env store with
                      | (Reference loc,store1) -> match Map.find loc store1 with 
                                                  | SimpVal res -> (res,store1)
                                                  | _           -> failwith "error"
                      | _                   -> failwith "error"

    | Apply(f,es) -> let (arglist, store1) = expList es env store
                     let (func, store2) = exp f env store1
                     match func with 
                     | Primitive f   -> (f arglist, store1) 
                     | Reference r -> 
                        let procedure = match Map.find r store with
                                        | Proc(_,_,_) as p -> p
                                        | _ -> failwith "variable doesnt reference a procedure"
                        match procedure with
                        | Proc(parlist, defenv, statement) ->
                            let (procenv,procstore) =
                                List.fold2
                                    (fun (aenv,astore) arg par ->
                                        match arg with
                                        | Reference _ ->
                                            ((Map.add par arg aenv), astore)
                                        | x -> let loc = nextLoc()
                                               ((Map.add par (Reference loc) aenv), Map.add loc (SimpVal x) astore)
                                    ) (defenv,store1) arglist parlist
                            let (ret, store') = stm statement procenv procstore
                            match ret with
                            | Some x -> (x, store')
                            | None -> failwith "Procedure must have return value"
                        | _ -> failwith "parser problem"
                     | _              -> failwith "type error"          
                                                               
    | Int i       -> (IntVal i, store)
    | Bool b      -> (BoolVal b,store)
    | String s    -> (StringVal s,store)
    | ArrayElm(var, ex) ->
        let (var, store1) = exp var env store
        let (elm, store2) = exp ex env store1
        let index = match elm with
                    | IntVal v -> v
                    | _ -> failwith "array index must be integer"
        match var with
        | Reference r -> match Map.find r store2 with
                         | ArrayCnt a -> 
                            try
                                (a.[index], store2)
                            with
                            | :? System.IndexOutOfRangeException -> failwith "array index out of bounds"
                         | _ -> failwith "array not in store"
        | _ -> failwith "variable is not array"
    | ArrayFun(arrayExp, func) ->
        let (var,_) = exp arrayExp env store
        let array = match var with | Reference r -> match Map.find r store with
                                                    | ArrayCnt a -> a
                                                    | _ -> failwith "Expression not resolved to array"
                                   | _ -> failwith "Expression not resolved to array"
        match func with
        | "length" -> (IntVal (Array.length array), store)
        | _ -> failwith "unknown array function"

and expList es env store = 
    match es with 
    | []       -> ([],store)
    | e::erest -> let (res1, store1) = exp e env store
                  let (ress, store2) = expList erest env store1
                  (res1::ress, store2)  

 
// stm: Stm -> Env -> Store -> option<Value> * Store
and stm st (env:Env) (store:Store) = 
    let (res, store') = (ref None, ref store)
    let exec st e = let (r,s) = stm st e !store'
                    res := r
                    store' := s
    match st with 
    | Asg(el,e) ->
        let (res,store1) = exp e env store
        match el with
        | Var v -> let (resl, store1) = exp el env store1
                   let loc = match resl with | Reference l -> l | _ -> failwith "type error"
                   match res with
                   | Reference refloc -> (None, Map.add loc (Map.find refloc store1) store1)
                   | _ -> (None, Map.add loc (SimpVal res) store1)
        | ArrayElm(v,elexp) ->
            let (indexval, store2) = exp elexp env store1
            let index = match indexval with | IntVal v -> v | _ -> failwith "index expression must evaluate to integer"
            match v with
            | Var v ->
                match Map.find v env with
                | Reference loc -> match Map.find loc store2 with
                                   | ArrayCnt a -> a.[index] <- res
                                                   (None, store2)
                                   | _ -> failwith "type error"
                | _ -> failwith "error"
            | _ -> failwith "type error"
        | _ -> failwith "wut?"
                   
    | PrintLn e -> match exp e env store with
                   | (StringVal s,store1) -> (printfn "%s" s; (None,store1))
                   | _                    -> failwith "error"                  
                                                                 
                                           
    | Seq []        -> (None,store)
    | Seq (st::sts) -> match stm st env store with 
                       | (None,store1)   -> stm (Seq sts) env store1
                       | result       -> result

    | While(e,st1)  -> let (res, store1) = exp e env store
                       match res with 
                       | BoolVal true  -> match stm st1 env store1 with
                                          | (None,store2) -> stm st env store2
                                          | result     -> result
                       | BoolVal false -> (None, store1)
                       | _             -> failwith "type error"                     
 
    | Block(ds,st1) -> let (env1,store1) = decList ds env store 
                       stm st1 env1 store1
    | Do(st) ->     stm st env store
    | ProcCall(nameExp) ->
        match nameExp with
        | Apply(a, b) ->
            let procedure = match fst (exp a env store) with
                            | Reference r -> match Map.find r store with
                                             | Proc(_,_,_) as p -> p
                                             | _ -> failwith "variable doesnt reference a procedure"
                            | _ -> failwith "variable is not a reference"
            let (arglist,store1) = expList b env store
            match procedure with
            | Proc(parlist, defenv, statement) ->
                let (procenv,procstore) =
                    List.fold2
                        (fun (aenv,astore) arg par ->
                            match arg with
                            | Reference _ -> ((Map.add par arg aenv), astore)
                            | x -> let loc = nextLoc()
                                   ((Map.add par (Reference loc) aenv), Map.add loc (SimpVal x) astore)
                        ) (defenv,store1) arglist parlist
                stm statement procenv procstore
            | _ -> failwith "impossible"    
        | _ -> failwith "parser problem"

    | Return(ex) ->
        let (value, store') = exp ex env store
        (Some value, store')

    | IT(ex, st) -> let (res, st') = exp ex env store
                    match res with
                    | BoolVal true -> stm st env st'
                    | BoolVal false -> (None, st')
                    | _ -> failwith "if expr must evalute to boolean"

    | ITE(ex, st1, st2) ->
        let (res, st') = exp ex env store
        match res with
        | BoolVal true -> stm st1 env st'
        | BoolVal false -> stm st2 env st'
        | _ -> failwith "if expr must evalute to boolean"

    | TC(var, t,c) ->
        try
            stm t env store
        with
        | e -> let (env1, store1) = dec (VarDec(var, String e.Message)) env store
               stm c env1 store1
    | TF(t,f) -> try
                    try
                        exec t env
                    with
                    | e -> ()
                 finally
                    exec f env
                |> fun _ -> (!res, !store')
    | TCF(var, t,c,f) ->
        try
            try
                exec t env
            with
            | e -> let (env1, store1) = dec (VarDec(var, String e.Message)) env !store'
                   store' := store1

                   exec c env1
        finally
            exec f env
        |> fun _ -> (!res, !store')
    
and decList ds env store = 
    match ds with
    | []       -> (env,store)
    | d::drest -> let (env1,store1) = dec d env store
                  decList drest env1 store1

and dec d env store =
    match d with 
    | VarDec(s,e) -> let loc = nextLoc()
                     match exp e env store with
                     | (IntVal _ as res, store1)  
                     | (BoolVal _ as res, store1) 
                     | (StringVal _ as res, store1)  
                     | (Reference _ as res, store1)
                                                 -> let env2 = Map.add s (Reference loc) env
                                                    let store2 = Map.add loc (SimpVal res) store1
                                                    (env2, store2)
                     | x                         -> failwith "error"
    | ProcDec(name, parlist, stm) ->
        let loc = nextLoc()
        let env2 = Map.add name (Reference loc) env
        let store2 = Map.add loc (Proc (parlist, env2, stm)) store
        (env2, store2)
    | ArrayDec(name, size, init) ->
        let loc = nextLoc()
        let env2 = Map.add name (Reference loc) env
        let (sizeVal, store2) = exp size env2 store
        let arraysize = match sizeVal with
                        | IntVal v -> v
                        | _ -> failwith "Array length must be given as int"
        let (res, store3) = exp init env2 store2
        let store4 = Map.add loc (ArrayCnt(Array.init arraysize (fun _ -> res))) store3
        (env2, store4)
;;