(* Interpreter for a simple WHILE-language. Michael R. Hansen 03-01-2014 *)
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
                     | Reference loc as refl -> (refl,store)
                     | IntVal i              -> printfn "%s" (string i) ; failwith "errorXXX"
                     | _                     -> failwith "errorYYY"
    | ContOf er    -> match exp er env store with
                      | (Reference loc,store1) -> match Map.find loc store1 with 
                                                  | SimpVal res -> (res,store1)
                                                  | _           -> failwith "error"
                      | _                   -> failwith "error"

    | Apply(f,es) -> let (vals, store1) = expList es env store
                     match Map.find f env with 
                     | Primitive f   -> (f vals, store1) 
                     | Reference f -> 
                        match Map.find f store with
                        | Proc (parlist, defenv, statement) ->
                            let env =
                                List.fold2 (fun acc p1 p2 ->
                                    let p1' = match p1 with
                                              | Var n -> n
                                              | _ -> failwith "parameters must be vars"
                                    let loc = match Map.find p1' env with
                                              | Reference _ as refl -> refl
                                              | _ -> failwith "undefined parameter"
                                    Map.add p2 loc acc
                                ) env es parlist
                            let (ret, store') = stm statement env store
                            match ret with
                            | Some x -> (x, store')
                            | None -> failwith "Procedure must have return value"
                        | _ -> failwith "Unknown appliance"
                     | _              -> failwith "type error"          
                                                               
    | Int i       -> (IntVal i, store)
    | Bool b      -> (BoolVal b,store)
    | String s    -> (StringVal s,store)
    | ArrayElm(var, ex) ->
        let v = match var with
                | Var v -> v
                | _ -> failwith "must be array var"
        let (elm, store2) = exp ex env store
        let index = match elm with
                    | IntVal v -> v
                    | _ -> failwith "array index must be integer"
        match Map.find v env with
        | Reference r -> match Map.find r store2 with
                         | ArrayCnt a -> 
                            try
                                (a.[index], store2)
                            with
                            | :? System.IndexOutOfRangeException -> failwith "array index out of bounds"
                         | _ -> failwith "array not in store"
        | _ -> failwithf "variable %s is not array" v
    | ArrayFun(var, func) ->
        let var = match var with | Var v -> v | _ -> failwith "must be array var"
        match Map.find var env with
        | Reference r -> match Map.find r store with
                         | ArrayCnt a ->
                            match func with
                            | "length" -> (IntVal (Array.length a), store)
                            | _ -> failwith "unknown array function"
                         | _ -> failwith "array not in store"
        | _ -> failwith "variable %s not defined" var

and expList es env store = 
    match es with 
    | []       -> ([],store)
    | e::erest -> let (res1, store1) = exp e env store
                  let (ress, store2) = expList erest env store1
                  (res1::ress, store2)  

 
// stm: Stm -> Env -> Store -> option<Value> * Store
and stm st (env:Env) (store:Store) = 
    let (res, store') = (ref None, ref store)
    let exec st = let (r,s) = stm st env !store'
                  res := r
                  store' := s
    match st with 
    | Asg(el,e) ->
        let (res,store1) = exp e env store
        match el with
        | Var v -> let (resl, store2) = exp el env store1
                   let loc = match resl with | Reference l -> l | _ -> failwith "type error"
                   match res with
                   | Reference refloc -> (None, Map.add loc (Map.find refloc store2) store2)
                   | _ -> (None, Map.add loc (SimpVal res) store2)
        | ArrayElm(v,elexp) ->
            let (indexval, store2) = exp elexp env store
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
    | ProcCall(name, parlist) ->
        match Map.find name env with
        | Reference loc as refl ->
            match Map.find loc store with
            | Proc (parlist', defenv, st) ->
                let env' = 
                    List.fold2 (fun acc p1 p2 ->
                        let loc = match Map.tryFind p1 defenv with
                                  | Some x -> match x with
                                              | Reference _ as refl -> refl
                                              | _ -> failwith "undefined parameter"
                                  | None -> match Map.find p1 env with
                                            | Reference _ as refl -> refl
                                            | _ -> failwith "parameter not in scope"
                        Map.add p2 loc acc
                    ) env parlist parlist'
                stm st env' store
            | _ -> failwith "Error"  
        | _ -> failwith "Error"
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
    | TC(t,c) ->
        try
            stm t env store
        with
        | e -> printfn "(failed with: %s)" e.Message
               stm c env store
    | TF(t,f) -> try
                    try
                        exec t
                    with
                    | e -> printfn "(failed with: %s)" e.Message; ()
                 finally
                    exec f
                |> fun _ -> (!res, !store')
    | TCF(t,c,f) ->
        try
            try
                exec t
            with
            | e -> printfn "(failed with: %s)" e.Message
                   exec c
        finally
            exec f
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
                                                 -> let env2 = Map.add s (Reference loc) env
                                                    let store2 = Map.add loc (SimpVal res) store1
                                                    (env2, store2)
                     | _                         -> failwith "error"
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
        let store4 = match res with
                     | IntVal _ -> Map.add loc (ArrayCnt(Array.init arraysize (fun index -> res))) store3
                     | _ -> failwith "Arrays only support integers... for now"
        (env2, store4)
;;