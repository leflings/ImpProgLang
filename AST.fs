// Michael R. Hansen 03-01-2014
module AST
open System

type Exp = | Int of int 
           | Bool of bool 
           | String of string 
           | Var of string 
           | ContOf of Exp 
           | Apply of string * List<Exp>    

and  Stm = | Asg of Exp * Exp
           | PrintLn of Exp
           | Seq of List<Stm>
           | While of Exp * Stm
           | Block of List<Dec> * Stm
           | ProcCall of string * string list
and Dec  = | VarDec of string * Exp
           | ProcDec of string * string list * Stm





