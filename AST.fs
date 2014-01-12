// Michael R. Hansen 03-01-2014
module AST
open System

type Exp = | Int of int 
           | Bool of bool 
           | String of string 
           | Var of string 
           | ContOf of Exp 
           | Apply of string * List<Exp>
           | ArrayElm of Exp * Exp  // Var * Exp
           | ArrayFun of Exp * string // Var * length (or others)

and  Stm = | Asg of Exp * Exp
           | PrintLn of Exp
           | Seq of List<Stm>
           | While of Exp * Stm
           | Block of List<Dec> * Stm
           | ProcCall of string * string list
           | IT of Exp * Stm
           | ITE of Exp * Stm * Stm
           | Return of Exp
           | Do of Stm
           | TC of string * Stm * Stm // Try Catch
           | TF of Stm * Stm // Try Finally
           | TCF of string * Stm * Stm * Stm // Try catch finally
and Dec  = | VarDec of string * Exp
           | ProcDec of string * string list * Stm
           | ArrayDec of string * Exp * Exp





