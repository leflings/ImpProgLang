// Implementation file for parser generated by fsyacc
module Parser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing.ParseHelpers
# 2 "C:\fp\Code\ImpProgLang\Parser.fsy"
 
open AST

# 10 "C:\fp\Code\ImpProgLang\Parser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | EOF
  | LPAR
  | RPAR
  | COLON
  | COMMA
  | PRINT
  | PROC
  | CALL
  | ASG
  | SKIP
  | SEMI
  | WHILE
  | DO
  | OD
  | CONTOF
  | LET
  | IN
  | END
  | TRUE
  | FALSE
  | NAME of (string)
  | STRING of (string)
  | BOOL of (bool)
  | INT of (int)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_EOF
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_COLON
    | TOKEN_COMMA
    | TOKEN_PRINT
    | TOKEN_PROC
    | TOKEN_CALL
    | TOKEN_ASG
    | TOKEN_SKIP
    | TOKEN_SEMI
    | TOKEN_WHILE
    | TOKEN_DO
    | TOKEN_OD
    | TOKEN_CONTOF
    | TOKEN_LET
    | TOKEN_IN
    | TOKEN_END
    | TOKEN_TRUE
    | TOKEN_FALSE
    | TOKEN_NAME
    | TOKEN_STRING
    | TOKEN_BOOL
    | TOKEN_INT
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startMain
    | NONTERM__startExp
    | NONTERM__startExpList
    | NONTERM__startDecList
    | NONTERM__startStm
    | NONTERM__startStmList
    | NONTERM__startDec
    | NONTERM__startPar
    | NONTERM__startParList
    | NONTERM_Main
    | NONTERM_Stm
    | NONTERM_StmList
    | NONTERM_Dec
    | NONTERM_DecList
    | NONTERM_Exp
    | NONTERM_ExpList
    | NONTERM_Par
    | NONTERM_ParList

// This function maps tokens to integers indexes
let tagOfToken (t:token) = 
  match t with
  | EOF  -> 0 
  | LPAR  -> 1 
  | RPAR  -> 2 
  | COLON  -> 3 
  | COMMA  -> 4 
  | PRINT  -> 5 
  | PROC  -> 6 
  | CALL  -> 7 
  | ASG  -> 8 
  | SKIP  -> 9 
  | SEMI  -> 10 
  | WHILE  -> 11 
  | DO  -> 12 
  | OD  -> 13 
  | CONTOF  -> 14 
  | LET  -> 15 
  | IN  -> 16 
  | END  -> 17 
  | TRUE  -> 18 
  | FALSE  -> 19 
  | NAME _ -> 20 
  | STRING _ -> 21 
  | BOOL _ -> 22 
  | INT _ -> 23 

// This function maps integers indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_EOF 
  | 1 -> TOKEN_LPAR 
  | 2 -> TOKEN_RPAR 
  | 3 -> TOKEN_COLON 
  | 4 -> TOKEN_COMMA 
  | 5 -> TOKEN_PRINT 
  | 6 -> TOKEN_PROC 
  | 7 -> TOKEN_CALL 
  | 8 -> TOKEN_ASG 
  | 9 -> TOKEN_SKIP 
  | 10 -> TOKEN_SEMI 
  | 11 -> TOKEN_WHILE 
  | 12 -> TOKEN_DO 
  | 13 -> TOKEN_OD 
  | 14 -> TOKEN_CONTOF 
  | 15 -> TOKEN_LET 
  | 16 -> TOKEN_IN 
  | 17 -> TOKEN_END 
  | 18 -> TOKEN_TRUE 
  | 19 -> TOKEN_FALSE 
  | 20 -> TOKEN_NAME 
  | 21 -> TOKEN_STRING 
  | 22 -> TOKEN_BOOL 
  | 23 -> TOKEN_INT 
  | 26 -> TOKEN_end_of_input
  | 24 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startMain 
    | 1 -> NONTERM__startExp 
    | 2 -> NONTERM__startExpList 
    | 3 -> NONTERM__startDecList 
    | 4 -> NONTERM__startStm 
    | 5 -> NONTERM__startStmList 
    | 6 -> NONTERM__startDec 
    | 7 -> NONTERM__startPar 
    | 8 -> NONTERM__startParList 
    | 9 -> NONTERM_Main 
    | 10 -> NONTERM_Stm 
    | 11 -> NONTERM_Stm 
    | 12 -> NONTERM_Stm 
    | 13 -> NONTERM_Stm 
    | 14 -> NONTERM_Stm 
    | 15 -> NONTERM_StmList 
    | 16 -> NONTERM_StmList 
    | 17 -> NONTERM_Dec 
    | 18 -> NONTERM_Dec 
    | 19 -> NONTERM_DecList 
    | 20 -> NONTERM_DecList 
    | 21 -> NONTERM_DecList 
    | 22 -> NONTERM_Exp 
    | 23 -> NONTERM_Exp 
    | 24 -> NONTERM_Exp 
    | 25 -> NONTERM_Exp 
    | 26 -> NONTERM_Exp 
    | 27 -> NONTERM_Exp 
    | 28 -> NONTERM_Exp 
    | 29 -> NONTERM_ExpList 
    | 30 -> NONTERM_ExpList 
    | 31 -> NONTERM_ExpList 
    | 32 -> NONTERM_Par 
    | 33 -> NONTERM_ParList 
    | 34 -> NONTERM_ParList 
    | 35 -> NONTERM_ParList 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 26 
let _fsyacc_tagOfErrorTerminal = 24

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | EOF  -> "EOF" 
  | LPAR  -> "LPAR" 
  | RPAR  -> "RPAR" 
  | COLON  -> "COLON" 
  | COMMA  -> "COMMA" 
  | PRINT  -> "PRINT" 
  | PROC  -> "PROC" 
  | CALL  -> "CALL" 
  | ASG  -> "ASG" 
  | SKIP  -> "SKIP" 
  | SEMI  -> "SEMI" 
  | WHILE  -> "WHILE" 
  | DO  -> "DO" 
  | OD  -> "OD" 
  | CONTOF  -> "CONTOF" 
  | LET  -> "LET" 
  | IN  -> "IN" 
  | END  -> "END" 
  | TRUE  -> "TRUE" 
  | FALSE  -> "FALSE" 
  | NAME _ -> "NAME" 
  | STRING _ -> "STRING" 
  | BOOL _ -> "BOOL" 
  | INT _ -> "INT" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | EOF  -> (null : System.Object) 
  | LPAR  -> (null : System.Object) 
  | RPAR  -> (null : System.Object) 
  | COLON  -> (null : System.Object) 
  | COMMA  -> (null : System.Object) 
  | PRINT  -> (null : System.Object) 
  | PROC  -> (null : System.Object) 
  | CALL  -> (null : System.Object) 
  | ASG  -> (null : System.Object) 
  | SKIP  -> (null : System.Object) 
  | SEMI  -> (null : System.Object) 
  | WHILE  -> (null : System.Object) 
  | DO  -> (null : System.Object) 
  | OD  -> (null : System.Object) 
  | CONTOF  -> (null : System.Object) 
  | LET  -> (null : System.Object) 
  | IN  -> (null : System.Object) 
  | END  -> (null : System.Object) 
  | TRUE  -> (null : System.Object) 
  | FALSE  -> (null : System.Object) 
  | NAME _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | STRING _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | BOOL _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | INT _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 0us; 65535us; 0us; 65535us; 0us; 65535us; 0us; 65535us; 0us; 65535us; 0us; 65535us; 0us; 65535us; 0us; 65535us; 1us; 65535us; 0us; 1us; 7us; 65535us; 0us; 18us; 8us; 9us; 10us; 40us; 25us; 40us; 30us; 40us; 41us; 40us; 50us; 51us; 4us; 65535us; 10us; 11us; 25us; 26us; 30us; 31us; 41us; 42us; 4us; 65535us; 6us; 52us; 12us; 13us; 28us; 52us; 53us; 52us; 3us; 65535us; 6us; 7us; 28us; 29us; 53us; 54us; 10us; 65535us; 2us; 3us; 4us; 67us; 21us; 22us; 23us; 24us; 33us; 34us; 44us; 45us; 59us; 60us; 61us; 62us; 64us; 67us; 68us; 67us; 3us; 65535us; 4us; 5us; 64us; 65us; 68us; 69us; 5us; 65535us; 14us; 15us; 16us; 71us; 37us; 71us; 48us; 71us; 72us; 71us; 4us; 65535us; 16us; 17us; 37us; 38us; 48us; 49us; 72us; 73us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 2us; 3us; 4us; 5us; 6us; 7us; 8us; 9us; 11us; 19us; 24us; 29us; 33us; 44us; 48us; 54us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 1us; 1us; 1us; 1us; 1us; 2us; 1us; 2us; 1us; 3us; 1us; 3us; 1us; 4us; 1us; 4us; 1us; 5us; 1us; 5us; 1us; 6us; 1us; 6us; 1us; 7us; 1us; 7us; 1us; 8us; 1us; 8us; 1us; 9us; 1us; 9us; 1us; 10us; 1us; 10us; 1us; 10us; 1us; 11us; 1us; 11us; 1us; 11us; 1us; 11us; 1us; 11us; 1us; 12us; 1us; 12us; 1us; 12us; 1us; 12us; 1us; 12us; 1us; 13us; 1us; 13us; 1us; 14us; 1us; 14us; 1us; 14us; 1us; 14us; 1us; 14us; 2us; 15us; 16us; 1us; 16us; 1us; 16us; 1us; 17us; 1us; 17us; 1us; 17us; 1us; 18us; 1us; 18us; 1us; 18us; 1us; 18us; 1us; 18us; 1us; 18us; 2us; 20us; 21us; 1us; 21us; 1us; 21us; 2us; 22us; 28us; 1us; 23us; 1us; 24us; 1us; 25us; 1us; 26us; 1us; 26us; 1us; 27us; 1us; 27us; 1us; 27us; 1us; 28us; 1us; 28us; 1us; 28us; 2us; 30us; 31us; 1us; 31us; 1us; 31us; 1us; 32us; 2us; 34us; 35us; 1us; 35us; 1us; 35us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 6us; 8us; 10us; 12us; 14us; 16us; 18us; 20us; 22us; 24us; 26us; 28us; 30us; 32us; 34us; 36us; 38us; 40us; 42us; 44us; 46us; 48us; 50us; 52us; 54us; 56us; 58us; 60us; 62us; 64us; 66us; 68us; 70us; 72us; 74us; 76us; 78us; 80us; 83us; 85us; 87us; 89us; 91us; 93us; 95us; 97us; 99us; 101us; 103us; 105us; 108us; 110us; 112us; 115us; 117us; 119us; 121us; 123us; 125us; 127us; 129us; 131us; 133us; 135us; 137us; 140us; 142us; 144us; 146us; 149us; 151us; |]
let _fsyacc_action_rows = 74
let _fsyacc_actionTableElements = [|5us; 32768us; 5us; 33us; 7us; 35us; 11us; 23us; 15us; 28us; 20us; 20us; 0us; 49152us; 6us; 32768us; 1us; 61us; 14us; 59us; 20us; 55us; 21us; 58us; 22us; 57us; 23us; 56us; 0us; 49152us; 6us; 16413us; 1us; 61us; 14us; 59us; 20us; 55us; 21us; 58us; 22us; 57us; 23us; 56us; 0us; 49152us; 2us; 16403us; 6us; 46us; 20us; 43us; 0us; 49152us; 5us; 32768us; 5us; 33us; 7us; 35us; 11us; 23us; 15us; 28us; 20us; 20us; 0us; 49152us; 5us; 32768us; 5us; 33us; 7us; 35us; 11us; 23us; 15us; 28us; 20us; 20us; 0us; 49152us; 2us; 32768us; 6us; 46us; 20us; 43us; 0us; 49152us; 1us; 32768us; 20us; 70us; 0us; 49152us; 1us; 16417us; 20us; 70us; 0us; 49152us; 1us; 32768us; 0us; 19us; 0us; 16393us; 1us; 32768us; 8us; 21us; 6us; 32768us; 1us; 61us; 14us; 59us; 20us; 55us; 21us; 58us; 22us; 57us; 23us; 56us; 0us; 16394us; 6us; 32768us; 1us; 61us; 14us; 59us; 20us; 55us; 21us; 58us; 22us; 57us; 23us; 56us; 1us; 32768us; 12us; 25us; 5us; 32768us; 5us; 33us; 7us; 35us; 11us; 23us; 15us; 28us; 20us; 20us; 1us; 32768us; 13us; 27us; 0us; 16395us; 2us; 16403us; 6us; 46us; 20us; 43us; 1us; 32768us; 16us; 30us; 5us; 32768us; 5us; 33us; 7us; 35us; 11us; 23us; 15us; 28us; 20us; 20us; 1us; 32768us; 17us; 32us; 0us; 16396us; 6us; 32768us; 1us; 61us; 14us; 59us; 20us; 55us; 21us; 58us; 22us; 57us; 23us; 56us; 0us; 16397us; 1us; 32768us; 20us; 36us; 1us; 32768us; 1us; 37us; 1us; 16417us; 20us; 70us; 1us; 32768us; 2us; 39us; 0us; 16398us; 1us; 16399us; 10us; 41us; 5us; 32768us; 5us; 33us; 7us; 35us; 11us; 23us; 15us; 28us; 20us; 20us; 0us; 16400us; 1us; 32768us; 3us; 44us; 6us; 32768us; 1us; 61us; 14us; 59us; 20us; 55us; 21us; 58us; 22us; 57us; 23us; 56us; 0us; 16401us; 1us; 32768us; 20us; 47us; 1us; 32768us; 1us; 48us; 1us; 16417us; 20us; 70us; 1us; 32768us; 2us; 50us; 5us; 32768us; 5us; 33us; 7us; 35us; 11us; 23us; 15us; 28us; 20us; 20us; 0us; 16402us; 1us; 16404us; 10us; 53us; 2us; 16403us; 6us; 46us; 20us; 43us; 0us; 16405us; 1us; 16406us; 1us; 64us; 0us; 16407us; 0us; 16408us; 0us; 16409us; 6us; 32768us; 1us; 61us; 14us; 59us; 20us; 55us; 21us; 58us; 22us; 57us; 23us; 56us; 0us; 16410us; 6us; 32768us; 1us; 61us; 14us; 59us; 20us; 55us; 21us; 58us; 22us; 57us; 23us; 56us; 1us; 32768us; 2us; 63us; 0us; 16411us; 6us; 16413us; 1us; 61us; 14us; 59us; 20us; 55us; 21us; 58us; 22us; 57us; 23us; 56us; 1us; 32768us; 2us; 66us; 0us; 16412us; 1us; 16414us; 4us; 68us; 6us; 16413us; 1us; 61us; 14us; 59us; 20us; 55us; 21us; 58us; 22us; 57us; 23us; 56us; 0us; 16415us; 0us; 16416us; 1us; 16418us; 4us; 72us; 1us; 16417us; 20us; 70us; 0us; 16419us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 6us; 7us; 14us; 15us; 22us; 23us; 26us; 27us; 33us; 34us; 40us; 41us; 44us; 45us; 47us; 48us; 50us; 51us; 53us; 54us; 56us; 63us; 64us; 71us; 73us; 79us; 81us; 82us; 85us; 87us; 93us; 95us; 96us; 103us; 104us; 106us; 108us; 110us; 112us; 113us; 115us; 121us; 122us; 124us; 131us; 132us; 134us; 136us; 138us; 140us; 146us; 147us; 149us; 152us; 153us; 155us; 156us; 157us; 158us; 165us; 166us; 173us; 175us; 176us; 183us; 185us; 186us; 188us; 195us; 196us; 197us; 199us; 201us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 2us; 3us; 5us; 5us; 2us; 5us; 1us; 3us; 3us; 6us; 0us; 1us; 3us; 1us; 1us; 1us; 1us; 2us; 3us; 4us; 0us; 1us; 3us; 1us; 0us; 1us; 3us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 3us; 4us; 5us; 6us; 7us; 8us; 9us; 10us; 10us; 10us; 10us; 10us; 11us; 11us; 12us; 12us; 13us; 13us; 13us; 14us; 14us; 14us; 14us; 14us; 14us; 14us; 15us; 15us; 15us; 16us; 17us; 17us; 17us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 49152us; 65535us; 49152us; 65535us; 49152us; 65535us; 49152us; 65535us; 49152us; 65535us; 49152us; 65535us; 49152us; 65535us; 49152us; 65535us; 16393us; 65535us; 65535us; 16394us; 65535us; 65535us; 65535us; 65535us; 16395us; 65535us; 65535us; 65535us; 65535us; 16396us; 65535us; 16397us; 65535us; 65535us; 65535us; 65535us; 16398us; 65535us; 65535us; 16400us; 65535us; 65535us; 16401us; 65535us; 65535us; 65535us; 65535us; 65535us; 16402us; 65535us; 65535us; 16405us; 65535us; 16407us; 16408us; 16409us; 65535us; 16410us; 65535us; 65535us; 16411us; 65535us; 65535us; 16412us; 65535us; 65535us; 16415us; 16416us; 65535us; 65535us; 16419us; |]
let _fsyacc_reductions ()  =    [| 
# 255 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Stm)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startMain));
# 264 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Exp)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startExp));
# 273 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Exp list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startExpList));
# 282 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Dec list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startDecList));
# 291 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Stm)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startStm));
# 300 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Stm list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startStmList));
# 309 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Dec)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startDec));
# 318 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startPar));
# 327 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startParList));
# 336 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Stm)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 30 "C:\fp\Code\ImpProgLang\Parser.fsy"
                                                               _1 
                   )
# 30 "C:\fp\Code\ImpProgLang\Parser.fsy"
                 : Stm));
# 347 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Exp)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 33 "C:\fp\Code\ImpProgLang\Parser.fsy"
                                                             Asg(Var _1,_3) 
                   )
# 33 "C:\fp\Code\ImpProgLang\Parser.fsy"
                 : Stm));
# 359 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Exp)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : Stm list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 34 "C:\fp\Code\ImpProgLang\Parser.fsy"
                                                       While(_2,Seq _4) 
                   )
# 34 "C:\fp\Code\ImpProgLang\Parser.fsy"
                 : Stm));
# 371 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Dec list)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : Stm list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 35 "C:\fp\Code\ImpProgLang\Parser.fsy"
                                                                Block(_2,Seq _4) 
                   )
# 35 "C:\fp\Code\ImpProgLang\Parser.fsy"
                 : Stm));
# 383 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Exp)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 36 "C:\fp\Code\ImpProgLang\Parser.fsy"
                                                                PrintLn _2 
                   )
# 36 "C:\fp\Code\ImpProgLang\Parser.fsy"
                 : Stm));
# 394 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : string list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 37 "C:\fp\Code\ImpProgLang\Parser.fsy"
                                                       ProcCall(_2, _4) 
                   )
# 37 "C:\fp\Code\ImpProgLang\Parser.fsy"
                 : Stm));
# 406 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Stm)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 40 "C:\fp\Code\ImpProgLang\Parser.fsy"
                                                               [_1] 
                   )
# 40 "C:\fp\Code\ImpProgLang\Parser.fsy"
                 : Stm list));
# 417 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Stm)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Stm list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 41 "C:\fp\Code\ImpProgLang\Parser.fsy"
                                                               _1 :: _3 
                   )
# 41 "C:\fp\Code\ImpProgLang\Parser.fsy"
                 : Stm list));
# 429 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Exp)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 44 "C:\fp\Code\ImpProgLang\Parser.fsy"
                                                                VarDec(_1,_3) 
                   )
# 44 "C:\fp\Code\ImpProgLang\Parser.fsy"
                 : Dec));
# 441 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : string list)) in
            let _6 = (let data = parseState.GetInput(6) in (Microsoft.FSharp.Core.Operators.unbox data : Stm)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 45 "C:\fp\Code\ImpProgLang\Parser.fsy"
                                                            ProcDec(_2, _4, _6) 
                   )
# 45 "C:\fp\Code\ImpProgLang\Parser.fsy"
                 : Dec));
# 454 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 48 "C:\fp\Code\ImpProgLang\Parser.fsy"
                                                               [] 
                   )
# 48 "C:\fp\Code\ImpProgLang\Parser.fsy"
                 : Dec list));
# 464 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Dec)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 49 "C:\fp\Code\ImpProgLang\Parser.fsy"
                                                               [_1] 
                   )
# 49 "C:\fp\Code\ImpProgLang\Parser.fsy"
                 : Dec list));
# 475 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Dec)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Dec list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 50 "C:\fp\Code\ImpProgLang\Parser.fsy"
                                                               _1 :: _3 
                   )
# 50 "C:\fp\Code\ImpProgLang\Parser.fsy"
                 : Dec list));
# 487 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 53 "C:\fp\Code\ImpProgLang\Parser.fsy"
                                                                Var _1  
                   )
# 53 "C:\fp\Code\ImpProgLang\Parser.fsy"
                 : Exp));
# 498 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 54 "C:\fp\Code\ImpProgLang\Parser.fsy"
                                                                Int _1 
                   )
# 54 "C:\fp\Code\ImpProgLang\Parser.fsy"
                 : Exp));
# 509 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : bool)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 55 "C:\fp\Code\ImpProgLang\Parser.fsy"
                                                                Bool _1 
                   )
# 55 "C:\fp\Code\ImpProgLang\Parser.fsy"
                 : Exp));
# 520 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 56 "C:\fp\Code\ImpProgLang\Parser.fsy"
                                                                String _1
                   )
# 56 "C:\fp\Code\ImpProgLang\Parser.fsy"
                 : Exp));
# 531 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Exp)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 57 "C:\fp\Code\ImpProgLang\Parser.fsy"
                                                                ContOf _2 
                   )
# 57 "C:\fp\Code\ImpProgLang\Parser.fsy"
                 : Exp));
# 542 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Exp)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 58 "C:\fp\Code\ImpProgLang\Parser.fsy"
                                                                _2 
                   )
# 58 "C:\fp\Code\ImpProgLang\Parser.fsy"
                 : Exp));
# 553 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Exp list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 59 "C:\fp\Code\ImpProgLang\Parser.fsy"
                                                                Apply(_1, _3) 
                   )
# 59 "C:\fp\Code\ImpProgLang\Parser.fsy"
                 : Exp));
# 565 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 62 "C:\fp\Code\ImpProgLang\Parser.fsy"
                                                               [ ] 
                   )
# 62 "C:\fp\Code\ImpProgLang\Parser.fsy"
                 : Exp list));
# 575 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Exp)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 63 "C:\fp\Code\ImpProgLang\Parser.fsy"
                                                               [_1]   
                   )
# 63 "C:\fp\Code\ImpProgLang\Parser.fsy"
                 : Exp list));
# 586 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Exp)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Exp list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 64 "C:\fp\Code\ImpProgLang\Parser.fsy"
                                                               _1 :: _3 
                   )
# 64 "C:\fp\Code\ImpProgLang\Parser.fsy"
                 : Exp list));
# 598 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 67 "C:\fp\Code\ImpProgLang\Parser.fsy"
                                _1 
                   )
# 67 "C:\fp\Code\ImpProgLang\Parser.fsy"
                 : string));
# 609 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 70 "C:\fp\Code\ImpProgLang\Parser.fsy"
                             [] 
                   )
# 70 "C:\fp\Code\ImpProgLang\Parser.fsy"
                 : string list));
# 619 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 71 "C:\fp\Code\ImpProgLang\Parser.fsy"
                                 [_1] 
                   )
# 71 "C:\fp\Code\ImpProgLang\Parser.fsy"
                 : string list));
# 630 "C:\fp\Code\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : string list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 72 "C:\fp\Code\ImpProgLang\Parser.fsy"
                                            _1 :: _3 
                   )
# 72 "C:\fp\Code\ImpProgLang\Parser.fsy"
                 : string list));
|]
# 643 "C:\fp\Code\ImpProgLang\Parser.fs"
let tables () : Microsoft.FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:Microsoft.FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 27;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let Main lexer lexbuf : Stm =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))
let Exp lexer lexbuf : Exp =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 2))
let ExpList lexer lexbuf : Exp list =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 4))
let DecList lexer lexbuf : Dec list =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 6))
let Stm lexer lexbuf : Stm =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 8))
let StmList lexer lexbuf : Stm list =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 10))
let Dec lexer lexbuf : Dec =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 12))
let Par lexer lexbuf : string =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 14))
let ParList lexer lexbuf : string list =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 16))
