type identifier = string
datatype let_type = LET | LET_STAR | LET_REC

datatype value = NIL
               | BOOL of bool
               | NUM of int
               | S_EXP_LIT of value
               | S_EXP_SYM of identifier
               | S_EXP_LIST of value list
               | PAIR of value * value
               | CLOSURE of ((identifier list) * exp) * ((identifier * exp)
               list)
               | PRIMITIVE of string
and

exp = LIT of value
             | VAR of identifier
             | SET of identifier * exp
             | IFX of exp * exp * exp
             | WHILEX of exp * exp
             | BEGIN of exp list
             | APPLY of exp * (exp list)
             | LETX of let_type * (identifier list) * (exp list) * exp
             | LAMBDA of lambda
             withtype lambda = (identifier list) * exp

datatype def = VAL of identifier * exp
             | EXP of exp
             | DEFINE of identifier * lambda

