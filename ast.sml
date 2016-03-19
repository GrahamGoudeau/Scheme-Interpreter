type identifier = string
type value = string

datatype expr = CONST of const_type * value
                and const_type = BOOL | NUM | CHAR | STRING
datatype form = DEFN of identifier * expr
                | EXPR of expr
