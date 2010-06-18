{
open Factrun
open Factparse
}

let ws = [' ''\r''\t''\n']*
let num = ['0'-'9']+

rule token = parse
             "X" { PORT (X) }
| (num as n) "L" { PORT (L (int_of_string n)) }
| (num as n) "R" { PORT (R (int_of_string n)) }
|           "0#" { GATE }
|         "," ws { COMMA }
|         ":" ws { COLON }
|         "\n"? { EOF }
