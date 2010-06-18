%{
open Factrun

let pmax i p = match p with
  X -> i
| L i'
| R i' -> max i (succ i')

let str = function
    X -> Printf.sprintf "X"
  | L i -> Printf.sprintf "%dL" i
  | R i -> Printf.sprintf "%dR" i

%}

%token <Factrun.port> PORT
%token GATE COMMA COLON EOF

%start factory
%type <Factrun.factory> factory

%%

inline : PORT COLON { $1 }
gatespec : PORT PORT GATE PORT PORT { ($1,$2,$4,$5) }
outline : COLON PORT { $2 }

gatelist : 
    { [] }
| gatespec
    { [$1] }
| gatelist COMMA gatespec 
    { $3::$1 }

factory : inline gatelist outline EOF {
  let gates = Array.of_list (List.rev $2) in
  let n =
    Array.fold_left (fun a (b,c,d,e) ->
      pmax (pmax (pmax (pmax a b) c) d) e) 
      (pmax (pmax 0 $1) $3) gates in
  let f = Factrun.factory (n + 1) in
  wire f X $1;
  Array.iteri (fun i (_,_,outl,outr) ->
    wire f (L i) outl;
    wire f (R i) outr) gates;
  Array.iteri (fun i (inl,inr,_,_) ->
    checkwire f inl (L i);
    checkwire f inr (R i)) gates;
  checkwire f $3 X;
  f
}

%%
