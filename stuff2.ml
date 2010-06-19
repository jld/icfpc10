open Cheapsynth;;
open Factrun;;
open Xdr;;

go (key @ (xdr_int 3) @ (xdr_int 3) @ [0;0;0;1;0;0;0;0;0;0;0;0;0;0;0;0])

(*
let p = Factread.fparse stdin;;
let agh = [0;1;2;0;2;1;0;1;2;1;0;2];;
let ugh = run p (agh@agh);;
List.iter print_int ugh;
print_newline ();;
*)

(*

110212101121012211121012
11021210112101221

*)

