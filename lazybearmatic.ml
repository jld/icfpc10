open Carmatic
open Statics
open Fakebear

let synth = Fancysynth.synth 4 12 6

let _ = 
  List.iter (fun (carid, fuel) ->
    factorize_fuel synth carid fuel)
    (strategize_trusted (lazypunch 5) (targets ()))
(*       (Longpipe.replicate 10 ("5109",(getcar "5109")))) *)

(* 3 -> 10.26s, 2 cars *)
(* 4 -> 57.41s, 1 car *)
(* 5 -> 218.74s, 3 cars *)
(* 6 -> 647.81, 1 car *)
