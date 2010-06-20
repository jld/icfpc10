open Carmatic
open Statics
open Fakebear

let synth = Fancysynth.synth 4 12 6

let _ = 
  List.iter (fun (carid, fuel) ->
    factorize_fuel synth carid fuel)
    (strategize_trusted (twopunch 3) (targets ()))
(*
       (List.map (fun (t,n,c) -> n)
	  (List.sort compare 
	     (List.map (fun (n,c) -> (tanks c, n, c)) 
		(targets ())))))
*)
 
