open Carmatic
open Statics
open Fakebear

let synth = Fancysynth.synth 4 12 6

let _ = 
  List.iter (fun (carid, fuel) ->
    factorize_fuel synth carid fuel)
    (strategize_trusted (twopunch 8)
       (List.filter (fun (n,c) -> tanks c <= 5) (targets ())))
 
