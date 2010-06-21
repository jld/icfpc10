open Carmatic
open Statics
open Fakebear

let synth = Fancysynth.synth 4 12 6

let _ = 
  List.iter (fun (carid, fuel) ->
    factorize_fuel synth carid fuel)
    (strategize_trusted (clobber 1600) (targets ()))
