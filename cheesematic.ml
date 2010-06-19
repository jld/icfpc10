open Carmatic
open Statics

let synth = Fancysynth.synth 4 12 6

let _ =
  List.iter (fun (carid, fuel) ->
    factorize_fuel synth carid fuel) 
    (strategize cheese (targets ()))
