open Carmatic
open Statics
open Longpipe

let synth = Fancysynth.synth 4 12 6

let _ =
  List.iter (fun (carid, fuel) ->
    factorize_efuel synth carid fuel)
    (strategize_trusted maybe_longcheese (targets ()))
