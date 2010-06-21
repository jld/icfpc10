open Randfuel
open Carmatic

let synth = Fancysynth.synth 4 12 6

let () =
  Random.self_init ();
  let carid = Sys.argv.(1) in
  let fuel = xrandfuel 4 4 4 in
  let car = balcar fuel 18 80 in
  register_car carid car;
  factorize_fuel synth carid fuel
