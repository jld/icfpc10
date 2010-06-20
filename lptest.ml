open Fancysynth
open Carmatic

let synth = Cheapsynth.cheapsynth

let _ =
  let carid = "13252" in
  let car = getcar carid in
  let fuel = Longpipe.hockeyize 23 car in
  factorize_fuel synth carid fuel
