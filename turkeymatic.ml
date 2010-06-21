open Carmatic
open Statics
  
let synth = Fancysynth.synth 4 12 6
   
let turkey car =
  try 
    keep_trying 2 2 (Some 10000) car
  with Time_exceeded -> 
    try keep_trying 3 2 (Some 50000) car
    with Time_exceeded ->
      cheese car
	
let longest car =
  let blah = ref 0 in
  List.iter (fun (up,_,dn) ->
    blah := max (max !blah (List.length up)) (List.length dn)) car;
  !blah

let _ = 
  List.iter (fun (name, car) ->
    if longest car > 10 then
      Printf.printf "PUNT: %s\n%!" name
    else 
      let fuel = turkey car in
      if will_run car fuel then begin
	Printf.printf "YES: %s\n%!" name;
	factorize_fuel synth name fuel
      end else
	Printf.printf "NO: %s\n%!" name)
    (targets ())
