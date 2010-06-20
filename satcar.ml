open Randfuel
open Carmatic

let synth = Fancysynth.synth 4 12 6

let () =
  let carid = Sys.argv.(1) in
  Random.self_init ();
  let fu =
    let rec loop () =
      let fu = xrandfuel 6 4 5 in
      Printf.eprintf "Trying...\n%!";
      match tryfrag [5,15,20;5,75,100] 15 16 fu 100000 with
	None -> loop ()
      | Some _ -> fu
    in loop ()
  in
  Printf.eprintf "Got fuel!\n%!";
  let car = ref [] in
  for i = 1 to 20 do
    match tryfrag [5,14,20;5,70,100] 15 16 fu max_int with
      Some (n,ca) ->
	Printf.eprintf "Got rule (%d)\n%!" (max_int - n);
	car := ca @ !car
    | None ->
	Printf.eprintf "Can't happen\n%!"
  done;
  let car = ([0;1;2;3;4;5],true,[0;1;2;3;4;5])::(List.sort compare !car) in
  register_car carid car;
  factorize_fuel synth carid fu
