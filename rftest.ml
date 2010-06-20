open Randfuel

let () =
  let nt = int_of_string (Sys.argv.(1))
  and l = int_of_string (Sys.argv.(2)) in
  for i = 1 to l do
    let fu = xrandfuel 6 4 5 in
    let rec loop nt =
      match trysome 15 16 fu nt with
	None -> Printf.printf "None\n%!"
      | Some (n,ca) -> Printf.printf "Some %5d\t" (nt - n);
	  Printf.printf "  frag %d %d %d\n%!"
	    (fragility fu ca 5 100)
	    (fragility fu ca 10 100)
	    (fragility fu ca 20 100);
	  loop n
    in loop nt
  done
