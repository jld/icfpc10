type gate = { in_left: port option ref;
	      in_right: port option ref;
              out_left: port option ref;
	      out_right: port option ref;
	      num: int }
and port = 
    X
  | L of gate
  | R of gate
  | P of pipe
and pipe = { in_pipe: port option ref;
             out_pipe: port option ref }

let ctr = ref 0
let gates = ref []
let out_world : port option ref = ref None
let in_world : port option ref = ref None

let gate () = 
  let gate = { in_left = ref None; in_right = ref None;
               out_left = ref None; out_right = ref None;
               num = !ctr } in
  incr ctr;
  gates := gate::!gates;
  gate 

let pipe () =
  { in_pipe = ref None; out_pipe = ref None }


let port_loc_in = function
  X -> in_world
| L g -> g.in_left
| R g -> g.in_right
| P p -> p.in_pipe

let port_loc_out = function
  X -> out_world
| L g -> g.out_left
| R g -> g.out_right
| P p -> p.out_pipe

let port_check r =
  if !r != None then failwith "Can't re-use port" else r

let wire p1 p2 =
  let r1 = port_check (port_loc_out p1)
  and r2 = port_check (port_loc_in p2) in
  r1 := Some p2;
  r2 := Some p1

let bprint_port_resolved b = function
    None -> Printf.bprintf b "_"
  | Some X -> Printf.bprintf b "X"
  | Some (L g) -> Printf.bprintf b "%dL" g.num
  | Some (R g) -> Printf.bprintf b "%dR" g.num
  | _ -> failwith "Unresolved port"

let rec bprint_port_in b p = match !p with
  | Some (P p) -> bprint_port_in b p.in_pipe
  | rp -> bprint_port_resolved b rp

let rec bprint_port_out b p = match !p with
  | Some (P p) -> bprint_port_out b p.out_pipe
  | rp -> bprint_port_resolved b rp

let bprint b =
  Printf.bprintf b "%a:" bprint_port_out out_world;
  let rec loop i l = 
    match l with 
      [] -> 
	Printf.bprintf b ":\n"
    | g::l ->
        assert (i == g.num);
	Printf.bprintf b "\n%a%a0#%a%a%s"
	  bprint_port_in g.in_left
	  bprint_port_in g.in_right
	  bprint_port_out g.out_left
	  bprint_port_out g.out_right
	  (if l == [] then "" else ",");
	loop (i + 1) l
  in loop 0 (List.rev !gates);
  Printf.bprintf b "%a\n" bprint_port_in in_world

let sprint () =
  let b = Buffer.create 512 in
  bprint b;
  Buffer.contents b

let print () =
  print_string (sprint ())

(* *)

let fgate pl pr =
  let g = gate () in
  wire pl (L g);
  wire pr (R g);
  ((L g), (R g))
