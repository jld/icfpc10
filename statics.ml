let matmult a b =
  let il = Array.length a 
  and jl = Array.length b
  and kl = Array.length b.(0) in
  Array.init il (fun i ->
    Array.init kl (fun k ->
      let rec loop j s =
	if j >= jl then s
	else loop (succ j) (s + a.(i).(j) * b.(j).(k))
      in loop 0 0))

let matsub a b =
  Array.mapi (fun i ar ->
    Array.mapi (fun j ae ->
      ae - b.(i).(j)) ar) a

let ident n =
  Array.init n (fun row ->
    Array.init n (fun col ->
      if row == col then 1 else 0))

let rec crunch_pipe n = function
    [] -> ident n
  | [s] -> s
  | s::ss -> matmult s (crunch_pipe n ss)
	
let will_run car fuel = 
  if car == [] then true else
  let n = Array.length fuel.(0) in
  List.for_all (fun (up,auxp,dn) ->
    let diff = matsub
	(crunch_pipe n (List.map (Array.get fuel) up))
	(crunch_pipe n (List.map (Array.get fuel) dn)) in
    if (not auxp) && diff.(0).(0) <= 0 then false
    else
      let rec loop i =
	if i >= n then true
	else (let rec loop j =
	  if j >= n then true
	  else (diff.(i).(j) >= 0) &&
	    loop (succ j)
	in loop 0) && 
	  loop (succ i)
      in loop 0) car

let randfuel fs n mx =
  Array.init fs (fun _ ->
    Array.init n (fun i ->
      Array.init n (fun j ->
	(Random.int mx) + (if i == 0 && j == 0 then 1 else 0))))

let tanks car =
  let nt = ref 0 in
  let f x = nt := max !nt (succ x) in
  List.iter (fun (up,_,dn) ->
    List.iter f up;
    List.iter f dn) car;
  !nt

exception Time_exceeded
let keep_trying n mx tries car =
  let fs = tanks car in
  let rec loop tries =
    let rf = randfuel fs n mx in
    if will_run car rf then rf
    else match tries with
      None -> loop None
    | Some x when x <= 0 -> raise Time_exceeded
    | Some x -> loop (Some (pred x))
  in
  loop tries


let cheese car =
  Array.create (tanks car) [|[|2|]|]

let strategize strat car_alist =
  List.fold_right (fun (name,car) acc ->
    let fuel = strat car in
    if will_run car fuel then
      (name,fuel)::acc
    else
      acc) car_alist []

let strategize_trusted strat car_alist =
  List.fold_right (fun (name,car) acc ->
    Printf.eprintf "%s\n%!" name;
    match strat car with
      Some fuel ->
	(name,fuel)::acc
    | None -> acc) car_alist []

let xtracheesep car =
  List.for_all (fun (up,auxp,dn) ->
    let lu = List.length up
    and ld = List.length dn in
    if auxp then lu >= ld else lu > ld) car
