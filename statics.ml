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
  | s::ss -> matmult (crunch_pipe n ss) s
	
let will_run car fuel = 
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

let rec keep_trying car fs n mx =
  let rf = randfuel fs n mx in
  if will_run car rf then rf
  else keep_trying car fs n mx
