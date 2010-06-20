open Statics

let rec matpow m = function
    0 -> ident (Array.length m)
  | 1 -> m
  | n ->
      let m2 = matpow m (n / 2) in
      let m3 = matmult m2 m2 in
      if n mod 2 == 1 then
	matmult m3 m
      else 
	m3

let hockey n =
  Array.init n (fun row ->
    Array.init n (fun col ->
      if ((row == 0 && col == 0) 
	|| (col == (row + 1) mod n)) then 1 else 0))

let replicate n x = Array.to_list (Array.create n x)

let dereplicate l =
  match l with 
    [] -> None
  | x::_ -> let n = List.length l in
    if l = (replicate n x) then Some (n,x) else None

let unpower car =
  List.map (fun (up,auxp,dn) ->
    if up = dn && auxp then None
    else match dereplicate up with
      Some dup -> (match dereplicate dn with
	Some ddn -> Some (dup,auxp,ddn)
      | _ -> raise Not_found)
    | _ -> raise Not_found) car

let recognize_longpipe car = 
  let dep = unpower car in
  let tn = tanks car in
  let lbs = Array.create tn None
  and ubs = Array.create tn None 
  and hock = Array.create tn false in
  List.iter (function
      None -> ()
    | Some ((1, big), _, (n, small)) ->
	(* big > small**n *)
	hock.(small) <- true;
	lbs.(big) <- Some (small, n)
    | Some ((n, small), _, (1, big)) ->
	(* small**n > big *)
	hock.(small) <- true;
	ubs.(big) <- Some (small, n)
    | _ ->
	failwith "Unrecognized chamber") dep;
  let stuff = ref [] in
  for i = 0 to pred tn do
    match (hock.(i), lbs.(i), ubs.(i)) with
      (true, None, None) -> ()
    | (false, Some (hl, pl), Some (hu, pu))
	when hl == hu && hock.(hl) && pl < pu ->
	  stuff := (i, hl, pl, pu)::!stuff
    | _ ->
	failwith "Unrecognized situation"
  done;
  List.rev !stuff
	

let hockeyize n car =
  let tn = tanks car 
  and reco = recognize_longpipe car in
  let hock = hockey n in
  let fu = Array.create tn hock in
  List.iter (fun (big, small, pl, pu) ->
    fu.(big) <- matpow hock pl;
    fu.(big).(0).(0) <- fu.(big).(0).(0) + 1) reco;
  fu
