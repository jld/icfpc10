open Statics
open Num

let zero = num_of_int 0
let one = num_of_int 1

let stats c = 
  let t = tanks c in
  List.map (fun (up,_,dn) ->
    let au = Array.create t 0 
    and ad = Array.create t 0 in
    List.iter (fun x ->
      au.(x) <- au.(x) + 1) up;
    List.iter (fun x ->
      ad.(x) <- ad.(x) + 1) dn;
    Array.mapi (fun i u -> u - ad.(i)) au) c

let bees car ffuel = 
  let f = List.fold_left (fun a e -> a */ (num_of_int ffuel.(e))) one in
  List.map (fun (up,auxp,dn) ->
    (f up), (f dn) +/ (if auxp then zero else one)) car

let punch car st ffuel =
  let b = bees car ffuel in
  let sacc = Array.create (Array.length ffuel) 0
  and burnt = ref false in
  List.iter2 (fun (bxu,bxd) sx ->
    if bxu < bxd then begin
      burnt := true;
      Array.iteri (fun i se ->
	sacc.(i) <- sacc.(i) + se) sx
    end) b st;
  if not !burnt then None else
  let best = ref (sacc.(0), 0) in
  for i = 1 to pred (Array.length ffuel) do
    best := max !best (sacc.(i), i)
  done;
  Some !best

let punches car ffuel n =
  let ffuel = Array.copy ffuel
  and st = stats car in
  let rec loop n =
    if n <= 0 then None else
    match punch car st ffuel with
      None -> Some ffuel
    | Some (del,idx) -> 
	if del > 0 then begin
	  ffuel.(idx) <- ffuel.(idx) + 1;
	  loop (pred n)
	end else
	  None
  in loop n
	
let ifuel car = Array.create (tanks car) 1

let clobber n car =
  match punches car (ifuel car) n with
    None -> None
  | Some ff -> Some (Array.map (fun x -> [|[|x|]|]) ff)
