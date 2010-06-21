open Statics

let xrandfuel fs n mx =
  Array.init fs (fun _ ->
    Array.init n (fun i ->
      Array.init n (fun j ->
	if i == 0 && j == 0 then 1 else Random.int mx)))

let randpipe ns nf =
  Array.to_list (Array.init ns (fun _ -> Random.int nf))

let randscen nu nd fu =
  let nf = Array.length fu in
  [(randpipe nu nf), false, (randpipe nd nf)]

let rec trysome nu nd fu nt =
  if nt <= 0 then None
  else
    let sc = randscen nu nd fu in
    if will_run sc fu then
      Some (nt, sc)
    else
      trysome nu nd fu (pred nt)

let tocar = function
    Some (n,c) -> c
  | None -> raise Not_found

let perturb fu x =
  let fu = Array.map (Array.map Array.copy) fu in
  for h = 1 to x do
    let i = Random.int (Array.length fu) in
    let j = Random.int (Array.length fu.(i)) in
    let k = Random.int (Array.length fu.(i).(j)) in
    if (j != 0) || (k != 0) then
      match fu.(i).(j).(k) with
	0 -> fu.(i).(j).(k) <- 1
      | n -> fu.(i).(j).(k) <- 
	  if Random.bool () then
	    fu.(i).(j).(k) + 1
	  else
	    fu.(i).(j).(k) - 1
  done;
  fu
      
let fragility fu ca np nt =
  let c = ref 0 in
  for i = 1 to nt do
    if not (will_run ca (perturb fu np)) then
      incr c
  done;
  !c

let rec tryfrag otests nu nd fu n =
  match trysome nu nd fu n with
    None -> None
  | Some (n, sc) ->
      let rec loop tests =
	match tests with
	  [] -> Some (n, sc)
	| (np,rt,nt)::tests ->
	    if fragility fu sc np nt >= rt then
	      loop tests
	    else
	      tryfrag otests nu nd fu n
      in loop otests
      
let rec pbreak fu ca x =
  let fu = perturb fu x in
  if will_run ca fu then
    pbreak fu ca x
  else
    fu


(* *)

let shuffle a = 
  let a = Array.of_list a in
  for i = 1 to (Array.length a) - 1 do
    let j = Random.int (i + 1) in
    if i != j then
      let tmp = a.(i) in
      a.(i) <- a.(j);
      a.(j) <- tmp
  done;
  Array.to_list a

let randbal n fu =
  let u = randpipe n (Array.length fu) in
  [u, false, (shuffle u)]

let iota n = Array.to_list (Array.init n (fun x -> x))

let rec trybal n fu nt =
  if nt <= 0 then None
  else
    let sc = randbal n fu in
    if will_run sc fu then
      Some (nt, sc)
    else
      trybal n fu (pred nt)

let balcar fu n c =
  let rec loop c a =
    if c <= 0 then 
      let nf = Array.length fu in
  ((iota nf),true,(iota nf))::(List.sort compare a)
    else match trybal n fu max_int with
      Some (_, ch) ->
	loop (pred c) (ch@a)
    | _ -> raise Not_found
  in loop c []
    
