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
    if bxu </ bxd then begin
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


(*  *)

let eyes car ffuel = 
  let f = List.fold_left (fun a e -> a */ (num_of_int ffuel.(e))) one in
  List.for_all (fun (up,auxp,dn) ->
    (f up) >=/ (f dn) +/ (if auxp then zero else one)) car

let aiter n b sum f =
  let arr = Array.create n 0 in
  let rec loopi sum i =
    if sum < i || sum > b * i then () else
    if i <= 0 then f arr else
    for x = 1 to b do
      arr.(n - i) <- x;
      loopi (sum - x) (pred i)
    done in
  loopi sum n 


exception Get_off_the_bike of int array

let lazypunch b car =
  let n = tanks car in
  try
    for sum = n to b * n do
      aiter n b sum (fun ff ->
	if eyes car ff then
	  raise (Get_off_the_bike ff))
    done;
    None
  with
    Get_off_the_bike ff ->
      Some (Array.map (fun x -> [|[|x|]|]) ff)

(* *)

let ualue pipe twobs = 
  let a = [|one;zero;zero;one|] in
    (* Why I'm going to all this trouble to avoid allocating
       when Num does so even for 2 + 2.... *)
  List.iter (fun idx ->
    let (b01,b10,b11) = twobs.(idx)
    and a00 = a.(0) and a01 = a.(1) and a10 = a.(2) and a11 = a.(3)
    in
    let c00 = (            a00          ) +/ (if b10 then a01 else zero)
    and c01 = (if b01 then a00 else zero) +/ (if b11 then a01 else zero)
    and c10 = (            a10          ) +/ (if b10 then a11 else zero)
    and c11 = (if b01 then a10 else zero) +/ (if b11 then a11 else zero) 
    in
    a.(0) <- c00; a.(1) <- c01; a.(2) <- c10; a.(3) <- c11) pipe;
  a

let twobees car twobs =
  List.for_all (fun (up,auxp,dn) ->
    let ux = ualue up twobs
    and dx = ualue dn twobs in
    ux.(0) >=/ dx.(0) +/ (if auxp then zero else one) &&
    ux.(1) >=/ dx.(1) &&
    ux.(2) >=/ dx.(2) &&
    ux.(3) >=/ dx.(3)) car

let itwob = [|false,false,true;
	      false,true,true;
	      false,true,false;
	      true,false,false;
	      true,false,true;
	      true,true,false;
	      true,true,true;
	      false,false,false|]

let twopunch lim car =
  let n = tanks car in
  try
    for sum = n to lim * n do
      aiter n 8 sum (fun ff ->
	if twobees car (Array.map (fun x -> itwob.(x-1)) ff) then
	  raise (Get_off_the_bike ff))
    done;
    None
  with Get_off_the_bike ff ->
    Some (Array.map (fun x ->
      let (a01,a10,a11) = itwob.(x-1) in
      [|[|1;                    if a01 then 1 else 0|];
	[|if a10 then 1 else 0; if a11 then 1 else 0|]|]) ff)
	      
