type fuel = int array array array
type car = (int list * bool * int list) list

let en_be size n =
  let rec loop n size a =
    if size == 0 then a
    else loop (n / 3) (size - 1) ((n mod 3)::a) in
  loop n size []

let rec en_cnt = function
    0 -> [0]
  | 1 -> [1]
  | n ->
      let rec loop n size max =
	if n < max then
	  [2;2]@(en_cnt size)@(en_be size n)
	else
	  loop (n - max) (size + 1) (max * 3)
      in
      loop (n - 2) 0 1

let en_num n =
  let rec loop n size max =
    if n < max then
      (en_cnt size)@(en_be size n)
    else
      loop (n - max) (size + 1) (max * 3)
  in loop n 0 1

let en_list f l =
  (en_cnt (List.length l))@
  (List.fold_right (fun e c -> (f e)@c) l [])

let en_array f a =
  (en_cnt (Array.length a))@
  (Array.fold_right (fun e c -> (f e)@c) a [])

let en_fuel = en_array (en_array (en_array en_num))

let en_pipe = en_list en_num
let en_bool b = en_num (if b then 1 else 0)
let en_car = en_list (fun (up,auxp,dn) ->
  (en_pipe up)@(en_bool auxp)@(en_pipe dn))

(* *)

type sbuf = { string: string; mutable offset: int }

let sbuf s = { string = s; offset = 0 }

let de_trit sb =
  let ch = sb.string.[sb.offset] in
  sb.offset <- succ sb.offset;
  (int_of_char ch) - 48

let de_be sb n =
  let rec loop a n =
    if n == 0 then a
    else loop (a * 3 + (de_trit sb)) (pred n)
  in loop 0 n

let pyramid n =
  let rec loop a i pyr =
    if i >= n then a
    else loop (a + pyr) (succ i) (pyr * 3)
  in loop 0 0 1

let rec de_cnt sb =
  match de_trit sb with
    0 -> 0
  | 1 -> 1
  | 2 -> (match de_trit sb with
      2 ->
	let size = de_cnt sb in
	2 + pyramid size + de_be sb size
    | _ -> failwith "Invalid count")
  | _ -> failwith "Not a trit"

let de_num sb =
  let size = de_cnt sb in
  pyramid size + de_be sb size

let de_array f sb =
  let n = de_cnt sb in
  Array.init n (fun _ -> (* XXX inorder *)
    f sb)

let de_list f sb =
  Array.to_list (de_array f sb)


let de_pipe = de_list de_num
let de_bool sb = (de_num sb) != 0

let de_chamber sb = 
  let up = de_pipe sb in
  let auxp = de_bool sb in
  let dn = de_pipe sb in
  (up, auxp, dn)

let de_car = de_list de_chamber

let de_fuel = de_array (de_array (de_array de_num))
  
