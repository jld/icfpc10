type fuel = int array array array
type car = (int list * bool * int list) list

let bigend size n =
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
	  [2;2]@(en_cnt size)@(bigend size n)
	else
	  loop (n - max) (size + 1) (max * 3)
      in
      loop (n - 2) 0 1

let en_num n =
  let rec loop n size max =
    if n < max then
      (en_cnt size)@(bigend size n)
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

