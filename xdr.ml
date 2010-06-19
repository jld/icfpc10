let rec xdr_int = function
    0 -> [0]
  | 1 -> [1]
  | n ->
      let rec be n size a =
	if size == 0 then a
	else be (n / 3) (size - 1) ((n mod 3)::a) in
      let rec loop n size max =
	if n < max then
	  [2;2]@(xdr_int size)@(be n size [])
	else
	  loop (n - max) (size + 1) (max * 3)
      in
      loop (n - 2) 0 1
