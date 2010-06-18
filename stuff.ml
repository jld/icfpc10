open Factory 

let two_of_zero zero =
  let p = pipe () in
  let (zero', two) = fgate zero (P p) in
  wire zero' (P p);
  two

let one_of_two two =
  let p = pipe () in
  let (one, two') = fgate (P p) two in
  wire two' (P p);
  one

let one_of_zero zero = one_of_two (two_of_zero zero)

let _ =
  let p = pipe () in
  let larg = two_of_zero X 
  and rarg = one_of_zero (P p) in
  let (l, r) = fgate larg rarg in
  wire r X;
  wire l (P p);
  print ()
  

(* 
L 0 1 2
0 0 2 1
1 1 0 2
2 2 1 0

R 0 1 2
0 2 2 2
1 2 0 1
2 2 1 0
*)
