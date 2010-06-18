open Factory 

let zero_two zero two =
  let g = gate () in
  wire zero (L g);
  wire (L g) (R g);
  wire (R g) two

let zero_one zero one =
  let two = P (pipe ()) in
  zero_two zero two;
  let g = gate () in
  wire (R g) (L g);
  wire two (R g);
  wire (L g) one

let _ =
  let p0 = pipe () in
  let p1 = pipe () in
  zero_one (P p0) (P p1);
  let g = gate () in
  wire X (R g);
  wire (P p1) (L g);
  wire (R g) X;
  wire (L g) (P p0);
  print ()
  

(* 
L 0 1 2
0 0 2 1
1 1    
2 2   0

R 0 1 2
0 2 2 2
1 2    
2 2   0
*)
