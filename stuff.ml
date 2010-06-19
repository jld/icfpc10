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

let keyin = [0;2;2;2;2;2;2;0;2;1;0;1;1;0;0;1;1]

let srvin = [0;1;2;0;2;1;0;1;2;1;0;2;0;1;2;0;2]
let key =   [1;1;0;2;1;2;1;0;1;1;2;1;0;1;2;2;1]

let notkey = [1; 0; 2; 2; 1; 2; 2; 0; 0; 0; 2; 0; 1; 1; 0; 1; 1]


let perms l =
  let rec pa l r = match l with
    [] -> (match r with [] -> [[]] | _ -> [])
  | h::t -> (List.map (fun x -> h::x) (pa (r@t) []))@(pa t (h::r))
  in pa l []


(*



*)
