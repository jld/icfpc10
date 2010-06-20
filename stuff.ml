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

let cheaptry stuff = 
  List.map (fun dels ->
    let u = ref 1 and d = ref 1 in
    Array.iteri (fun i del ->
      let edel = let rec loop j a = 
	if j == 0 then a else loop (pred j) (stuff.(i) * a) 
      in loop (abs del) 1 in
      if del > 0 then u := !u * edel else d := !d * edel) dels;
    (!u, !d, dels))


(* 

2708 scratchbook:

0 = abd->b  aabbdd->e
1 = b->c d->a   e->f g->d g->g
2 = c->d        f->g

a0,b0,c0,d0  0->   
a0,abd0,0,0  1->   e=aabbdd0
a0,0,abd0,0  2->   f=aabbdd0
a0,0,0,abd0  1->   g=aabbdd0
aabd0,0,0,0        d,g=aabbdd0

a0,b0,c0,d0,  e0,f0,g0   1->
ad0,0,b0,g0,  0,e0,g0   2->
ad0,0,0,b0,  0,0,e0    1->
abd0,0,0,e0,  0,0,e0   1-> 
abde0,0,0,e0,  0,0,e0   0-> 
abde0,abdee0,0,0 aabbddeeee0,0,0   1->
abd0,0,abde0,0    2->
abd0,0,0,abde0     0->
abd0,aabbdd0,0,0  1->
abd0,0,aabbdd0,0  2->
abd0,0,0,aabbdd0


    
Starting over.

0121 vs. 1211012012

0: a->a a->b      | b->e d->e  | e->b g->b
1: a->a b->c d->a | e->f g->a
2: a->a c->d      | f->g

 0  1  2  1
a->b->c->d->a
b->e->f->g->a
d->e->f->g->a
e->b->c->d->a
g->b->c->d->a

 1  2  1  1  0
b->c->d->a->a->...
d->a->a->a->a->...
e->f->g->a...
g->a->...    0  1  2
           *a->b->c->d

for any v,w,x,y,z st. (0: v->w, 1:w->x y->z, 2:x->y), then
  1 = w->x y->z
  so, when z = a, y->a
  1 2 = w->y
  1 2 1 = w->v
  so, when v = a, w->a

   0  1  2  1
XXX->h->i->j->dg
 hj->e->f->g->dg

 1  2  1   1  0  1  2  0  1  2
h->i->j->dg->a
j->dg
             

let f = 
[|[|[|1;1;0;0;0;0;0;1;0;0|];
    [|0;0;0;0;1;0;0;1;0;0|];
    [|0;0;0;0;0;0;0;0;0;0|];
    [|0;0;0;0;1;0;0;1;0;0|];
    [|0;1;0;0;0;0;0;1;0;0|];
    [|0;0;0;0;0;0;0;0;0;0|];
    [|0;1;0;0;0;0;0;1;0;0|];
    [|0;0;0;0;1;0;0;0;0;0|];
    [|0;0;0;0;0;0;0;0;0;0|];
    [|0;0;0;0;2;0;0;0;0;0|]|];
  [|[|1;0;0;0;0;0;0;0;0;0|];
    [|0;0;1;0;0;0;0;0;0;0|];
    [|0;0;0;0;0;0;0;0;0;0|];
    [|1;0;0;0;0;0;0;0;0;0|];
    [|0;0;0;0;0;1;0;0;0;0|];
    [|0;0;0;0;0;0;0;0;0;0|];
    [|1;0;0;0;0;0;0;0;0;0|];
    [|0;0;0;0;0;0;0;0;1;0|];
    [|0;0;0;0;0;0;0;0;0;0|];
    [|0;0;0;1;0;0;1;0;0;0|]|];
  [|[|1;0;0;0;0;0;0;0;0;0|];
    [|0;0;0;0;0;0;0;0;0;0|];
    [|0;0;0;1;0;0;0;0;0;0|];
    [|0;0;0;0;0;0;0;0;0;0|];
    [|0;0;0;0;0;0;0;0;0;0|];
    [|0;0;0;0;0;0;1;0;0;0|];
    [|0;0;0;0;0;0;0;0;0;0|];
    [|0;0;0;0;0;0;0;0;0;0|];
    [|0;0;0;0;0;0;0;0;0;1|];
    [|0;0;0;0;0;0;0;0;0;0|]|]|]

The chicken, she does not work.

----
22100, anyone?

01210 vs. 1211012012

0: a->b e->a e->d
1: b->c d->e
2: c->d

 0  1  2  1  0
a->b->c->d->e->a
e->a
            e->d

 1  2  1  1  0  1  2  0  1  2
b->c->d->e/
d->e/
                     a->b->c->d
e->d/

let f = 
[|[|[|1;1;0;0;0|];
    [|0;0;0;0;0|];
    [|0;0;0;0;0|];
    [|0;0;0;0;0|];
    [|1;0;0;1;0|]|];
  [|[|1;0;0;0;0|];
    [|0;0;1;0;0|];
    [|0;0;0;0;0|];
    [|0;0;0;0;1|];
    [|0;0;0;0;0|]|];
  [|[|1;0;0;0;0|];
    [|0;0;0;0;0|];
    [|0;0;0;1;0|];
    [|0;0;0;0;0|];
    [|0;0;0;0;0|]|]|]

*)
