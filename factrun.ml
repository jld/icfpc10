
type gate = {
    mutable in_left: int ref; mutable in_right: int ref;
    out_left: int ref; out_right: int ref }

type factory = {
    out_world: int ref;
    gates: gate array;
    mutable in_world: int ref }

type port = 
    X
  | L of int
  | R of int

let gate () = 
  let outl = ref 0
  and outr = ref 0 in
  { out_left = outl; out_right = outr;
    in_left = outl; in_right = outr }

let factory n =
  let out = ref 0 in
  { out_world = out;
    gates = Array.init n (fun _ -> gate ());
    in_world = out }

let wire f po pi =
  let r = match po with
    X -> f.out_world;
  | L n -> f.gates.(n).out_left
  | R n -> f.gates.(n).out_right
  in
  match pi with
    X -> f.in_world <- r
  | L n -> f.gates.(n).in_left <- r
  | R n -> f.gates.(n).in_right <- r

let checkwire f po pi =
  let r = match po with
    X -> f.out_world;
  | L n -> f.gates.(n).out_left
  | R n -> f.gates.(n).out_right
  in
  if begin match pi with
    X -> f.in_world != r
  | L n -> f.gates.(n).in_left != r
  | R n -> f.gates.(n).in_right != r
  end then failwith "Gate mismatch"

let tab_left =
  [|0; 2; 1;
    1; 0; 2;
    2; 1; 0|]
and tab_right =
  [|2; 2; 2;
    2; 0; 1;
    2; 1; 0|]

let step f in_trit =
  f.out_world := in_trit;
  Array.iter begin fun g -> (* XXX iter may be slow *)
    let inl = !(g.in_left)
    and inr = !(g.in_right)
    in
    let idx = inl * 2 + inl + inr
    in
    let outl = tab_left.(idx)
    and outr = tab_right.(idx)
    in
    g.out_left := outl;
    g.out_right := outr
  end f.gates;
  !(f.in_world)
    
let run f ins =
  let outs = ref [] in
  List.iter (fun i -> outs := (step f i)::!outs) ins;
  List.rev !outs

let reset f =
  f.out_world := 0;
  Array.iter (fun g ->
    g.out_left := 0;
    g.out_right := 0) f.gates

let size f =
  Array.length f.gates

