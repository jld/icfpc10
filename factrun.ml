
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

(* *)

let rec do_perms f l r a =
  match (l,r) with
    ([],[]) -> f (List.rev a)
  | ([],_) -> ()
  | (h::t,_) ->
      do_perms f (r@t) [] (h::a);
      do_perms f t (h::r) a

let all_factories n f =
  let fact = factory n in
  let stuff = X::(Array.fold_right (@) (Array.init n (fun i -> [L i; R i])) [])
  in
  do_perms begin fun stuff' ->
    List.iter2 (wire fact) stuff stuff';
    reset fact;
    f (stuff,stuff') fact
  end (List.tl stuff) [List.hd stuff] []

let find_fact_k n tests k =
  all_factories n (fun diag fact ->
    let rec xloop tests =
      match tests with 
	[] -> true
      | ((trin,trout)::tests) ->
	  let rec loop trin trout =
	    match (trin,trout) with
	      ([],_) | (_,[]) -> true
	    | (tri::trin,tro::trout) ->
		let tro' = step fact tri in
		(* Printf.eprintf "BEES %d %d %d\n%!" tri tro tro'; *)
		if tro != tro' then false
		else loop trin trout
	  in 
	  loop trin trout && xloop tests
    in
    if xloop tests then begin
      k diag
    end (* else Printf.eprintf "."*))

let find_facts n tests =
  let stuff = ref [] in
  find_fact_k n tests (fun diag->
    Printf.eprintf "YES!\n%!";
    stuff := diag::(!stuff));
  List.rev !stuff


exception Done
let find_fact n tests = 
  let foo = ref None in
  begin try 
    find_fact_k n tests (fun diag ->
      foo := Some diag;
      raise Done)
  with
    Done -> ()
  end;
  !foo

let rec find_fact_ids n tests =
  if n <= 0 then None
  else match find_fact_ids (pred n) tests with
    None -> find_fact n tests 
  | yay -> yay

let junk n =
  Array.to_list (Array.init n (fun _ -> Random.int 3))

let junks n m =
  Array.to_list (Array.init n (fun _ -> junk m))
