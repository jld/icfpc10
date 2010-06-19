open Factrun

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


let to_hdl (diagout,diagin) fpin fpout =
  let n = (List.length diagin) / 2 in
  let fgs = Array.init n (fun _ -> Factory.gate ()) in (* XXX inorder *)
  let pp px = function
      X -> px
    | L i -> Factory.L (fgs.(i))
    | R i -> Factory.R (fgs.(i))
  in
  List.iter2 (fun pout pin ->
    Factory.wire (pp fpin pout) (pp fpout pin)) diagout diagin

let facprint diag =
  Factory.reset ();
  to_hdl diag Factory.X Factory.X;
  Factory.print ()
