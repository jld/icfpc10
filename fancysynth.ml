open Factfind
open Factrun

let rec fancysynth strat trits fpin fpout =
  match trits with
    [] -> Factory.wire fpin fpout
  | trit::trits ->
      match strat trit trits with
	Some (trits, diag) ->
	  let p = Factory.pipe () in
	  to_hdl diag (Factory.P p) fpout;
	  fancysynth strat trits fpin (Factory.P p)
      | None ->
	  failwith "Could not find"

let cheap n trit trits =
  match find_fact_ids n [(0::trits),(trit::trits)] with
    Some diag -> Some (trits, diag)
  | None -> None

(* Found by randomized testing.  Should work. *)
let leading_zero =
  Array.map (fun dst -> ([X; L 0; R 0; L 1; R 1; L 2; R 2], dst))
    [|[L 0; X; L 1; L 2; R 2; R 0; R 1];
      [L 1; R 1; R 2; L 2; R 0; X; L 0];
      [R 1; L 1; L 2; R 2; R 0; X; L 0]|]



let gatel inl inr = tab_left.(inl * 3 + inr)
let gater inl inr = tab_right.(inl * 3 + inr)

let unloop (fs0,fsm,_) ileftp otrits =
  let inl0 = if ileftp then 0 else fs0
  and inr0 = if ileftp then fs0 else 0 in
  let outl0 = gatel inl0 inr0
  and outr0 = gater inl0 inr0 in
  match otrits with
    [] -> failwith "No trits"
  | otrit::otrits when otrit != outl0 -> failwith "First symbol mismatch"
  | _::otrits ->
      let rec loop loopsym otrits ritrits =
	match otrits with 
	  [] -> List.rev ritrits
	| otrit::otrits ->
	    let loopsym = fsm loopsym in
	    let intrit = if ileftp then
	      (* intrit - loopsym = otrit | intrit = otrit + loopsym *)
	      (otrit + loopsym) mod 3
	    else
	      (* loopsym - intrit = otrit | loopsym - otrit = intrit *)
	      (loopsym - otrit + 3) mod 3
	    in
	    let inl = if ileftp then intrit else loopsym 
	    and inr = if ileftp then loopsym else intrit in
	    let outl = gatel inl inr
	    and outr = gater inl inr in
	    if (outl != otrit) then failwith "intrit computation was wrong";
	    loop outr otrits (intrit::ritrits)
      in
      loop outr0 otrits []

let makeloop (fsm_src,fsm_dst) ileftp =
  let n = (List.length fsm_src) / 2 in
  let inpn = if ileftp then L n else R n
  and loopn = if ileftp then R n else L n in
  let rec loop osrc odst nsrc ndst =
    match (osrc,odst) with
      [], [] -> nsrc, ndst
    | X::osrc, X::odst -> loop osrc odst ((R n)::nsrc) (loopn::ndst)
    | X::osrc, d::odst -> loop osrc odst ((R n)::nsrc) (d::ndst)
    | s::osrc, X::odst -> loop osrc odst (s::nsrc) (loopn::ndst)
    | s::osrc, d::odst -> loop osrc odst (s::nsrc) (d::ndst)
    | _, _ -> failwith "List lengths"
  in
  loop fsm_src fsm_dst [L n; X] [X; inpn]



let fsm_pipe = (0,(fun x -> x))
let diag_pipe = [X],[X]

(* pipe false 00/2 01/1 02/0 *)
(* pipe true  00/2 01/0 02/1 *)

let fsm_loop inleftp outleftp =
  ((if outleftp then 0 else 2),
   (let state = ref (if outleftp then 2 else 0) in
   fun insym ->
     let inl = if inleftp then insym else !state
     and inr = if inleftp then !state else insym in
     let outl = gatel inl inr
     and outr = gater inl inr in
     state := if outleftp then outr else outl;
     if outleftp then outl else outr))

let diag_loop inleftp outleftp =
  [X;
   if outleftp then L 0 else R 0;
   if outleftp then R 0 else L 0],
  [if inleftp then L 0 else R 0;
   X;
   if inleftp then R 0 else L 0]
    
(* (loop _ false) false  20/2 21/1 22/0 *)
(* (loop _ false) true   10/2 11/0 12/1 *)
(* (loop _ true) false   00/0 01/2 02/1 *)
(* (loop _ true) true    00/0 01/1 02/2 *)

