open Factfind
open Factory

let rec cheapsynth trits fpin fpout =
  match trits with
    [] -> wire fpin fpout
  | trit::trits ->
      match find_fact_ids 4 [(0::trits),(trit::trits)] with
	Some diag ->
	  let p = pipe () in
	  to_hdl diag (P p) fpout;
	  cheapsynth trits fpin (P p)
      | None ->
	  failwith "Could not find"

let key = [1;1;0;2;1;2;1;0;1;1;2;1;0;1;2;2;1]

let go key =
  Factory.reset ();
  cheapsynth key X X;
  print ()
