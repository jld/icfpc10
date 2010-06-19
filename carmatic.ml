open Xdr
open Statics

let getlines path =
  let cr = ref []
  and fi = open_in path in
  let rec loop () =
    if begin try
      cr := (input_line fi)::!cr; true
    with
      End_of_file -> false
    | e -> close_in fi; raise e
    end then loop ()
  in loop ();
  close_in fi;
  List.rev !cr

let carlist () =
  getlines "remote/carlist"


let getoneline path =
  let fi = open_in path in
  try 
    let xcar = input_line fi in
    close_in fi;
    xcar
  with
    e -> close_in fi; raise e

let getcar car =
  de_car (sbuf (getoneline ("remote/cars/"^car)))


let allcars () =
  List.map (fun car -> (car, getcar car)) (carlist ())
