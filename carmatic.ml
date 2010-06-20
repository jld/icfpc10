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

let donelist () =
  List.filter (fun car -> Sys.file_exists ("victory/fuels/"^car))
    (carlist ())

let undonelist () =
  List.filter (fun car -> not (Sys.file_exists ("victory/fuels/"^car)))
    (carlist ())


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

let targets () =
  List.map (fun car -> (car, getcar car)) (undonelist ())


let register_efuel carid fex =
  let fo = open_out ("victory/fuels/"^carid) in
  begin try 
    List.iter (fun trit ->
      output_char fo (char_of_int (trit + 48))) fex;
    output_char fo '\n';
  with 
    e -> close_out fo; raise e
  end;
  close_out fo

let register_fuel carid fuel = 
  register_efuel carid (en_fuel fuel)

let factory_cache = Hashtbl.create 17
let clear_factory_cache () = Hashtbl.clear factory_cache

let factorize_efuel synth carid fuel = 
  register_efuel carid fuel;
  let factory =
    try Hashtbl.find factory_cache fuel
    with Not_found ->
      Factory.reset ();
      synth (Cheapsynth.key @ fuel) Factory.X Factory.X;
      let factory = Factory.sprint () in
      Hashtbl.add factory_cache fuel factory;
      factory
  in
  print_string factory;
  let fo = open_out ("victory/factories/"^carid) in
  begin try
    output_string fo factory
  with
    e -> close_out fo; raise e
  end;
  close_out fo;
  let fr = open_out_gen [Open_wronly; Open_append; Open_creat] 0o666
      "victory/unsubmitted" in
  begin try
    output_string fr (carid^"\n")
  with 
    e -> close_out fr; raise e
  end;
  close_out fr

let factorize_fuel synth carid fuel =
  factorize_efuel synth carid (en_fuel fuel)

let siphon_fuel path : Xdr.fuel =
  let fi = open_in_bin path in
  let fu = Marshal.from_channel fi in
  close_in fi;
  fu

let thaw_fuel carid = 
  de_fuel (sbuf (getoneline ("victory/fuels/"^carid)))

