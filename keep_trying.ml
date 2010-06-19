open Xdr
open Statics
open Cheapsynth

let nfu = int_of_string (Sys.argv.(1))
let msi = int_of_string (Sys.argv.(2))
let imx = int_of_string (Sys.argv.(3))

let car = de_car (sbuf Sys.argv.(4))
let fuel = keep_trying car nfu msi imx

let _ = 
  go (key @ (en_fuel fuel))
