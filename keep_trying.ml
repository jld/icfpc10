open Xdr
open Statics

let car = de_car (sbuf Sys.argv.(1))
let msi = int_of_string (Sys.argv.(2))
let imx = int_of_string (Sys.argv.(3))

let tri = if Array.length Sys.argv > 4 then 
  Some (int_of_string (Sys.argv.(4)))
else None

let fuel = keep_trying msi imx tri car

let () = 
  Marshal.to_channel stdout fuel []
