let fparse f =
  let lexbuf = Lexing.from_channel f in
  Factparse.factory Factlex.token lexbuf
