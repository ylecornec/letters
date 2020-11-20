let () = (Unix.time()) |> int_of_float |> Random.init
let char () = (Char.chr (97 + (Random.int 26)))
let string length =
  String.init length (fun _ -> char ())
