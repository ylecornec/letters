  let uniq_id_counter = ref 0
  let get_uniq_id () =
    let x = !uniq_id_counter in
    let () = incr uniq_id_counter in
    x
