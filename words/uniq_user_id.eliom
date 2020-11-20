  let user_id_counter = ref 0L

  let new_user_id () =
    let uid = !user_id_counter in
    let () = user_id_counter := Int64.(add uid 1L) in
    uid
