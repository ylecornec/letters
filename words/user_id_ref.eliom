let user_id_ref: int64 option Eliom_reference.Volatile.eref =
  Eliom_reference.Volatile.eref ~scope:Eliom_common.default_session_scope None

let%server get_id () : int64 option Lwt.t =
  Lwt.return @@ Eliom_reference.Volatile.get user_id_ref

let%client get_id =
  ~%(Eliom_client.server_function [%json: unit] get_id)

let%server set_new_id () : int64 Lwt.t =
  let new_id = Uniq_user_id.new_user_id () in
  let () = Eliom_reference.Volatile.set user_id_ref (Some new_id) in
  Lwt.return new_id

let%client set_new_id =
  ~%(Eliom_client.server_function [%json: unit] set_new_id)
