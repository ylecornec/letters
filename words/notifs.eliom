module Notif =
  Eliom_notif.Make_Simple
    (struct
      type identity = int64
      (* let get_identity () = Lwt.return @@ Eliom_reference.Volatile.get User_id_ref.user_id_ref *)
      let get_identity () = Lwt.return 0L
      type key = unit
      type notification = string
    end)


let%server notify v =
  (* Notif.notify ~notfor:`Me (() : Notif.key) v; *)
  Notif.notify (() : Notif.key) v;
  Lwt.return_unit

let%server init () = Notif.init ()
let%client init =
    ~%(Eliom_client.server_function [%json: unit] init)
let%server listen () = Notif.listen (); Lwt.return_unit

let%client listen =
  (* Lwt.async *)
    (* ~%(Eliom_client.server_function [%json: unit] (fun () -> listen (); Lwt.return_unit)) *)
    ~%(Eliom_client.server_function [%json: unit] listen)
         (* (Os_session.connected_wrapper (fun () -> listen (); Lwt.return_unit))) *)

let%server unlisten () = Notif.unlisten (); Lwt.return_unit

let%client unlisten =
  ~%(Eliom_client.server_function [%json: unit] unlisten)

let%server client_ev () : (unit * string) Eliom_react.Down.t Lwt.t =
  Lwt.return @@ Notif.client_ev ()

let%client client_ev =
  ~%(Eliom_client.server_function [%json: unit] client_ev)

