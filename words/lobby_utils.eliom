
open Shared_types
(* module CMap = Core.Map
 * 
 * type nicks_map = (Core.Int64.t, string, Core.Int64.comparator_witness) CMap.t
 * 
 * let pp_core_map key_module value_sexp_of_t fmt m = 
 *   Format.fprintf fmt "%s" @@
 *     Core.Sexp.to_string
 *       (CMap.sexp_of_m__t key_module value_sexp_of_t m)
 * 
 * let pp_last_ping_map fmt m =
 *   let sexp_of_t = Core.Tuple.T2.sexp_of_t Core.Float.sexp_of_t Core.String.sexp_of_t in
 *   pp_core_map (module Core.Int64) sexp_of_t fmt m
 * 
 * let pp_nicks_map fmt m =
 *   pp_core_map (module Core.Int64) (Core.String.sexp_of_t) fmt m *)

(* module LastPings = Map.Make(Int64) *)
module Lobbies = Map.Make(String)
type user_id = int64
type player_info =
  {last_ping: float;
   mutable detected_inactive: bool; (** true if user was previously detected as inactive *)
  nick: string}

module Players = Map.Make(Int64)

type lobby = {
    gm : user_id;
    players : player_info Players.t;
  }
(* let pp_lobby fmt lobby =
 *   Format fmt "gm=%Lu, pings=%a" gm lobby.users_last_ping *)

let update_ping uid player_info =
  match player_info with
  | None -> Some {detected_inactive = false; last_ping = Unix.time(); nick = Int64.to_string uid}
  | Some player_info -> Some {player_info with last_ping = Unix.time()}

let update_nick uid nick player_info =
  match player_info with
  | None -> Some {detected_inactive = false; last_ping = Unix.time(); nick}
  | Some player_info -> Some {player_info with last_ping = Unix.time(); nick}

let record_ping (uid:user_id) lobby =
  {lobby with players = Players.update uid (update_ping uid) lobby.players}

let record_nick (uid:user_id) (nick:string) lobby =
  {lobby with players = Players.update uid (update_nick uid nick) lobby.players}

let new_lobby (gm:user_id) (gm_nick: string)=
  {gm; players = Players.empty}
  |> record_ping gm
  |> record_nick gm gm_nick

(* let empty = Map.empty (module String) *)
let lobbies = ref Lobbies.empty

type%shared lobby_notification = Update_please | Redirect of string

[%%server

module Lobby_update_notif = Eliom_notif.Make_Simple (struct
  type identity = int64
  let get_identity () = Lwt.return 0L
  type key = unit
  type notification = lobby_notification
end)
]

let inactive_player player_info =
  Unix.time() -. player_info.last_ping > 10.

let remove_inactive lobby =
  let exception Break in
  try
  Players.iter
    (fun _id infos ->
      let inactive = inactive_player infos in
      if inactive && not infos.detected_inactive
         || infos.detected_inactive && not inactive
      then
        let () = infos.detected_inactive <- inactive in
        let () = Lobby_update_notif.notify () Update_please in
        raise Break
    )
    lobby.players
  with
  | Break -> ()


let remove_all_inactive () =
  Lobbies.iter
    (fun k lobby -> remove_inactive lobby)
    !lobbies

let fork_remove_inactive_thread () =
  while%lwt true do
    let () = remove_all_inactive () in
    let%lwt () = Lwt_unix.sleep 5. in
    Lwt.return ()
  done

let () = Lwt.async fork_remove_inactive_thread

let ping lobby_id uid =
  try
    let lobby = Lobbies.find lobby_id !lobbies in
    lobbies := Lobbies.add lobby_id (record_ping uid lobby) !lobbies
  with
  | Not_found ->
     lobbies := Lobbies.add lobby_id (new_lobby uid @@ Int64.to_string uid) !lobbies

let ping_and_set_nick lobby_id uid nick =
  let () = try
    let lobby = Lobbies.find lobby_id !lobbies in
    lobbies := Lobbies.add lobby_id (record_nick uid nick lobby) !lobbies
  with
  | Not_found ->
     lobbies := Lobbies.add lobby_id (record_nick uid nick (new_lobby uid nick)) !lobbies
  in
  Lobby_update_notif.notify () Update_please


(* let show_lobby lobby_id =
 *   try
 *     Format.asprintf "%a" pp_lobby @@ Lobbies.find lobby_id !lobbies
 *   with
 *   | Not_found -> Format.asprintf "lobby %s not found" lobby_id *)


let nick_of_id lobby (user_id: user_id) : string =
    match Players.find_opt user_id lobby.players with
    | Some {nick} -> nick
    | None -> Int64.to_string user_id

open Eliom_content.Html
open Eliom_content.Html.D

let list_of_players lobby =
    Players.fold
      (fun id _ acc -> id::acc)
      lobby.players
      []

let players_div lobby =
  let players =
    Players.fold
      (fun _ pl_info acc ->
        if not @@ inactive_player pl_info then
        (li @@ [txt pl_info.nick])::acc
        else
          acc
      )
      lobby.players
      []
  in
  div @@ [ul players]

let lobby_view (lobby_id:string) : Html_types.div Eliom_content.Html.D.elt =
  try
    let l = Lobbies.find lobby_id !lobbies in
    let gm_nick = nick_of_id l l.gm in
    div [
      h3 @@ [txt "game master : "; txt gm_nick];
      players_div l;
    ]
  with
    | Not_found -> div [txt @@ Format.asprintf "lobby %s not found" lobby_id]

let game_path game_id = "game"

let launch_game lobby_id : Shared_types.ask_new_game_response Lwt.t =
  try
    let lobby = Lobbies.find lobby_id !lobbies in
    let res = Games_manager.start_new_game lobby.gm (list_of_players lobby) in
    match%lwt res with
    | New_game_id game_id ->
       let () =
         Lobby_update_notif.notify () @@
           Redirect (game_path game_id)
       in
       res
    | Waiting_for_game | New_game_error _ -> res

  with
  | Not_found -> Lwt.return @@ Shared_types.New_game_error "lobby not found"



let%server send_redirect path =
 Lwt.return @@ Lobby_update_notif.notify () (Redirect path)

let%client send_redirect =
    ~%(Eliom_client.server_function [%json: string] send_redirect)

let%server send_update_please () =
 Lwt.return @@ Lobby_update_notif.notify () Update_please

let%client send_update_please =
  ~%(Eliom_client.server_function [%json: unit] send_update_please)

let%server init () = Lobby_update_notif.init ()
let%client init =
    ~%(Eliom_client.server_function [%json: unit] init)
let%server listen () = Lobby_update_notif.listen (); Lwt.return_unit

let%client listen =
  (* Lwt.async *)
    (* ~%(Eliom_client.server_function [%json: unit] (fun () -> listen (); Lwt.return_unit)) *)
    ~%(Eliom_client.server_function [%json: unit] listen)
         (* (Os_session.connected_wrapper (fun () -> listen (); Lwt.return_unit))) *)

let%server unlisten () = Lobby_update_notif.unlisten (); Lwt.return_unit

let%client unlisten =
  ~%(Eliom_client.server_function [%json: unit] unlisten)

let%server client_ev () : (unit * lobby_notification) Eliom_react.Down.t Lwt.t =
  Lwt.return @@ Lobby_update_notif.client_ev ()

let%client client_ev =
  ~%(Eliom_client.server_function [%json: unit] client_ev)

let%server rec new_lobby_name () =
  let lobby_name = Generate_random.string 10 in
  match Lobbies.find_opt lobby_name !lobbies with
  | None -> Lwt.return lobby_name
  | Some _ -> new_lobby_name ()

let%client new_lobby_name =
  ~%(Eliom_client.server_function [%json: unit] new_lobby_name)
