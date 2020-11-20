open Games_manager
open Api_utils

let json_of_current_game () = Lwt.return "<json_of_current_game in not used anymore>"
  (* match Os_current_user.Opt.get_current_userid () with
   * | None -> Lwt.return "<not logged in>"
   * | Some my_id ->
   *    let%lwt gm = Eliom_reference.get global_games_manager in
   *    match game_of_user gm my_id with
   *      None -> Lwt.return "<game not found>"
   *    | Some (game_id, game) -> Lwt.return @@ Yojson.Safe.to_string (Game.to_yojson game) *)


(* let get_current_game_state_service =
 *   Eliom_service.create
 *     ~path:(Eliom_service.Path ["current_game"])
 *     ~meth:(Eliom_service.Get Eliom_parameter.unit)
 *     ()
 * 
 * let () =
 *   let open Eliom_content.Html.D in
 *   let current_game_state_page_handler myid_o page () =
 *     let%lwt current_game_str = json_of_current_game () in
 *     Words_container.page
 *       myid_o
 *       [h1 [txt "Current game :"];
 *        txt current_game_str]
 *   in
 *   Words_base.App.register
 *     ~service:get_current_game_state_service
 *     (Words_page.Opt.connected_page current_game_state_page_handler) *)


let current_game_json =
  Eliom_service.create
    ~path:(Eliom_service.Path ["current_game_json"])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

let current_game_json_handler () =
  match Eliom_reference.Volatile.get User_id_ref.user_id_ref with
  | None ->
     send_error ~code:401 "Not logged in"
  | Some my_id ->
     let%lwt gm = Eliom_reference.get global_games_manager in
     match game_of_user gm my_id with
       None -> send_error ~code:400 @@ Format.asprintf "User %Lu is not in game" my_id
     | Some (game_id, game) ->
        send_json ~code:200  (Yojson.Safe.to_string (Game.view_to_yojson @@ Game.view Default my_id game))

let () =
  Eliom_registration.Any.register current_game_json (fun () -> current_game_json_handler)
    (* (fun () -> (Os_session.connected_wrapper current_game_json_handler)) *)
