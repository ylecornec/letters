open Lobby_utils.Lobbies
open%shared Shared_types
[%%shared
    open Eliom_content.Html
    open Eliom_content.Html.D
]

(* Services *)
(* let main_service = Eliom_service.create
 *                      ~path:(Eliom_service.Path ["new_lobby"])
 *                      ~meth:(Eliom_service.Get Eliom_parameter.unit)
 *                      () *)

let%server service = Eliom_service.create
                     ~path:(Eliom_service.Path ["lobby"])
                     ~meth:(Eliom_service.Get Eliom_parameter.(suffix @@ string "lobby_id"))
                     ()

let%client service = ~%service

let%shared page_class = "game-lobby"
(* let connection_service = Eliom_service.create_attached_post
 *                            ~fallback:main_service
 *                            ~post_params:Eliom_parameter.(string "name" ** string "password")
 *                            () *)

(* let make_game_service = Eliom_service.create_attached_post
 *                           ~fallback:main_service
 *                           ~post_params:Eliom_parameter.(unit)
 *                           () *)


(* let new_game_box () = Eliom_content.Html.D.(
 *     Form.post_form ~service:make_game_service
 *       (fun () ->
 *         [fieldset
 *            [Form.input
 *               ~input_type:`Submit ~value:"New game"
 *               Form.string
 *       ]]) ()
 *                       ) *)

(* let user_service  = Eliom_service.create
 *   ~path:(Eliom_service.Path ["users"])
 *   ~meth:(Eliom_service.Get Eliom_parameter.(suffix (string "name")))
 *   ()
 * 
 * let user_links = Eliom_content.Html.D.(
 *   let link_of_user = fun (name, _) ->
 *     li [a ~service:user_service [txt name] name]
 *   in
 *   fun () -> ul (List.map link_of_user !users)
 * ) *)

let display_user_name name =
  let open Eliom_content.Html.F in
  match name with
  | None -> p [txt ("you are not connected")]
  | Some user ->
     p
       [ txt "you are "
       ; em [txt (Os_user.fullname_of_user user)] ]

let display_user_id =
  let open Eliom_content.Html.F in
  function
  | None -> p [txt "log in please"]
  | Some userid ->
     p
       [ txt "your_user_id "
       ; em [txt (Int64.to_string userid)] ]



(* let%client handle_notif_message_list rmessages (_, msgid) =
 *   Eliom_shared.ReactiveData.RList.cons msgid (snd rmessages) *)

(* let%server lobby_view_0 (s : string) : Html_types.div Eliom_content.Html.D.elt  = *)
let%server lobby_view_0 (s : string) : _ =
  Lobby_utils.lobby_view s

let%server ping ((lobby_id , user_id) : (string * int64)) : unit Lwt.t =
  Lwt.return @@ Lobby_utils.ping lobby_id user_id

let%client ping: (string * int64) -> unit Lwt.t =
  ~%(Eliom_client.server_function [%json: string * int64] ping)

let%server set_nick ((lobby_id , user_id, nick) : (string * int64 * string)) : unit Lwt.t =
  Lwt.return @@ Lobby_utils.ping_and_set_nick lobby_id user_id nick

let%client set_nick: (string * int64 * string) -> unit Lwt.t =
  ~%(Eliom_client.server_function [%json: string * int64 * string] set_nick)

(* let%server lobby_view (lobby_id:string) : Html_types.div Eliom_content.Html.D.elt Lwt.t = *)
let%server lobby_view (lobby_id:string) =
    Lwt.return @@ lobby_view_0 lobby_id

(* let%client lobby_view : string -> Html_types.div_content_fun Eliom_content.Html.D.elt Lwt.t =
 *   ~%(Eliom_client.server_function [%json: string] (Os_session.connected_rpc (fun id lobby_id -> lobby_view_aux lobby_id))) *)

let%server lobby_view_rpc =
  Eliom_client.server_function
    [%json: string]
    (* (Os_session.connected_rpc (fun userid value -> lobby_view value)) *)
    lobby_view

let%client lobby_view = ~%lobby_view_rpc

let%client launch_new_game =
  ~%(
      Eliom_client.server_function
        [%json: string]
        (* (Os_session.connected_rpc (fun userid lobby_id -> Lobby_utils.launch_game lobby_id)) *)
        Lobby_utils.launch_game
    )

(* let%shared display_message lobby_id (i: int) =
 *   let%lwt s = lobby_view lobby_id in
 *   Lwt.return @@ s *)

let%shared display_message_unit lobby_id i =
  let%lwt s = lobby_view lobby_id in
  Lwt.return s

let%shared
  (s : int Eliom_shared.React.S.t),
  (f : (?step:React.step -> int -> unit) Eliom_shared.Value.t)
  =
  Eliom_shared.React.S.create 0

let%client incr_s () =
  let v = Eliom_shared.React.S.value ~%s in
  ~%f (v)

let%client handle_notif s (_key, notif) =
  let () = Js_of_ocaml.Firebug.console##log(Js_of_ocaml.Js.string "handle notif") in
  let open Lobby_utils in
  match notif with
  | Update_please ->
     let v = Eliom_shared.React.S.value ~%s in
     ~%f (v + 1)
  | Redirect path ->
     let base_url = "http://"^Js_of_ocaml.Js.to_string @@ Js_of_ocaml.Dom_html.window##.location##.host in
     Lwt.async @@(fun () -> Eliom_client.change_page_uri ~replace:true @@ base_url^"/"^path)


let%shared display_players lobby_id =
  let%lwt () = Lobby_utils.init () in
  let%lwt () = Lobby_utils.listen () in
  let%lwt ev = Lobby_utils.client_ev () in
  let (_ : unit Eliom_client_value.t) =
  [%client
          let () = Js_of_ocaml.Firebug.console##log(Js_of_ocaml.Js.string "calling display player") in
    (ignore
       (React.E.map (handle_notif ~%s)
          (* ~%(Lobby_utils.client_ev () : (unit * Lobby_utils.lobby_notification) Eliom_react.Down.t)) *)
          ~%ev)
     : unit)
  ] in

  let%lwt content =
    Eliom_shared.React.S.Lwt.map_s
      [%shared display_message_unit ~%lobby_id]
      s
  in
  Lwt.return content
  (* Lwt.return (R.div content) *)

let%shared new_game_button (lobby_id: string) =
  let new_game_button = D.button [txt "Launch new game"] in
  let (_ : unit Eliom_client_value.t) = [%client
    Lwt.async (fun () ->
        Js_of_ocaml_lwt.Lwt_js_events.clicks (Eliom_content.Html.To_dom.of_element ~%new_game_button)
          (fun _ _ -> launch_new_game ~%lobby_id; Lwt.return ()))
           ]
  in
  new_game_button


let%shared make_change_nick_form lobby_id uid =
              let input = D.Form.input ~input_type:`Text Form.string in
              let onclick_handler =
                [%client (fun _ ->
                      let open Js_of_ocaml in
                      let nick =
                        Js.to_string
                          (Eliom_content.Html.To_dom.of_input ~%input)##.value
                      in
                      Lwt.async (fun () -> set_nick(~%lobby_id, ~%uid, nick))
                        (* Dom_html.window##alert(Js.string ("Input value :" ^ v)) *)
                    )
                ]
              in
              let button =
                button ~a:[a_onclick onclick_handler] [txt "Change nickname"]
              in
    Lwt.return @@
      (* html
       *   (head (title (txt ("Lobby"^lobby_id))) [])
       *   (body *)
           [input;
            button;]
        (* ) *)

let%shared display_lobby_when_id_set myid my_nick =
  let open Eliom_content.Html.F in
  (fun lobby_id ->
    let%lwt () =
      let%lwt () =
        match my_nick with
        | Some my_nick -> set_nick (lobby_id, myid, my_nick)
        | None -> ping(lobby_id, myid)
      in
       let (_ : unit Eliom_client_value.t) =
         [%client
          let () = Js_of_ocaml.Firebug.console##log(Js_of_ocaml.Js.string "redisplayed") in
              let () = Js_of_ocaml.Firebug.console##log(Js_of_ocaml.Js.string @@ Int64.to_string ~%myid) in
              let () = Js_of_ocaml.Firebug.console##log(Js_of_ocaml.Js.string ~%lobby_id) in
              let ping_cb =
                Js_of_ocaml.Js.wrap_callback
                  (fun () -> Lwt.async (fun () ->
                                 let () = Js_of_ocaml.Firebug.console##log(Js_of_ocaml.Js.string "ping") in
                                 ping(~%lobby_id, ~%myid)))
              in
              let ping_interval =
                Js_of_ocaml.Dom_html.window##setInterval
                  ping_cb
                  10000.
              in
              Eliom_client.Page_status.ondead
                (fun () -> Js_of_ocaml.Dom_html.window##clearInterval ping_interval)
         ]
       in
       Lobby_utils.send_update_please ()
    in
    let%lwt messages = display_players lobby_id in
    let%lwt change_nick = make_change_nick_form lobby_id myid in
    let new_game_button = new_game_button lobby_id in
    Lwt.return @@
      (* html
       *   (head (title (txt ("Lobby"^lobby_id))) [])
       *   (body *)
           [
        (* txt @@ Int64.to_string myid; *)
        div @@ change_nick;
        div [R.node messages];
        new_game_button;
           ]
        (* ) *)
  )

let%shared display_lobby_no_login =
  let open Eliom_content.Html.F in
  (fun lobby_id ->

    match%lwt User_id_ref.get_id () with
    | None ->
       let%lwt new_id = User_id_ref.set_new_id () in
       let nick = "nick " ^ Int64.to_string new_id in
       display_lobby_when_id_set new_id (Some nick) lobby_id
    | Some uid -> display_lobby_when_id_set uid None lobby_id
  )

(* let%server display_lobby =
 *   let open Eliom_content.Html.F in
 *   (fun lobby_id ->
 *     let myid_o = Os_current_user.Opt.get_current_userid () in
 *     let me_o = Os_current_user.Opt.get_current_user () in
 *     let%lwt () =
 *       match myid_o, me_o with
 *     | None, _ -> Lwt.return ()
 *     | Some myid, Some my_nick ->
 *        (\* let%lwt () = ping (lobby_id,myid) in *\)
 *        let%lwt () = set_nick (lobby_id, myid, Os_user.firstname_of_user my_nick) in
 *        let (_ : unit Eliom_client_value.t) =
 *          [%client
 *           let ping_cb =
 *             Js_of_ocaml.Js.wrap_callback
 *               (fun () -> Lwt.async (fun () -> ping(~%lobby_id, ~%myid)))
 *           in
 *               ignore @@ Js_of_ocaml.Dom_html.window##setInterval
 *                           ping_cb
 *                           10000.
 *          ]
 *        in
 *        let () = Lobby_utils.Lobby_update_notif.notify () Lobby_utils.Update_please in
 *        Lwt.return ()
 *     | Some _, None -> failwith "user should have an id"
 *     in
 *     let%lwt messages = display_players lobby_id in
 *     (\* let new_game_button = new_game_button lobby_id in *\)
 *     Lwt.return
 *       [
 *         div [R.node messages];
 *         (\* new_game_button; *\)
 * 
 *   ]) *)
(* let%server toto =
 *   let open Eliom_content.Html.F in
 *   (fun () ->
 *     let myid_o = Os_current_user.Opt.get_current_userid () in
 *     let me_o = Os_current_user.Opt.get_current_user () in
 *     let%lwt game_create_msg =
 *       match myid_o with
 *     | None -> Lwt.return "you must be logged in to launch a game"
 *     | Some myid ->
 *        let%lwt gm = get_games_manager () in
 *        let game_o = request_game gm myid in
 *        let msg = match game_o with
 *          | New_game_error e, gm ->
 *             let%lwt () = set_games_manager gm in
 *             Lwt.return e
 *          | New_game_id gid, gm ->
 *             let%lwt () = set_games_manager gm in
 *             Lwt.return @@ Format.asprintf "game id = %a" Games_manager.pp_game_id gid
 *          | Waiting_for_game, gm ->
 *             let%lwt () = set_games_manager gm in
 *             Lwt.return @@ Format.asprintf "waiting for opponent"
 *        in
 *        msg
 *     in
 *     Lwt.return
 *       [
 *         h1 [txt game_create_msg] ;
 *         display_user_name me_o ;
 *         display_user_id myid_o
 *   ]) *)


(* let () =
 *   let open Eliom_content.Html.D in
 *   let detail_page_handler myid_o page () =
 *     let ngb = new_game_box () in
 *     Words_container.page
 *       myid_o
 *       [h1 [txt "New game"];
 *        ngb;
 *       ]
 *   in
 *   Words_base.App.register
 *     ~service:main_service
 *     (Words_page.Opt.connected_page detail_page_handler) *)


let%shared name () = "New game lobby"
let%shared page = display_lobby_no_login 

(* let%server () =
 *   let _detail_page_handler myid_o lobby_id () =
 *     let%lwt lobby = display_lobby lobby_id in
 *     Words_container.page
 *       myid_o
 *       lobby
 *   in
 *   Words_base.App.register
 *     ~service
 *     (fun lobby_id () -> display_lobby_no_login lobby_id) *)

    (* (Words_page.Opt.connected_page _detail_page_handler) *)

(* let%server () =
 *   let detail_page_handler myid_o page () =
 *     let%lwt toto_content = toto () in
 *     Words_container.page
 *       myid_o
 *       toto_content
 *       (\* (Demo_pagetransition.make_detail_page page ()) *\)
 *   in
 *   Words_base.App.register
 *     ~service:make_game_service
 *     (Words_page.Opt.connected_page detail_page_handler) *)

let%server () =
    Words_base.App.register ~service
      ( Words_page.Opt.connected_page @@ fun myid_o lobby_id () ->
        let%lwt p = page lobby_id in
        Words_container.page ~a:[a_class [page_class]] myid_o p )
