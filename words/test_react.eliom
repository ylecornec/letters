[%%client
 open Js_of_ocaml_lwt
 open Js_of_ocaml
]

let%server service =
  Eliom_service.create
    ~path:(Eliom_service.Path ["game"])
    ~meth:(Eliom_service.Get Eliom_parameter.unit) ()

(* Make service available on the client *)
let%client service = ~%service
type%shared messages = int [@@deriving json]
let%server bus = Eliom_bus.create [%json: messages]
let%client write_to_bus = Eliom_bus.write ~%bus

let%client _ =
  Js.export "global_configuration"
    (object%js
       method add x y = x +. y
       method abs x = abs_float x
       method write_to_bus x = write_to_bus x
       val zero = 0.
     end)

(* let%client parse_view = Game.view_of_json
 * let%client _ = Js.export "parse_view" parse_view *)

let%client _ =
  Js.export "my_event" @@ Js.Unsafe.js_expr "new Event('state_change')"

(* Name for demo menu *)
let%shared name () = "game"
(* Class for the page containing this demo (for internal use) *)
let%shared page_class = "os-page-demo-spinner"

(* Page for this demo *)
open%shared Eliom_content.Html.D (* provides functions to create HTML nodes *)

let%shared reason_react_script =
  let open Eliom_content.Html.D in
  js_script
    ~uri:(make_uri (Eliom_service.static_dir ())
            ["words_client/bundleOutput/index.js"])
    ()

(* let%client init_client () =
 *   let () = Js_of_ocaml.Firebug.console##log(Js_of_ocaml.Js.string "init client called") in
 *   Lwt.async (fun () ->
 *       Lwt_stream.iter
 *         (fun i ->
 *          (Dom_html.window##alert
 *             (Js.string @@ Printf.sprintf "recieved %d" i)
 *           : unit);)
 *         (Eliom_bus.stream ~%bus)) *)

(* let%server () =
 *   Os_session.on_start_process (fun _ ->
 *       let e : (unit * string) Eliom_react.Down.t = Notifs.Notif.client_ev () in
 *       ignore
 *         [%client
 *           (ignore
 *            @@ React.E.map
 *                 (fun (e, msg) ->
 *                   Js_of_ocaml.Firebug.console##log("recieved notif");
 *                   Dom_html.document##dispatchEvent(Js.Unsafe.js_expr "my_event"))
 *                 ~%e
 *             : unit)];
 *       Lwt.return_unit) *)

let%shared register_update_notifs () =
  let%lwt () = Notifs.init () in
  let%lwt () = Notifs.listen () in
  let (_ : unit Eliom_client_value.t) =
    [%client Eliom_client.Page_status.ondead (fun () -> Lwt.async Notifs.unlisten)]
  in
  let%lwt e = Notifs.client_ev () in
  (* let e : (unit * string) Eliom_react.Down.t = Notifs.Notif.client_ev () in *)
  let (_ : unit Eliom_client_value.t) =
    [%client
          let () = Js_of_ocaml.Firebug.console##log(Js_of_ocaml.Js.string "register update notif") in
              (ignore
         @@ React.E.map
              (fun (e, msg) ->
                Js_of_ocaml.Firebug.console##log("recieved notif");
                Dom_html.document##dispatchEvent(Js.Unsafe.js_expr "my_event"))
              ~%e
            : unit)]
  in
  Lwt.return_unit

let%shared leave_game_button =
  let onclick_handler =
    [%client (fun _ ->
          Lwt.async
              (fun () ->
                 let%lwt () = Games_manager.leave_my_game () in
                 let%lwt () = Lobby_utils.send_redirect "" in
                 let base_url = "http://"^Js.to_string (Dom_html.window##.location##.host) in
                 Eliom_client.change_page_uri ~replace:true @@ base_url
              )
    )]
  in
    button ~a:[a_onclick onclick_handler] [txt "Leave game"]


let%shared reason_react_app _ () =
  let open Eliom_content.Html.D in
  (* let (_ : unit Eliom_client_value.t) =
   *   [%client Eliom_client.Page_status.ondead (fun () -> Lwt.async Notifs.unlisten)]
   * in *)
  let%lwt () = register_update_notifs () in
  (* let _ = [%client (init_client () : unit) ] in *)
  Lwt.return
    Eliom_content.Html.[
      div ~a:[a_id "client_application"] [];
      reason_react_script;
      leave_game_button;
  ]

let%shared page = reason_react_app ()

(* let%server () =
 *   (\* Eliom_registration.Html.register *\)
 * 
 *   let page_handler () () =
 *     let%lwt g = reason_react_app () () in
 *     Lwt.return @@ Os_page.content ~title:"game" g
 *   in
 *   Words_base.App.register
 *     ~service
 *     (Words_page.connected_page (fun uid_o () () -> page_handler () ()))
 *     (\* (fun () () -> Template.page "game" @@ reason_react_app () ()) *\) *)
