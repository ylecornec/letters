open%shared Shared_types
[%%shared
    open Eliom_content.Html
    open Eliom_content.Html.D
    open Js_of_ocaml
]

let%shared create_new_lobby =
  let onclick_handler =
    [%client (fun _ ->
          Lwt.async
              (fun () ->
                 let%lwt lobby_id = Lobby_utils.new_lobby_name () in
                 let base_url = "http://"^Js.to_string (Dom_html.window##.location##.host) in
                 Eliom_client.change_page_uri ~replace:true @@ base_url^"/lobby/"^lobby_id
              )
    )]
  in
    button ~a:[a_onclick onclick_handler] [txt "Create new game"]



let%shared content = [create_new_lobby]


