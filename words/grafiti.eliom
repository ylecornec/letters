open%shared Js_of_ocaml
open Eliom_content.Html.D (* provides functions to create HTML nodes *)
module Graffiti_app =
  Eliom_registration.App (struct
      let application_name = "graffiti"
      let global_data_path = None
    end)

let%server count = ref 0

let%server main_service =
  Graffiti_app.create
    ~path:(Eliom_service.Path ["grafiti"])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    (fun () () ->
       let c = incr count; !count in
       let text = Printf.sprintf "You came %i times to this page" in
       ignore [%client
         (Dom_html.window##alert
            (Js.string @@ Printf.sprintf "You came %i times to this page" ~%c)
          : unit)
       ];
       Lwt.return
         (html
            (head (title (txt "Graffiti")) [])
            (body [h1 [txt @@ text c]])))
