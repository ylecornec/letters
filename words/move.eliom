open Games_manager
open Api_utils
type move_request =
  {move: Game.move}

[@@deriving yojson]

let post_move_service =
  Eliom_service.create
    ~meth:
      (Eliom_service.Post
         (Eliom_parameter.unit,
          Eliom_parameter.raw_post_data))
    ~path:(Eliom_service.Path ["move"])
    ()

let () =
  let open Eliom_content.Html.D in
  let move_page_handler () (_, raw_post_data_o) =
    let%lwt str_post_data =
      match raw_post_data_o with
      | Some data -> Api_utils.read_raw_content data
      | None -> Lwt.return "<nothing>"
    in
    Api_utils.send_success ()
  in
  Eliom_registration.Any.register post_move_service move_page_handler

open Lwt

(**** Data types ****)

type coordinates = {
  latitude : float;
  longitude : float;
} [@@deriving yojson]

type location = {
  description : string option;
  coordinates : coordinates;
} [@@deriving yojson]

(* List of pairs (identifier * location) *)
type locations =
  (string * location) list
   [@@deriving yojson]


let db : location Ocsipersist.table Lwt.t =
  Ocsipersist.open_table "locations"


(**** Services ****)

let path = Eliom_service.Path ["tuto"]

let get_params =
  Eliom_parameter.(suffix (neopt (string "id")))

let update_service =
  Eliom_service.create
    ~path
    ~meth:(Eliom_service.Post (get_params,Eliom_parameter.raw_post_data))
    ()

let delete_service =
  Eliom_service.create
    ~path
    ~meth:(Eliom_service.Delete get_params) 
    ()


let edit_handler_aux id_opt (content_type, raw_content_opt) =
  let exception Invalid_Json in
  let%lwt db = db in
  if not (check_content_type ~mime_type:json_mime_type content_type) then
    send_error ~code:400 "Content-type is wrong, it must be JSON"
  else
    match id_opt, raw_content_opt with
    | None, _ ->
      send_error ~code:400 "Location identifier is missing"
    | _, None ->
      send_error ~code:400 "Body content is missing"
    | Some id, Some raw_content ->
       let%lwt move_str = read_raw_content raw_content in
       try%lwt let {move} =
            (let open Result in
            let loc_result = move_request_of_yojson (Yojson.Safe.from_string move_str) in
            (function
              Ok loc-> loc
             | Error _ -> raise Invalid_Json) loc_result)
                   in let%lwt response =
                     Games_manager.play_my_move_on_my_game move
                   in
                   match response with
                   | Not_logged_in ->
                      send_error ~code:401 "Not logged in"
                   | Games_manager_error msg ->
                      send_error ~code:400 msg
                   | Play_response response ->
                      let%lwt () = Notifs.notify "move happened" in
                      send_json ~code:200 (Yojson.Safe.to_string @@ Game.play_response_to_yojson response)
         with
        | Not_found ->
          send_error ~code:404 ("Location not found: " ^ id)
        | Invalid_Json ->
           send_error ~code:400 @@
             Format.asprintf "Provided JSON is not valid: %s. random move = %s"
               move_str
               (Yojson.Safe.to_string @@ move_request_to_yojson {move = Game.random_move} )


let update_handler id_opt content =
  edit_handler_aux id_opt content

let delete_handler id_opt _ =
  let%lwt db = db in
  match id_opt with
  | None ->
    send_error ~code:400 "An id must be provided to delete a location"
  | Some id ->
    Ocsipersist.remove db id >>= fun () ->
    send_success ()

(* Register services *)

let () =
  Eliom_registration.Any.register update_service
    (fun id_opt content -> update_handler id_opt content);
  Eliom_registration.Any.register delete_service delete_handler;
  ()
