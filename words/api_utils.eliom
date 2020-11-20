let json_mime_type = "application/json"

let send_json ~code json =
  Eliom_registration.String.send ~code (json, json_mime_type)

let check_content_type ~mime_type content_type =
  match content_type with
  | Some ((type_, subtype), _)
      when (type_ ^ "/" ^ subtype) = mime_type -> true
  | _ -> false

let read_raw_content ?(length = 4096) raw_content =
  let content_stream = Ocsigen_stream.get raw_content in
  Ocsigen_stream.string_of_stream length content_stream

let send_success () =
  Eliom_registration.String.send ~code:200 ("", "")

type error = {
  error_message : string;
} [@@deriving yojson]

let send_error ~code error_message =
  let json = Yojson.Safe.to_string (error_to_yojson {error_message}) in
  send_json ~code json

