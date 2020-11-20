(* This file was generated by Ocsigen Start.
   Feel free to use it, modify it, and redistribute it as you wish. *)

(** This module defines handlers to upload avatar, upload personal
    data, set a new password, and also main handlers (main page, about
    page, and settings page).  In addition to including all default
    handlers from OS (see {!Os_handlers}), it overrides some of them
    for the purposes of this template. *)

[%%server.start]

val upload_user_avatar_handler
  :  Os_types.User.id
  -> unit
  -> unit
     * ((float * float * float * float) option * Ocsigen_extensions.file_info)
  -> unit Lwt.t
(** Update new user avatar with cropping option. The new avatar is saved
    and the old one is removed. *)

[%%shared.start]

val set_personal_data_handler
  :  unit
  -> (string * string) * (string * string)
  -> unit Lwt.t
(** Update personal data. It uses the default OS handler
    {!Os_handlers.set_personal_data_handler} and gets the user information
    with {!Os_session.connected_fun}. *)

val forgot_password_handler : unit -> string -> unit Lwt.t
(** Reset forgotten password. It uses the default OS handler
    {!Os_handlers.forgot_password_handler} with the main service. *)

val action_link_handler
  :  Os_types.User.id option
  -> string
  -> unit
  -> Words_base.App.result Lwt.t

val set_password_handler
  :  unit
  -> string * string
  -> Eliom_service.non_ocaml Eliom_registration.redirection Lwt.t
(** Set a new password. It uses the default OS handler
    {!Os_handlers.set_password_handler} and gets the user information
    with {!Os_session.connected_fun}. *)

val preregister_handler : unit -> string -> unit Lwt.t

(** The following functions are the handlers for the three main pages.
    They are created with {!Words_container.page} which
    means that a header and a footer will be displayed in addition to
    the main content.

    For each of them, you can personalize the page for a specific user
    by sending the userid as first parameter. *)

val main_service_handler
  :  Os_types.User.id option
  -> unit
  -> unit
  -> Os_page.content Lwt.t
(** The first page of the application *)

val about_handler
  :  Os_types.User.id option
  -> unit
  -> unit
  -> Os_page.content Lwt.t
(** About page *)

val settings_handler
  :  Os_types.User.id option
  -> unit
  -> unit
  -> Os_page.content Lwt.t
(** Settings page. If the user is connected (see
    {!Words_container.get_user_data}), a settings
    container will be created. *)

val update_language_handler
  :  unit
  -> string
  -> Eliom_registration.Action.page Lwt.t