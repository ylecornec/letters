(* This file was generated by Ocsigen-start.
   Feel free to use it, modify it, and redistribute it as you wish. *)

[%%server.start]

(** This module is used for i18n (internationalization). I18n allows
    to have an application in multiple languages.  The rule [make
    i18n-update] uses this module to create the i18n file for
    translations (see [Makefile.options]).  *)

val update_language : Words_i18n.t -> unit Lwt.t
(** [update_language language] updates the language (client and server
     side) for the current user with the value [language]. It also
     updates the value in the database if an user is connected.  *)
