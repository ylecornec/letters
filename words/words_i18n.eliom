[%%shared type t = En|Fr]
[%%shared exception Unknown_language of string]
let%shared string_of_language = function 
| En -> "en"| Fr -> "fr"
let%shared language_of_string = function
| "en" -> En| "fr" -> Fr| s -> raise (Unknown_language s)
let%shared guess_language_of_string s = 
try language_of_string s 
with Unknown_language _ as e -> 
try language_of_string (String.sub s 0 (String.index s '-')) 
with Not_found -> 
raise e 
let%shared languages = [En;Fr]
let%shared default_language = En
let%server _language_ = Eliom_reference.Volatile.eref
~scope:Eliom_common.default_process_scope default_language
let%server get_language () = Eliom_reference.Volatile.get _language_
let%server set_language language = 
Eliom_reference.Volatile.set _language_ language

let%client _language_ = ref default_language
let%client get_language () = !_language_
let%client set_language language = _language_ := language

let%shared txt = Eliom_content.Html.F.txt
[%%shared
module Tr = struct
let welcome_text1 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Welcome to Ocsigen Start. This is a template for applications based on Ocsigen (Eliom, Js_of_ocaml, etc.)."]
| Fr -> [txt "Bienvenue dans Ocsigen Start\194\160! Ceci est un template d'application \195\169crite avec Ocsigen (Eliom, Js_of_ocaml, etc.)."]
let welcome_text2 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Use it:"]
| Fr -> [txt "Utilisez-le\194\160:"]
let welcome_text3 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "As a basis for your own applications."]
| Fr -> [txt "Comme point de d\195\169part pour vos propres applications\194\160;"]
let welcome_text4 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "To learn the most important concepts of client-server programming with Ocsigen."]
| Fr -> [txt "Pour apprendre les principaux concepts de la programmation client-serveur avec Ocsigen."]
let welcome_text5 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "This application contains:"]
| Fr -> [txt "Cette application contient\194\160:"]
let welcome_text6 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Features for user management (log-in form, user registration, activation links, password recovery, settings page, etc.)."]
| Fr -> [txt "Des fonctionnalit\195\169s de gestion des utilisateurs (connexion, cr\195\169ation d'utilisateur, liens d'activation, r\195\169cup\195\169ration de mot de passe, param\195\168tres de l'utilisateur,...)\194\160;"]
let welcome_text7 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "An extensive demo of the most important features you need to implement your own app. Read the source code to learn! And remove the demo part when you're ready to start with your own app."]
| Fr -> [txt "Une d\195\169mo des plus importantes fonctionnalit\195\169s dont vous avez besoin pour \195\169crire votre propre application. Lisez le code source pour apprendre\194\160! Ensuite enlevez la partie demo quand vous \195\170tes pr\195\170ts \195\160 commencer votre propre application\194\160;"]
let welcome_text8 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "A library with useful features (tips, notifications, etc.)."]
| Fr -> [txt "Une biblioth\195\168que avec de nombeuses fonctionnalit\195\169s utiles (tips, notifications, etc.)\194\160;"]
let welcome_text9 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "All the features you need to create a multilingual app."]
| Fr -> [txt "Tous les outils pour cr\195\169er une application multilingue\194\160;"]
let welcome_text10 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "A basic responsive CSS."]
| Fr -> [txt "Une feuille de style \"responsive\" basique."]
let welcome_text11 ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "This application is multi-platform: it can run as a client-server Web application (with server-side generated pages) and as a mobile app (with client-side generated pages) for Android, iOS or Windows. Have a look at the README file to learn how to generate the mobile apps, which you will be able to upload on Google Play or Apple App Store. "]
| Fr -> [txt "Cette application est multi-plateforme\194\160: elle peut tourner comme application Web client-serveur (avec des pages g\195\169n\195\169r\195\169es c\195\180t\195\169 serveur) ou bien comme application mobile pour iOS, Android ou Windows (avec des pages g\195\169n\195\169r\195\169es c\195\180t\195\169 client). Regardez le fichier README pour apprendre comment g\195\169n\195\169rer les applications mobiles que vous pourrez envoyer sur Google Play ou Apple App Store."]
let about_handler_template ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "This template provides a skeleton for an Ocsigen application."]
| Fr -> [txt "Ce template fournit une base pour une application Ocsigen."]
let about_handler_license ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Feel free to modify the generated code and use it or redistribute it in any way you want."]
| Fr -> [txt "Vous \195\170tes libres de modifier le code g\195\169n\195\169r\195\169 et de l'utiliser ou le redistribuer comme vous le souhaitez."]
let footer_generated ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "This application has been generated using the"]
| Fr -> [txt "Cette application a \195\169t\195\169 g\195\169n\195\169r\195\169e en utilisant le template d'"]
let footer_eliom_distillery ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "template for Eliom-distillery and uses the"]
| Fr -> [txt "avec Eliom-distillery et utilise les technologies"]
let footer_technology ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt " technology."]
| Fr -> [txt "."]
let home ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "H" else "h")];[txt "ome"]]
| Fr -> List.flatten [[txt (if capitalize then "H" else "h")];[txt "ome"]]
let about ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "A" else "a")];[txt "bout"]]
| Fr -> List.flatten [[txt (if capitalize then "\195\128" else "\195\160")];[txt " propos"]]
let demo ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Demo"]
| Fr -> [txt "D\195\169mo"]
let password ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "P" else "p")];[txt "assword"]]
| Fr -> List.flatten [[txt (if capitalize then "M" else "m")];[txt "ot de passe"]]
let retype_password ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "retype your password"]
| Fr -> [txt "retapez votre mot de passe"]
let your_email ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "Y" else "y")];[txt "our email"]]
| Fr -> List.flatten [[txt (if capitalize then "V" else "v")];[txt "otre e-mail"]]
let your_password ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "Y" else "y")];[txt "our password"]]
| Fr -> List.flatten [[txt (if capitalize then "V" else "v")];[txt "otre mot de passe"]]
let keep_logged_in ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "keep me logged in"]
| Fr -> [txt "rester connect\195\169"]
let sign_in ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "S" else "s")];[txt "ign in"]]
| Fr -> List.flatten [[txt (if capitalize then "S" else "s")];[txt "e connecter"]]
let forgot_your_password_q ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "F" else "f")];[txt "orgot your password?"]]
| Fr -> List.flatten [[txt (if capitalize then "M" else "m")];[txt "ot de passe oubli\195\169\194\160?"]]
let sign_up ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "S" else "s")];[txt "ign up"]]
| Fr -> List.flatten [[txt (if capitalize then "S" else "s")];[txt "'enregistrer"]]
let logout ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "L" else "l")];[txt "ogout"]]
| Fr -> List.flatten [[txt (if capitalize then "S" else "s")];[txt "e d\195\169connecter"]]
let set_as_main_email ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "S" else "s")];[txt "et as main email"]]
| Fr -> List.flatten [[txt (if capitalize then "D" else "d")];[txt "\195\169finir comme e-mail principal"]]
let validated ?(lang = get_language ()) () ?(capitalize=false) ?(f=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "V" else "v")];[txt "alidated"]]
| Fr -> List.flatten [[txt (if capitalize then "V" else "v")];[txt "alid\195\169"];[txt (if f then "e" else "")]]
let waiting_confirmation ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "W" else "w")];[txt "aiting for confirmation"]]
| Fr -> List.flatten [[txt (if capitalize then "E" else "e")];[txt "n attente de confirmation"]]
let main_email ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "M" else "m")];[txt "ain email"]]
| Fr -> List.flatten [[txt (if capitalize then "E" else "e")];[txt "-mail principal"]]
let change_password ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "C" else "c")];[txt "hange your password:"]]
| Fr -> List.flatten [[txt (if capitalize then "C" else "c")];[txt "hanger votre mot de passe\194\160:"]]
let link_new_email ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Link a new email to your account:"]
| Fr -> [txt "Ajouter une adresse e-mail \195\160 votre compte\194\160:"]
let currently_registered_emails ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Currently registered emails:"]
| Fr -> [txt "E-mails actuellement enregistr\195\169s\194\160:"]
let settings ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "S" else "s")];[txt "ettings"]]
| Fr -> List.flatten [[txt (if capitalize then "P" else "p")];[txt "aram\195\168tres"]]
let error ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "E" else "e")];[txt "rror"]]
| Fr -> List.flatten [[txt (if capitalize then "E" else "e")];[txt "rreur"]]
let passwords_do_not_match ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Passwords do not match"]
| Fr -> [txt "Les mots de passe ne correspondent pas"]
let generate_action_link_key_subject_email ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "creation"]
| Fr -> [txt "cr\195\169ation"]
let sign_up_email_msg ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Welcome!\\r\\nTo confirm your email address, please click on this link:"]
| Fr -> [txt "Bienvenue\194\160!\\r\\nPour confirmer votre adresse e-mail, cliquer sur ce lien\194\160:"]
let email_already_exists ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Email already exists"]
| Fr -> [txt "Cet e-mail existe d\195\169j\195\160"]
let user_does_not_exist ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "user does not exist"]
| Fr -> [txt "Cet utilisateur n'existe pas"]
let account_not_activated ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Account not activated"]
| Fr -> [txt "Ce compte n'est pas activ\195\169"]
let wrong_password ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Wrong password"]
| Fr -> [txt "Mauvais mot de passe"]
let no_such_user ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "No such user"]
| Fr -> [txt "Cet utilisateur n'existe pas"]
let add_email_msg ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Welcome!\\r\\nTo confirm your email address, please click on this link:"]
| Fr -> [txt "Bienvenue\194\160!\\r\\nPour confirmer votre adresse e-mail, cliquez sur ce lien\194\160:"]
let invalid_action_key ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Invalid action key, please ask for a new one."]
| Fr -> [txt "Clef d'action invalide. Demandez en une nouvelle svp."]
let forgot_pwd_email_msg ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Hi,\\r\\nTo set a new password, please click on this link:"]
| Fr -> [txt "Bonjour,\\r\\nPour mettre \195\160 jour votre mot de passe, cliquez sur ce lien\194\160:"]
let must_be_connected_to_see_page ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "You must be connected to see this page."]
| Fr -> [txt "Vous devez \195\170tre connect\195\169 pour voir cette page."]
let email_address ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Email address"]
| Fr -> [txt "Adresse e-mail"]
let your_first_name ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Your first name"]
| Fr -> [txt "Votre pr\195\169nom"]
let your_last_name ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Your last name"]
| Fr -> [txt "Votre nom"]
let submit ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "S" else "s")];[txt "ubmit"]]
| Fr -> List.flatten [[txt (if capitalize then "E" else "e")];[txt "nvoyer"]]
let see_help_again_from_beginning ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "See help again from beginning"]
| Fr -> [txt "Revoir l'aide depuis le d\195\169but"]
let personal_information_not_set ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Your personal information has not been set yet."]
| Fr -> [txt "Vous n'avez pas encore entr\195\169 vos donn\195\169es personnelles."]
let take_time_enter_name_password ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Please take time to enter your name and to set a password."]
| Fr -> [txt "Veuillez entrer votre nom et choisir un mot de passe svp."]
let wrong_data_fix ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Wrong data. Please fix."]
| Fr -> [txt "Donn\195\169es incorrectes. Veuillez corriger."]
let send ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "S" else "s")];[txt "end"]]
| Fr -> List.flatten [[txt (if capitalize then "E" else "e")];[txt "nvoyer"]]
let recover_password ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "R" else "r")];[txt "ecover password"]]
| Fr -> List.flatten [[txt (if capitalize then "R" else "r")];[txt "\195\169cup\195\169rer le mot de passe."]]
let welcome ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "W" else "w")];[txt "elcome!"]]
| Fr -> List.flatten [[txt (if capitalize then "B" else "b")];[txt "ienvenue\194\160!"]]
let log_in_to_see_page ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> List.flatten [[txt (if capitalize then "L" else "l")];[txt "og in to see this page."]]
| Fr -> List.flatten [[txt (if capitalize then "C" else "c")];[txt "onnectez-vous pour voir cette page."]]
let change_profile_picture ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Change profile picture"]
| Fr -> [txt "Changer votre photo de profil."]
let change_language ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Change language"]
| Fr -> [txt "Changer la langue"]
let disconnect_all ?(lang = get_language ()) ()  () =
match lang with
| En -> [txt "Logout on all my devices"]
| Fr -> [txt "Me d\195\169connecter sur tous mes appareils"]
module S = struct
let welcome_text1 ?(lang = get_language ()) ()  () =
match lang with
| En -> "Welcome to Ocsigen Start. This is a template for applications based on Ocsigen (Eliom, Js_of_ocaml, etc.)."
| Fr -> "Bienvenue dans Ocsigen Start\194\160! Ceci est un template d'application \195\169crite avec Ocsigen (Eliom, Js_of_ocaml, etc.)."
let welcome_text2 ?(lang = get_language ()) ()  () =
match lang with
| En -> "Use it:"
| Fr -> "Utilisez-le\194\160:"
let welcome_text3 ?(lang = get_language ()) ()  () =
match lang with
| En -> "As a basis for your own applications."
| Fr -> "Comme point de d\195\169part pour vos propres applications\194\160;"
let welcome_text4 ?(lang = get_language ()) ()  () =
match lang with
| En -> "To learn the most important concepts of client-server programming with Ocsigen."
| Fr -> "Pour apprendre les principaux concepts de la programmation client-serveur avec Ocsigen."
let welcome_text5 ?(lang = get_language ()) ()  () =
match lang with
| En -> "This application contains:"
| Fr -> "Cette application contient\194\160:"
let welcome_text6 ?(lang = get_language ()) ()  () =
match lang with
| En -> "Features for user management (log-in form, user registration, activation links, password recovery, settings page, etc.)."
| Fr -> "Des fonctionnalit\195\169s de gestion des utilisateurs (connexion, cr\195\169ation d'utilisateur, liens d'activation, r\195\169cup\195\169ration de mot de passe, param\195\168tres de l'utilisateur,...)\194\160;"
let welcome_text7 ?(lang = get_language ()) ()  () =
match lang with
| En -> "An extensive demo of the most important features you need to implement your own app. Read the source code to learn! And remove the demo part when you're ready to start with your own app."
| Fr -> "Une d\195\169mo des plus importantes fonctionnalit\195\169s dont vous avez besoin pour \195\169crire votre propre application. Lisez le code source pour apprendre\194\160! Ensuite enlevez la partie demo quand vous \195\170tes pr\195\170ts \195\160 commencer votre propre application\194\160;"
let welcome_text8 ?(lang = get_language ()) ()  () =
match lang with
| En -> "A library with useful features (tips, notifications, etc.)."
| Fr -> "Une biblioth\195\168que avec de nombeuses fonctionnalit\195\169s utiles (tips, notifications, etc.)\194\160;"
let welcome_text9 ?(lang = get_language ()) ()  () =
match lang with
| En -> "All the features you need to create a multilingual app."
| Fr -> "Tous les outils pour cr\195\169er une application multilingue\194\160;"
let welcome_text10 ?(lang = get_language ()) ()  () =
match lang with
| En -> "A basic responsive CSS."
| Fr -> "Une feuille de style \"responsive\" basique."
let welcome_text11 ?(lang = get_language ()) ()  () =
match lang with
| En -> "This application is multi-platform: it can run as a client-server Web application (with server-side generated pages) and as a mobile app (with client-side generated pages) for Android, iOS or Windows. Have a look at the README file to learn how to generate the mobile apps, which you will be able to upload on Google Play or Apple App Store. "
| Fr -> "Cette application est multi-plateforme\194\160: elle peut tourner comme application Web client-serveur (avec des pages g\195\169n\195\169r\195\169es c\195\180t\195\169 serveur) ou bien comme application mobile pour iOS, Android ou Windows (avec des pages g\195\169n\195\169r\195\169es c\195\180t\195\169 client). Regardez le fichier README pour apprendre comment g\195\169n\195\169rer les applications mobiles que vous pourrez envoyer sur Google Play ou Apple App Store."
let about_handler_template ?(lang = get_language ()) ()  () =
match lang with
| En -> "This template provides a skeleton for an Ocsigen application."
| Fr -> "Ce template fournit une base pour une application Ocsigen."
let about_handler_license ?(lang = get_language ()) ()  () =
match lang with
| En -> "Feel free to modify the generated code and use it or redistribute it in any way you want."
| Fr -> "Vous \195\170tes libres de modifier le code g\195\169n\195\169r\195\169 et de l'utiliser ou le redistribuer comme vous le souhaitez."
let footer_generated ?(lang = get_language ()) ()  () =
match lang with
| En -> "This application has been generated using the"
| Fr -> "Cette application a \195\169t\195\169 g\195\169n\195\169r\195\169e en utilisant le template d'"
let footer_eliom_distillery ?(lang = get_language ()) ()  () =
match lang with
| En -> "template for Eliom-distillery and uses the"
| Fr -> "avec Eliom-distillery et utilise les technologies"
let footer_technology ?(lang = get_language ()) ()  () =
match lang with
| En -> " technology."
| Fr -> "."
let home ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "H" else "h");"ome"]
| Fr -> String.concat "" [(if capitalize then "H" else "h");"ome"]
let about ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "A" else "a");"bout"]
| Fr -> String.concat "" [(if capitalize then "\195\128" else "\195\160");" propos"]
let demo ?(lang = get_language ()) ()  () =
match lang with
| En -> "Demo"
| Fr -> "D\195\169mo"
let password ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "P" else "p");"assword"]
| Fr -> String.concat "" [(if capitalize then "M" else "m");"ot de passe"]
let retype_password ?(lang = get_language ()) ()  () =
match lang with
| En -> "retype your password"
| Fr -> "retapez votre mot de passe"
let your_email ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "Y" else "y");"our email"]
| Fr -> String.concat "" [(if capitalize then "V" else "v");"otre e-mail"]
let your_password ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "Y" else "y");"our password"]
| Fr -> String.concat "" [(if capitalize then "V" else "v");"otre mot de passe"]
let keep_logged_in ?(lang = get_language ()) ()  () =
match lang with
| En -> "keep me logged in"
| Fr -> "rester connect\195\169"
let sign_in ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "S" else "s");"ign in"]
| Fr -> String.concat "" [(if capitalize then "S" else "s");"e connecter"]
let forgot_your_password_q ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "F" else "f");"orgot your password?"]
| Fr -> String.concat "" [(if capitalize then "M" else "m");"ot de passe oubli\195\169\194\160?"]
let sign_up ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "S" else "s");"ign up"]
| Fr -> String.concat "" [(if capitalize then "S" else "s");"'enregistrer"]
let logout ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "L" else "l");"ogout"]
| Fr -> String.concat "" [(if capitalize then "S" else "s");"e d\195\169connecter"]
let set_as_main_email ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "S" else "s");"et as main email"]
| Fr -> String.concat "" [(if capitalize then "D" else "d");"\195\169finir comme e-mail principal"]
let validated ?(lang = get_language ()) () ?(capitalize=false) ?(f=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "V" else "v");"alidated"]
| Fr -> String.concat "" [(if capitalize then "V" else "v");"alid\195\169";(if f then "e" else "")]
let waiting_confirmation ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "W" else "w");"aiting for confirmation"]
| Fr -> String.concat "" [(if capitalize then "E" else "e");"n attente de confirmation"]
let main_email ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "M" else "m");"ain email"]
| Fr -> String.concat "" [(if capitalize then "E" else "e");"-mail principal"]
let change_password ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "C" else "c");"hange your password:"]
| Fr -> String.concat "" [(if capitalize then "C" else "c");"hanger votre mot de passe\194\160:"]
let link_new_email ?(lang = get_language ()) ()  () =
match lang with
| En -> "Link a new email to your account:"
| Fr -> "Ajouter une adresse e-mail \195\160 votre compte\194\160:"
let currently_registered_emails ?(lang = get_language ()) ()  () =
match lang with
| En -> "Currently registered emails:"
| Fr -> "E-mails actuellement enregistr\195\169s\194\160:"
let settings ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "S" else "s");"ettings"]
| Fr -> String.concat "" [(if capitalize then "P" else "p");"aram\195\168tres"]
let error ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "E" else "e");"rror"]
| Fr -> String.concat "" [(if capitalize then "E" else "e");"rreur"]
let passwords_do_not_match ?(lang = get_language ()) ()  () =
match lang with
| En -> "Passwords do not match"
| Fr -> "Les mots de passe ne correspondent pas"
let generate_action_link_key_subject_email ?(lang = get_language ()) ()  () =
match lang with
| En -> "creation"
| Fr -> "cr\195\169ation"
let sign_up_email_msg ?(lang = get_language ()) ()  () =
match lang with
| En -> "Welcome!\\r\\nTo confirm your email address, please click on this link:"
| Fr -> "Bienvenue\194\160!\\r\\nPour confirmer votre adresse e-mail, cliquer sur ce lien\194\160:"
let email_already_exists ?(lang = get_language ()) ()  () =
match lang with
| En -> "Email already exists"
| Fr -> "Cet e-mail existe d\195\169j\195\160"
let user_does_not_exist ?(lang = get_language ()) ()  () =
match lang with
| En -> "user does not exist"
| Fr -> "Cet utilisateur n'existe pas"
let account_not_activated ?(lang = get_language ()) ()  () =
match lang with
| En -> "Account not activated"
| Fr -> "Ce compte n'est pas activ\195\169"
let wrong_password ?(lang = get_language ()) ()  () =
match lang with
| En -> "Wrong password"
| Fr -> "Mauvais mot de passe"
let no_such_user ?(lang = get_language ()) ()  () =
match lang with
| En -> "No such user"
| Fr -> "Cet utilisateur n'existe pas"
let add_email_msg ?(lang = get_language ()) ()  () =
match lang with
| En -> "Welcome!\\r\\nTo confirm your email address, please click on this link:"
| Fr -> "Bienvenue\194\160!\\r\\nPour confirmer votre adresse e-mail, cliquez sur ce lien\194\160:"
let invalid_action_key ?(lang = get_language ()) ()  () =
match lang with
| En -> "Invalid action key, please ask for a new one."
| Fr -> "Clef d'action invalide. Demandez en une nouvelle svp."
let forgot_pwd_email_msg ?(lang = get_language ()) ()  () =
match lang with
| En -> "Hi,\\r\\nTo set a new password, please click on this link:"
| Fr -> "Bonjour,\\r\\nPour mettre \195\160 jour votre mot de passe, cliquez sur ce lien\194\160:"
let must_be_connected_to_see_page ?(lang = get_language ()) ()  () =
match lang with
| En -> "You must be connected to see this page."
| Fr -> "Vous devez \195\170tre connect\195\169 pour voir cette page."
let email_address ?(lang = get_language ()) ()  () =
match lang with
| En -> "Email address"
| Fr -> "Adresse e-mail"
let your_first_name ?(lang = get_language ()) ()  () =
match lang with
| En -> "Your first name"
| Fr -> "Votre pr\195\169nom"
let your_last_name ?(lang = get_language ()) ()  () =
match lang with
| En -> "Your last name"
| Fr -> "Votre nom"
let submit ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "S" else "s");"ubmit"]
| Fr -> String.concat "" [(if capitalize then "E" else "e");"nvoyer"]
let see_help_again_from_beginning ?(lang = get_language ()) ()  () =
match lang with
| En -> "See help again from beginning"
| Fr -> "Revoir l'aide depuis le d\195\169but"
let personal_information_not_set ?(lang = get_language ()) ()  () =
match lang with
| En -> "Your personal information has not been set yet."
| Fr -> "Vous n'avez pas encore entr\195\169 vos donn\195\169es personnelles."
let take_time_enter_name_password ?(lang = get_language ()) ()  () =
match lang with
| En -> "Please take time to enter your name and to set a password."
| Fr -> "Veuillez entrer votre nom et choisir un mot de passe svp."
let wrong_data_fix ?(lang = get_language ()) ()  () =
match lang with
| En -> "Wrong data. Please fix."
| Fr -> "Donn\195\169es incorrectes. Veuillez corriger."
let send ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "S" else "s");"end"]
| Fr -> String.concat "" [(if capitalize then "E" else "e");"nvoyer"]
let recover_password ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "R" else "r");"ecover password"]
| Fr -> String.concat "" [(if capitalize then "R" else "r");"\195\169cup\195\169rer le mot de passe."]
let welcome ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "W" else "w");"elcome!"]
| Fr -> String.concat "" [(if capitalize then "B" else "b");"ienvenue\194\160!"]
let log_in_to_see_page ?(lang = get_language ()) () ?(capitalize=false) () =
match lang with
| En -> String.concat "" [(if capitalize then "L" else "l");"og in to see this page."]
| Fr -> String.concat "" [(if capitalize then "C" else "c");"onnectez-vous pour voir cette page."]
let change_profile_picture ?(lang = get_language ()) ()  () =
match lang with
| En -> "Change profile picture"
| Fr -> "Changer votre photo de profil."
let change_language ?(lang = get_language ()) ()  () =
match lang with
| En -> "Change language"
| Fr -> "Changer la langue"
let disconnect_all ?(lang = get_language ()) ()  () =
match lang with
| En -> "Logout on all my devices"
| Fr -> "Me d\195\169connecter sur tous mes appareils"
end
end
]
