[%%shared
    open Eliom_content.Html
    open Eliom_content.Html.F
]
let%shared page my_title content =
  let%lwt c = content in
  Lwt.return @@
    html
      (head (title (txt my_title)) [])
      (body c)
