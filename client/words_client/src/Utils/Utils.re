[@bs.val] external window: Js.t({..}) = "window";
let base_url = "http://"++window##location##host;
let play_move_url = base_url++"/tuto/hello";
let play_move = (encapsulate, move, arg1) => {
  open Js.Promise;
  let payload = Js.Dict.empty();
  Js.Dict.set(payload, "move",
              encapsulate(
              Js.Json.array([|
                             Js.Json.string(move),
                             Js.Json.string(arg1)
                             |])));
    Fetch.fetchWithInit(
    play_move_url,
    Fetch.RequestInit.make(
      ~method_=Post,
      ~body=Fetch.BodyInit.make(Js.Json.stringify(Js.Json.object_(payload))),
      ~headers=Fetch.HeadersInit.make({"Content-Type": "application/json"}),
      ()
    )
  )
      |> then_(_jsonResponse =>{
          Js.log("click played")
          resolve();
        })
      |> ignore
};
