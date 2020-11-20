open Types_of_games
[@bs.val] external document: Js.t({..}) = "document";
[@bs.val] external is_game_master_state: Js.t({..}) = "is_game_master_state";
[@bs.val] external window: Js.t({..}) = "window";

let main_game_component = components_of_game_shape(x=>x, Product(Hangman,Product(Anagram, Product (Anagram, Product(Anagram, Anagram)))));

type state =
  | LoadingGame
  | ErrorFetchingGame
  | LoadedGame(string);

/* let base_url = "http://localhost:8080"; */
let base_url = "http://"++window##location##host;
let play_move_url = base_url++"/tuto/hello";
let game_state_url = base_url++"/current_game_json";

let get_game_state = () => {
  Fetch.fetch(game_state_url)
};

[@react.component]
  let make = () => {
    open Js.Promise;
    let (state, setState) = React.useState(() => LoadingGame);
    // Notice that instead of `useEffect`, we have `useEffect0`. See
    // reasonml.github.io/reason-react/docs/en/components#hooks for more info
    let update_state = () => {
      /* play_inc() */
      get_game_state()
      |> then_(Fetch.Response.json)
      |> then_(jsonResponse => {
          setState(_previousState => LoadedGame(Js.Json.stringify(jsonResponse)));
          resolve();
        })
      |> catch(_err => {
          setState(_previousState => ErrorFetchingGame);
          resolve();
        })
      |> ignore
    };

    React.useEffect0(() => {update_state(); None;});

    let state_change_handler = _ => {update_state ()};

    React.useEffect1(
      () => {
      ignore(document##addEventListener("state_change", state_change_handler));
      Js.log("added_new_event_listener");
      /* Some(() => document##addEventListener("keypress", logEvent)); */
      Some(() => document##removeEventListener("state_change", state_change_handler));
    },
      [||],
    );

      <div
          style={ReactDOMRe.Style.make(
      ~display="flex",
      ~alignItems="center",
      ~justifyContent="center",
      (),
    )}>
      {switch (state) {
      | ErrorFetchingGame => React.string("An error occurred!")
      | LoadingGame => React.string("Loading...")
      | LoadedGame(view) =>
      Js.log(view);
      let json_of_view = Js.Json.parseExn(view);
      {main_game_component(None, json_of_view)}
      /* let view = HangmanJson.decode_view(json_of_view); */
      /* {React.string("TODO")} */
    }}
</div>;
};
