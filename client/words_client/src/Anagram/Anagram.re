open AnagramJson;


[@bs.val] external window: Js.t({..}) = "window";
/* let base_url = "http://localhost:8080"; */
let base_url = "http://"++window##location##host;
let play_move_url = base_url++"/tuto/hello";
let game_state_url = base_url++"/current_game_json";

/* let solve_anagram = anagram => { */
/*   open Js.Promise; */
/*   let payload = Js.Dict.empty(); */
/*   Js.Dict.set(payload, "move", */
/*               Js.Json.array([| */
/*                              Js.Json.string("Solve_anagram"), */
/*                              Js.Json.string(anagram) */
/*                              |])); */
/*     Fetch.fetchWithInit( */
/*     play_move_url, */
/*     Fetch.RequestInit.make( */
/*       ~method_=Post, */
/*       ~body=Fetch.BodyInit.make(Js.Json.stringify(Js.Json.object_(payload))), */
/*       ~headers=Fetch.HeadersInit.make({"Content-Type": "application/json"}), */
/*       () */
/*     ) */
/*   ) */
/*       |> then_(_jsonResponse =>{ */
/*           Js.log("click played") */
/*           resolve(); */
/*         }) */
/*       |> ignore */
/* }; */

module InputAnagram = {
  [@react.component]
    let make = (~encapsulate_move) => {
      let (value, onChange) = React.useState(() => "");
      let solve_anagram = Utils.play_move(encapsulate_move, "Solve_anagram");
      <form
          onSubmit= {e => {
        ReactEvent.Form.preventDefault(e);
        solve_anagram(value);
      }} >
      <input
          onChange={
        event => {
            let value = ReactEvent.Form.target(event)##value;
            onChange(_ => value)
          }
      }
        />
    </form>;
  }
}
  module Running = {
    [@react.component]
      let make = (~encapsulate_move, ~st: running_game_player_view, ~available_moves) => {
        let solve_anagram = Utils.play_move(encapsulate_move, "Solve_anagram");
        <div>
            {React.string("current anagram:")}
        <br/>
          {React.string(st.displayed_anagram)}
        <br/>
        <TextInput callback=solve_anagram move_activity=available_moves.solve/>
          /* <InputAnagram encapsulate_move /> */
        </div>
      }
  };
[@react.component]
  let make = (~encapsulate_move, ~player_view:player_view) => {
    <div className="game">
        {
      switch (player_view.game_state) {
      | Loading => {React.string("Waiting for an anagram")}
      | Ended(_user_id) => {React.string("User <todo> wins")}
      | Running(st) => <Running encapsulate_move st available_moves=player_view.available_moves/>
      }
    }
  </div>;
};
