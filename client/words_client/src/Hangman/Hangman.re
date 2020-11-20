open HangmanJson;

/* let base_url = "http://localhost:8080"; */
/* let game_state_url = base_url++"/current_game_json"; */

/* let play_move = (move, arg1) => { */
/*   open Js.Promise; */
/*   let payload = Js.Dict.empty(); */
/*   Js.Dict.set(payload, "move", */
/*               Js.Json.array([| */
/*                              Js.Json.string(move), */
/*                              Js.Json.string(arg1) */
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

/* let solve_hangman = str => { */
/*   open Js.Promise; */
/*   let payload = Js.Dict.empty(); */
/*   Js.Dict.set(payload, "move", */
/*               Js.Json.array([| */
/*                              Js.Json.string("Solve_hangman"), */
/*                              Js.Json.string(str) */
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

module Running = {
  [@react.component]
    let make = (~available_moves, ~encapsulate_move, ~st: running_game_player_view) => {
      let play_move = Utils.play_move(encapsulate_move);
      <div>
          {React.string("current hangman:")}
  <br/>
    {React.string(st.displayed_hangman)}
  <br/>
    {React.string("one letter:")}
  <TextInput move_activity=available_moves.try_letter callback=play_move("Try_letter")/>
    {React.string("try solve:")}
  <TextInput move_activity=available_moves.solve_hangman callback=play_move("Solve_hangman")/>
      /* <Anagram player_view=st.subgame_player_view/> */
</div>
}
};

[@react.component]
  let make = (~encapsulate_move, ~player_view:player_view) => {
    let available_moves = player_view.available_moves;
    <div className="game">
        {
      switch (player_view.game_state) {
      | Loading => {React.string("Waiting for an Hangman")}
      | Ended(user_id) => {React.string("User"++Int64.to_string(user_id)++"<todo> wins")}
      | Running(st) => <Running available_moves encapsulate_move st/>
      }
    }
  </div>;
};
