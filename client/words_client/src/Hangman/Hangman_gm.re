open HangmanJson;

[@bs.val] external document: Js.t({..}) = "document";
[@bs.val] external window: Js.t({..}) = "window";
/* let base_url = "http://localhost:8080"; */
let base_url = "http://"++window##location##host;
let play_move_url = base_url++"/tuto/hello";
let game_state_url = base_url++"/current_game_json";

module Loading = {
  [@react.component]
    let make = (~available_moves, ~encapsulate_move) => {
    let set_hangman = Utils.play_move(encapsulate_move, "Set_hangman");
    <div>
        {React.string("Please set hangman")}
        <TextInput move_activity=available_moves.set_hangman callback=set_hangman/>
    </div>
    }
};

module Running = {
  [@react.component]
    let make = (~st: running_game) => {
      /* let play_move = Utils.play_move(encapsulate_move); */
      <div>
          {React.string("current hangman: ")}
    {React.string(st.displayed_hangman)}
  <br/>
          {React.string("Solution: ")}
    {React.string(st.current_hangman)}
  <br/>
  /*   {React.string("try solve:")} */
  /* <TextInput move_activity=available_moves.solve_hangman callback=play_move("Solve_hangman")/> */
</div>
}
};

[@react.component]
  let make = (~encapsulate_move, ~game_master_view) => {
    let available_moves = game_master_view.available_moves;
    <div className="game">
        {React.array(
    [|
     switch (game_master_view.game_state){
     | Loading => <Loading available_moves encapsulate_move/>
     | Ended(_user_id) => <div>{React.string("User <todo> wins")} <Loading available_moves encapsulate_move/></div>
      | Running(st) => <Running st/>
     }
     |])}
  </div>;
};
