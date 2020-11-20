open HangmanJson;

[@react.component]
let make (~encapsulate_move, ~view){
  switch(view){
  |  Game_master_view(game_master_view) =>
    Js.log("view=game_master");
  <Hangman_gm encapsulate_move game_master_view/>
  | Player_view(player_view) =>
    Js.log("view=player");
  <Hangman encapsulate_move player_view/>
  }
}
