open BasicTypesJson;
type running_game_player_view = {displayed_anagram: string};

let decode_running_game_player_view = json => {
  open Js.Json;
  open Js.Dict;
  switch (classify(json)) {
  | JSONObject(value) =>
    switch (Belt.Option.flatMap(get(value, "displayed_anagram"), decodeString)){
    | Some(displayed_anagram) => {displayed_anagram: displayed_anagram}
    | _ => failwith("wrong fields in running game player view")
    }
  | _ => failwith("expected running game player view")
  }
};

type running_game = {
    displayed_anagram: string,
    current_anagram: string,
  };

let decode_running_game = json => {
  open Js.Json;
  open Js.Dict;
  switch (classify(json)) {
  | JSONObject(value) =>
    switch (Belt.Option.flatMap(get(value, "displayed_anagram"),decodeString)
            ,Belt.Option.flatMap(get(value, "current_anagram"), decodeString)){
    | (Some(displayed_anagram), Some(current_anagram)) =>
      {displayed_anagram: displayed_anagram, current_anagram: current_anagram}
    | _ => failwith("wrong fields in running game")
    }
  | _ => failwith("expected running game")
  }
};

type player_moves = {solve: move_activity};
type player_view = {
  game_id: int,
  game_state: game_state(running_game_player_view),
  available_moves: player_moves,
};

let decode_player_moves : Js.Json.t => player_moves = json => {
  open Js.Json;
  open Js.Dict;
  switch (classify(json)) {
  | JSONObject(value) =>
    switch (Belt.Option.map(get(value, "solve"), decode_move_activity)
    ){
    | Some(activity) => {solve: activity}
    | _ => failwith("wrong fields in player_moves")
    }
  | _ => failwith("expected player_moves")
  }
};
let decode_player_view: Js.Json.t => player_view = json => {
  open Js.Json;
  open Js.Dict;
  switch (classify(json)) {
  | JSONObject(value) =>
    switch (  Belt.Option.flatMap(get(value, "game_id"), decodeNumber)
            , Belt.Option.map(get(value, "game_state"), decode_game_state(decode_running_game_player_view))
            , Belt.Option.map(get(value, "available_moves"), decode_player_moves)
    ){
    | (Some(f), Some(game_state), Some(available_moves)) =>
      {game_id : int_of_float(f), game_state, available_moves}
    | _ => failwith("wrong fields in player view")
    }
  | _ => failwith("expected player view")
  }
};

type game_master_moves = {set: move_activity};


let decode_game_master_moves : Js.Json.t => game_master_moves = json => {
  open Js.Json;
  open Js.Dict;
  switch (classify(json)) {
  | JSONObject(value) =>
    switch (Belt.Option.map(get(value, "set"), decode_move_activity)
    ){
    | Some(activity) => {set: activity}
    | _ => failwith("wrong fields in game master view")
    }
  | _ => failwith("expected game master moves")
  }
};

type game_master_view =
  {
    game_id: int,
    game_master_id: user_id,
    players_id:list(user_id),
    game_state: game_state(running_game),
    available_moves: game_master_moves,
  } ;


let decode_game_master_view : Js.Json.t => game_master_view = json => {
  open Js.Json;
  open Js.Dict;
  switch (classify(json)) {
  | JSONObject(value) =>
    switch (  Belt.Option.flatMap(get(value, "game_id"), decodeNumber)
            , Belt.Option.map(get(value, "game_master_id"), decode_user_id)
            , Belt.Option.map(get(value, "players_id"), decode_user_id_array)
            , Belt.Option.map(get(value, "game_state"), decode_game_state(decode_running_game))
            , Belt.Option.map(get(value, "available_moves"), decode_game_master_moves)
    ){
    | (Some(f),
       Some(game_master_id),
       Some(players_id),
       Some(game_state),
       Some(available_moves)) =>
      {game_id : int_of_float(f), game_master_id, players_id, game_state, available_moves}
    | _ => failwith("wrong fields in game master view")
    }
  | _ => failwith("expected game master view")
  }
};

let denone = opt => {
  switch (opt) {
    | Some(x) => x
    | None => failwith("cannot denone None")
  }
};

type view = Game_master_view(game_master_view) | Player_view(player_view);
let decode_view = json => {
  open Js.Json;
  switch (classify(json)) {
  | JSONArray(a) when decodeString(a[0]) == Some("Game_master_view") =>
    Game_master_view(decode_game_master_view(a[1]))
  | JSONArray(a) when decodeString(a[0]) == Some("Player_view") =>
   Player_view(decode_player_view(a[1]))
  | JSONArray(a) when decodeString(a[0]) == Some("Error") =>
    failwith(denone(Js.Json.decodeString(a[1])))
  | _ => failwith("expected view")
  }
};

[@bs.scope("JSON")] [@bs.val] external string_of_gm_view: game_master_view => string = "stringify";
[@bs.scope("JSON")] [@bs.val] external string_of_player_view: player_view => string = "stringify";
[@bs.val] external parseIntoView: string => view = "parse_view";
