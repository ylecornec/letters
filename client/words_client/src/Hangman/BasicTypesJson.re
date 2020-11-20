open Js.Json;
type user_id = int64;
type product_type = Sequence | SubGame;

let decode_product_type = json => {
  switch (classify(json)) {
  | JSONArray(a) when decodeString(a[0]) == Some("Sequence") => Sequence
  | JSONArray(a) when decodeString(a[0]) == Some("SubGame") => SubGame
  | _ => failwith("expected product_type")
  }
};
let decode_user_id = json => {
  switch (classify(json)) {
  | JSONNumber(f) => Int64.of_float(f)
  | _ => failwith("expected int64 as userid")
  }
};

let decode_user_id_array = json => {
  switch (decodeArray(json)){
    | Some(a) => List.map(decode_user_id, Array.to_list(a))
    | None => failwith("user_id_array should be an array")
  }
};


type game_state('a) = Loading | Ended(user_id) | Running('a);

let decode_game_state : (_,_)=> game_state(_) = (decode_running_state, json) => {
  switch (classify(json)) {
  | JSONArray(a) when decodeString(a[0]) == Some("Loading") => Loading
  | JSONArray(a) when decodeString(a[0]) == Some("Ended") => Ended(decode_user_id(a[1]))
  | JSONArray(a) when decodeString(a[0]) == Some("Running") => Running(decode_running_state(a[1]))
  | _ => failwith("expected game state")
  }
};

type move_activity = Disabled | GmReset | Enabled | Important;

let decode_move_activity = json => {
  switch (classify(json)) {
  | JSONArray(a) when decodeString(a[0]) == Some("Disabled") => Disabled
  | JSONArray(a) when decodeString(a[0]) == Some("GmReset") => GmReset
  | JSONArray(a) when decodeString(a[0]) == Some("Enabled") => Enabled
  | JSONArray(a) when decodeString(a[0]) == Some("Important") => Important
  | _ => failwith("expected move activity")
  }
};
