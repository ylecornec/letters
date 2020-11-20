include Game_types
type move =
  | Solve_anagram : string -> move
  | Set_anagram : string -> move
                              [@@deriving show, yojson]

let random_move = Set_anagram "toto"

let abstract_move (m: move) : Game_types.abstract_move =
  match m with
  | Solve_anagram _ -> Internal
  | Set_anagram _ -> Init

let random_move = Solve_anagram "random_move"

type user_id = int64 [@@deriving show, yojson, json]
type play_response =
  Ok | Wrong_move of string | Winner of user_id
                [@@deriving show, yojson, json]

let abstract_play_response :
      play_response -> Game_types.abstract_play_response =
  function | Ok | Wrong_move _ -> Ok
           | Winner id -> Ended (IdMap.add id 1 IdMap.empty)

type running_game = {
    displayed_anagram: string;
    current_anagram: string;
  }
[@@deriving show,yojson, json]

type running_game_player_view = {
    displayed_anagram: string;
  }
[@@deriving show,yojson, json]


type 'a game_state =
  Loading | Ended of user_id | Running of 'a
[@@deriving show,yojson,json]

let player_view_of_game_state (s: running_game game_state) =
  match s with
    Running state -> Running {displayed_anagram = state.displayed_anagram}
  | Loading -> Loading
  | Ended id -> Ended id

type t =
  {
    game_id: int;
    game_master_id: user_id;
    players_id: user_id list;
    game_state: running_game game_state;
  }

  let players st =
    UserSet.of_list st.players_id

let has_user user_id st =
  List.mem user_id st.players_id

type move_activity = Game_types.move_activity [@@deriving show, json, yojson]

type game_master_moves =
  {set: move_activity} [@@deriving show, json, yojson]
type player_moves =
  {solve: move_activity} [@@deriving show, json, yojson]

type game_master_view =
  {
    game_id: int;
    game_master_id: user_id;
    players_id: user_id list;
    game_state: running_game game_state;
    available_moves: game_master_moves;
  } [@@deriving show, yojson, json]

type player_view = {
    game_id: int;
    game_state: running_game_player_view game_state;
    available_moves: player_moves;
  }
[@@deriving show, yojson, json]

type view = Game_master_view of game_master_view | Player_view of player_view
[@@deriving yojson, json, show]

let player_view (modifier: Game_types.view_modifier) (st: t) : player_view =
  let available_moves = match st.game_state, modifier with
    | Running _, Only_player -> {solve = Important}
    | Running _, Disabled -> {solve = Disabled}
    | Running _, Default -> {solve = Enabled}
    | _, _ -> {solve = Disabled}
  in
  {game_id = st.game_id;
   game_state = player_view_of_game_state st.game_state;
   available_moves}


let game_master_view (modifier: Game_types.view_modifier) (st: t) : game_master_view =
  let available_moves =
    match st.game_state, modifier with
    | _, Disabled -> {set = Disabled}
    | Running _, _ -> {set = GmReset}
    | _, _ -> {set = Enabled}
  in
  {
    game_id= st.game_id;
    game_master_id= st.game_master_id;
    players_id= st.players_id;
    game_state= st.game_state;
    available_moves;
  }

let view (modifier: Game_types.view_modifier) user_id (st : t) =
  if user_id = st.game_master_id then
    Game_master_view (game_master_view modifier st)
  else
    Player_view (player_view modifier st)
  
(* The global counter of unique IDs. *)
let uniq_id_counter = ref 0

(* Bakes another unique ID and returns it. *)
let get_uniq_id () =
  (* assert(!uniq_id_counter <> -1); *)
  let x = !uniq_id_counter in incr uniq_id_counter; x;;

let new_game game_master_id players_id : t =
  {
    game_id = get_uniq_id ();
    game_master_id;
    players_id;
    game_state = Loading;
  }

let allowed_move st (id: user_id) = function
  | Set_anagram _ -> id = st.game_master_id
  | Solve_anagram _ -> true

 let knuth_shuffle a =
  let n = Array.length a in
  for i = n - 1 downto 1 do
    let k = Random.int (i+1) in
    let x = a.(k) in
    a.(k) <- a.(i);
    a.(i) <- x
  done

let shuffle s =
  let n = String.length s in
  let a = Array.init n (fun i -> s.[i]) in
  let () = knuth_shuffle a in
  String.init n (fun i -> a.(i))

let play (id: user_id) (st:t) (m: move) : t * play_response =
  match m, st.game_state with
  | Set_anagram s, _ when id = st.game_master_id ->
     let displayed_anagram = shuffle s in
     let current_anagram = s in
     {st with game_state = Running {current_anagram; displayed_anagram}}
     , Ok
  | Set_anagram _, _ ->
       st
     , Wrong_move "only game master can set the anagram"

  | Solve_anagram s, Running game_state ->
     if s = game_state.current_anagram then
       {st with game_state = Ended id}
     , Winner id
     else
       st
     , Wrong_move "wrong anagram"
  | Solve_anagram _, (Ended _| Loading) ->
       st
     , Wrong_move "game ended or loading"

  let remove_user uid (st : t) =
    if uid = st.game_master_id then
      None
    else
      let players_id = List.filter (fun id -> id <> uid) st.players_id in
      match players_id with
      | [] | [_] -> None
      |  _ -> Some {st with players_id}
