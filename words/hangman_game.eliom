open Game_types
module Hangman : GAME = struct

  type move =
    | Solve_hangman : string -> move
    | Set_hangman : string -> move
    | Try_letter : char -> move
                             [@@deriving show, yojson]

  let random_move = Set_hangman "hello"
  let abstract_move (m: move) : abstract_move =
    match m with
    | Solve_hangman _ | Try_letter _ -> Internal
    | Set_hangman _ -> Init

  type game_master_moves =
    {set_hangman: move_activity} [@@deriving show, json, yojson]

  type player_moves =
    {solve_hangman: move_activity;
     try_letter: move_activity;
    } [@@deriving show, json, yojson]

  type user_id = int64 [@@deriving show, yojson]
  type play_response =
    Ok | Wrong_move of string | Winner of user_id
    | Invalid of string
                   [@@deriving show, yojson]

  let abstract_play_response :
        play_response -> abstract_play_response =
    function
    | Ok -> Ok
    | Winner id -> Ended (IdMap.add id 1 IdMap.empty)
    | Wrong_move _ -> Ok
    | Invalid msg -> (Invalid msg)

  type running_game = {
      current_hangman: (char * bool) list;
      scores: scores;
    }
                        [@@deriving show,yojson]

  type running_game_gm_view = {
      displayed_hangman: string;
      current_hangman: string;
    }
                                [@@deriving show,yojson]

  type running_game_player_view = {
      displayed_hangman: string;
    }
                                    [@@deriving show, yojson]

  type 'a game_state =
    Loading | Ended of user_id | Running of 'a
                                              [@@deriving show,yojson]

  let hangman_to_hidden_string l =
    String.concat "" (List.map (fun (c,b) -> if b then Char.escaped c else "_") l)

  let hangman_to_string l =
    String.concat "" (List.map (fun (c,_) -> Char.escaped c) l)

  let string_to_hangman s =
    List.init (String.length s) (String.get s)
    |> List.map (fun c -> (c, false))


  let gm_view_of_game_state (s: running_game game_state) =
    match s with
      Running state ->
       Running {
           current_hangman = hangman_to_string state.current_hangman;
           displayed_hangman = hangman_to_hidden_string state.current_hangman;
         }
    | Loading -> Loading
    | Ended id -> Ended id


  type t =
    {
      game_master_id: user_id;
      players_id: user_id list;
      game_state: running_game game_state;
    }
      [@@deriving show, yojson]

  let players st =
    UserSet.of_list st.players_id

  let has_user user_id st =
    List.mem user_id st.players_id

  type game_master_view =
    {
      game_master_id: user_id;
      players_id: user_id list;
      game_state: running_game_gm_view game_state;
      available_moves: game_master_moves;
    } [@@deriving show, yojson]

  let game_master_view_of_t (modifier: view_modifier) (st:t) =
    let available_moves = match modifier, st.game_state with
      | Disabled, _ -> {set_hangman = Disabled}
      | _, Running _ -> {set_hangman = GmReset}
      | _, _ -> {set_hangman = Enabled}
    in
    {
      game_master_id = st.game_master_id;
      players_id = st.players_id;
      game_state = gm_view_of_game_state st.game_state;
      available_moves;
    }

  type player_view = {
      game_state: running_game_player_view game_state;
      available_moves: player_moves;
    }
                       [@@deriving show, yojson]

  type view = Game_master_view of game_master_view | Player_view of player_view
                                                                      [@@deriving show,yojson]

  let player_view_of_game_state modifier (uid: user_id) (s: running_game game_state) =
    match s with
      Running state ->
       Running {displayed_hangman = hangman_to_hidden_string state.current_hangman;}
    | Loading -> Loading
    | Ended id -> Ended id

  (* let player_view (st: t) : player_view =
   *   {game_state = player_view_of_game_state st.game_state} *)

  let view modifier user_id (st : t) =
    if user_id = st.game_master_id then
      Game_master_view (game_master_view_of_t modifier st)
    else
      let available_moves =
        match modifier, st.game_state with
        | Only_player, Running _ -> {solve_hangman = Important; try_letter = Important}
        | Disabled, _ -> {solve_hangman = Disabled; try_letter = Disabled}
        | _, _ -> {solve_hangman = Enabled; try_letter = Enabled}
      in
      Player_view
        {game_state = player_view_of_game_state modifier user_id st.game_state;
         available_moves}

  let new_game game_master_id players_id : t =
    {
      game_master_id;
      players_id;
      game_state = Loading;
    }

  let try_letter uid letter (game_state: running_game) : running_game game_state =
    let () = Format.printf "try_letter %c" letter in
    let () = Format.print_flush() in
    let (reved, found_one, all_solved) =
      List.fold_left
        (fun (acc,found_one, all_solved) (c,b) ->
          if not b && letter = c then
            ((c,true)::acc, true, all_solved)
          else if not b then
            ((c, b)::acc, found_one, false)
          else
            ((c, b)::acc, found_one, all_solved)
        )
        ([], false, true)
        game_state.current_hangman
    in
    if all_solved then
      Ended uid
    else
      let scores =
        if found_one then
          IdMap.update uid
            (function | Some i -> Some (i+1) | None -> Some 1)
            game_state.scores
        else
          game_state.scores
      in
      Running {
          current_hangman = List.rev reved;
          scores;
        }





  let init_scores gm_id players =
    List.fold_left
      (fun acc id -> if id <> gm_id then IdMap.add id 0 acc else acc)
      IdMap.empty
      players

  let play (id: user_id) (st:t) (m: move) : t * play_response =
    match m, st.game_state with
    | Solve_hangman s, Running game_state ->
       if s = hangman_to_string game_state.current_hangman then
         {st with game_state = Ended id}
       , Winner id
       else
         st
       , Wrong_move "wrong anagram"

    | Set_hangman s, _ when id = st.game_master_id ->
       {st with
         game_state =
           Running {
               scores = init_scores st.game_master_id st.players_id;
               current_hangman = string_to_hangman s;}}
      , Ok

    | Set_hangman s, _ ->
       st
      , Invalid ("Only game master can set hangman value")
    | (Try_letter l, Running game_state) ->
       let game_state = try_letter id l game_state in
       let resp = match game_state with
         | Ended uid -> Winner uid
         | _ -> Ok
       in
       {st with game_state}
       , resp

    | (Try_letter _ | Solve_hangman _), (Ended _ | Loading) ->
       st
      , Invalid "game loading or ended"

  let remove_user uid (st : t) =
    if uid = st.game_master_id then
      None
    else
      let players_id = List.filter (fun id -> id <> uid) st.players_id in
      match players_id with
      | [] | [_] -> None
      |  _ -> Some {st with players_id}
end
