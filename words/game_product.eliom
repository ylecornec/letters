include Game_types

type product_type = Sequence | SubGame [@@deriving show, yojson]

module type CONTROLER =
  sig
    type t[@@deriving show, yojson]
    (* type left_moves
     * type left_play_response
     * type right_play_response
     * type right_moves *)
    type controler_callbacks =
      ((abstract_play_response -> t) * (abstract_play_response -> abstract_play_response), string) result
    (* type move_types *)
    val new_controller: unit -> t
    val move_left: t -> user_id -> abstract_move -> controler_callbacks
    val move_right: t -> user_id -> abstract_move -> controler_callbacks
    val view_modifier_left_gm: t -> view_modifier
    val view_modifier_right_gm: t -> view_modifier
    val view_modifier_left_player: t -> user_id -> view_modifier
    val view_modifier_right_player: t -> user_id -> view_modifier
    val product_type: product_type
    (* val allowed_moves: t -> move_types list *)
  end


(** finish left game then right one and sum the scores*)
module ControlerSequence : CONTROLER
  =
  struct
    type user_id = int64

    (** remember the scores of left game *)
    type t = Left | Right of scores [@@deriving show,yojson]

    let new_controller () = Left
    let add_scores scores_left = function
      | Ended scores_right -> Ended (add_scores scores_right scores_left)
      | resp -> resp

    let stop_winner = function
      | Ended _ -> Ok
      | resp -> resp

    type controler_callbacks = ((abstract_play_response -> t) * (abstract_play_response -> abstract_play_response), string) result

    let move_left t uid (left_move: abstract_move) : controler_callbacks =
      match t, left_move with
      (* | Right _, _ -> Error("left game already ended") *)
      | Right _, Init -> Ok ((function _ -> t), stop_winner)
      | Right _, _ -> Error("left game already ended")
      | Left, _ -> Ok(
                       (function | Ended scores -> Right scores
                                 | _ -> t)
                       , stop_winner)

    let move_right t uid (right_move: abstract_move) : controler_callbacks =
      match t, right_move with
      | Left, Init -> Ok ((function _ -> t), stop_winner)
      | Left, _ -> Error "Left game must be completed"
      | Right left_scores, _ ->
         Ok ((function | Ended _ -> Left | _ -> Right left_scores),
             add_scores left_scores)

    let view_modifier_left_player t uid =
      match t with
      | Left -> Default
      | Right _ -> Disabled

    let view_modifier_right_player t uid : view_modifier =
      match t with
      | Right _ -> Default
      | Left -> Disabled

    let view_modifier_left_gm _ : view_modifier = Default
    let view_modifier_right_gm: t -> view_modifier = function
      | Left -> Default
      | Right _ -> Default

    let product_type = Sequence
  end


(**
 Right game is a subgame of Left game.
 Player winning the subgame is allowed to make a move in the left one.
*)
module ControlerSubgame : CONTROLER
  =
  struct
    type user_id = int64
    type t =
      Allowed_left of user_id | Not_allowed_left [@@deriving show,yojson]

    let new_controller () = Not_allowed_left

    let propagate_winner = function resp -> resp
    let stop_winner = function
      | Ended _ -> Ok
      | resp -> resp

    type controler_callbacks = ((abstract_play_response -> t) * (abstract_play_response -> abstract_play_response), string) result

    let move_left t uid (left_move: abstract_move) : controler_callbacks =
      match t, left_move with
      | Not_allowed_left, Init -> Ok ((function _ -> t), propagate_winner)
      | Not_allowed_left, Internal ->
         Error (Format.asprintf "user %Ld has no right to play left move" uid)
      | Allowed_left _, Init -> Ok ((function _ -> t), propagate_winner)
      | Allowed_left winner_id, Internal when uid = winner_id ->
         Ok ((function | Ended _ -> Not_allowed_left
                       | Ok -> Not_allowed_left
                      | Invalid _ -> t)
           , propagate_winner)
      | Allowed_left _, _ ->
         Error (Format.asprintf "user %Ld has no right to play left move" uid)

    let move_right t uid (right_move: abstract_move) : controler_callbacks =
      match t, right_move with
      | Allowed_left _, Init -> Error("wait for move on left game before setting up new right game")
      | Not_allowed_left, Init -> Ok ((function _ -> t), stop_winner)
      | Allowed_left _, Internal -> Error("Waiting for move on left game")
      | Not_allowed_left, Internal ->
         Ok ((function | Ended scores ->(
                         match winner_of_score scores with
                         | None -> Not_allowed_left
                         | Some right_winner -> Allowed_left right_winner)
                       | Ok | Invalid _ -> t)
           , stop_winner)

    let view_modifier_left_player t uid =
      match t with
      | Allowed_left id when id = uid -> Only_player
      | Allowed_left _ -> Disabled
      | Not_allowed_left -> Disabled
    let view_modifier_right_player t uid : view_modifier =
      match t with
      | Allowed_left _ -> Disabled
      | Not_allowed_left -> Default

    let view_modifier_left_gm _ : view_modifier = Default
    let view_modifier_right_gm: t -> view_modifier = function
      | Allowed_left _ -> Disabled
      | Not_allowed_left -> Default

    let product_type = SubGame
  end

module Product (C: CONTROLER) (L: GAME) (R: GAME) : GAME =
  struct
    type move = L_move of L.move | R_move of R.move [@@deriving show, yojson]
    let random_move = L_move L.random_move
    let abstract_move = function
      | L_move m -> L.abstract_move m
      | R_move m -> R.abstract_move m

    type user_id = int64 [@@deriving show, yojson]
    type t = {
        game_master_id: user_id;
        players: user_id list;
        left : L.t;
        right: R.t;
        controller: C.t;
      }

    type play_response = abstract_play_response
                                    [@@deriving show, yojson]

    let abstract_play_response = function x -> x
      (* | L_play_response resp -> Ok
       * | R_play_response resp -> Ok
       * | Error_play_response _ -> Invalid *)

    (* type available_moves = Left | Right | Both [@@deriving show, yojson] *)

    type game_master_view =
      {
        left_view: L.view;
        right_view: R.view;
        (* controller: C.t; *)
        product_type: product_type;
      }

        [@@deriving show, yojson]

    type 'a game_state =
      Loading | Ended of user_id | Running of 'a
                                                [@@deriving show,yojson]

    type player_view = game_master_view
                         [@@deriving show,yojson]

    type view = Game_master_view of game_master_view | Player_view of player_view
                                                                        [@@deriving show, yojson]

    let new_game (game_master_id: user_id) (players: user_id list) =
      {
        game_master_id;
        players;
        left = L.new_game game_master_id players;
        right = R.new_game game_master_id players;
        controller = C.new_controller ();
      }

    let compose_modifiers (m1: view_modifier) (m2: view_modifier) : view_modifier =
      match m1, m2 with
      | Disabled, _ | _, Disabled -> Disabled
      | Only_player, _ | _, Only_player -> Only_player
      | _, _ -> Default

    let game_master_view_of_t modifier uid (st:t) : game_master_view =
      let left_mod = compose_modifiers modifier @@ C.view_modifier_left_gm st.controller in
      let right_mod = compose_modifiers modifier @@ C.view_modifier_right_gm st.controller in
      {
        left_view = L.view left_mod uid st.left;
        right_view = R.view right_mod uid st.right;
        (* controller = st.controller; *)
        product_type = C.product_type;
        (* available_moves = (); *)
      }

    let players_view_of_t modifier uid (st:t) : player_view =
      let left_mod = compose_modifiers modifier @@ C.view_modifier_left_player st.controller uid in
      let right_mod = compose_modifiers modifier @@ C.view_modifier_right_player st.controller uid in
      {
        left_view = L.view left_mod uid st.left;
        right_view = R.view right_mod uid st.right;
        (* controller = st.controller; *)
        product_type = C.product_type;
      }

    let view modifier uid st: view =
      if uid = st.game_master_id then
        Game_master_view (game_master_view_of_t modifier uid st)
      else
        Player_view (players_view_of_t modifier uid st)

    let has_user user_id (st:t) = List.mem user_id st.players
    let players (st:t) = UserSet.of_list st.players
    let play uid (st:t) move : t * play_response =
      match move with
      | L_move m ->
         begin
         match C.move_left st.controller uid (L.abstract_move m) with
         | Error msg -> st, Invalid msg
         | Ok (state_cb, resp_cb) ->
            let left, resp = L.play uid st.left m in
            let abs_resp = L.abstract_play_response resp in
            {st with controller = state_cb abs_resp;
                     left},
            resp_cb abs_resp
         end
      | R_move m ->
         begin
         match C.move_right st.controller uid (R.abstract_move m) with
         | Error msg -> st, Invalid msg
         | Ok (state_cb, resp_cb) ->
            let right, resp = R.play uid st.right m in
            let abs_resp = R.abstract_play_response resp in
            {st with controller = state_cb abs_resp;
                     right},
            resp_cb abs_resp
         end

    let remove_user user_id (st:t) =
      if user_id = st.game_master_id then None
      else
        let left = L.remove_user user_id st.left in
        let right = R.remove_user user_id st.right in
        match left, right with
        | Some left, Some right ->
          Some {st with left; right}
        | _ -> None

  end
