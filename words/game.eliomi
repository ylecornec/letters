type move [@@deriving show, yojson]
type t
type user_id = int64 [@@deriving yojson]
type play_response [@@deriving show, yojson]

type game_master_view [@@deriving show,yojson]
type player_view [@@deriving show,yojson]

type view = Game_master_view of game_master_view | Player_view of player_view
[@@deriving show, yojson]

val random_move: move
val new_game: user_id -> user_id list -> t
val play: user_id -> t -> move -> t * play_response
val players: t -> Game_types.UserSet.t
val view: Game_types.view_modifier -> user_id -> t -> view
val has_user: user_id -> t -> bool
val abstract_move: move -> Game_types.abstract_move
val abstract_play_response: play_response -> Game_types.abstract_play_response
val remove_user: user_id -> t -> t option (** if game cannot go on without this user then return None *)
