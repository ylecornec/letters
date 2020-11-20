type user_id = int64 [@@deriving show,yojson]
module UserSet =
  Set.Make
    (struct
      type t = user_id
      let compare = Stdlib.compare
    end)

module IdMap = Map.Make(struct type t = user_id let compare = Stdlib.compare end)
type abstract_move = Init | Internal
type scores = int IdMap.t
let scores_to_list scores =
  IdMap.fold
    (fun k v acc -> (k,v)::acc)
    scores
    []

let scores_of_list l =
  List.fold_left
   (fun acc (k,v) -> IdMap.add k v acc)
   IdMap.empty
   l

type scores_as_list = (user_id * int) list [@@deriving show, yojson]
let pp_scores fmt scores =
  Format.fprintf fmt "%a" pp_scores_as_list @@ scores_to_list scores

let scores_to_yojson scores =
  scores_as_list_to_yojson @@ scores_to_list scores

let scores_of_yojson json =
 Result.map scores_of_list @@ scores_as_list_of_yojson json

type abstract_play_response =
  Ok | Invalid of string | Ended of scores [@@deriving show, yojson]

type view_modifier = Default | Only_player | Disabled
type move_activity = Disabled | GmReset | Enabled | Important [@@deriving show, json, yojson]

module type GAME = sig
  type move [@@deriving show, yojson]
  type t
  type user_id := user_id [@@deriving yojson]
  type play_response [@@deriving show, yojson]
  type game_master_view  [@@deriving show,yojson]
  type player_view [@@deriving show,yojson]
  type view = Game_master_view of game_master_view | Player_view of player_view [@@deriving show, yojson]

  val random_move: move
  val new_game: user_id -> user_id list -> t
  val play: user_id -> t -> move -> t * play_response
  val view: view_modifier -> user_id -> t -> view
  val has_user: user_id -> t -> bool
  val players: t -> UserSet.t
  val abstract_move: move -> abstract_move
  val abstract_play_response: play_response -> abstract_play_response
  val remove_user: user_id -> t -> t option (** if game cannot go on without this user then return None *)
end


let winners_of_scores scores =
  fst @@ IdMap.fold
           (fun id id_score ((winners, max_score) as acc) ->
             if id_score > max_score then
               ([id], id_score)
             else if id_score = max_score then
               (id::winners, max_score)
             else
               acc)
           scores
           ([], 0)

let winner_of_winners winners = List.hd winners

let winner_of_score scores =
  match winners_of_scores scores with
  | [id] -> Some id
  | _ -> None

let add_scores scores1 scores2 =
  IdMap.union (fun id sc1 sc2 -> Some(sc1+sc2)) scores1 scores2
