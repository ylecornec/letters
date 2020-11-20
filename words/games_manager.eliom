open Shared_types
open Game_types
open Result
module type GAMES_MANAGER =
  sig
    type game_id [@@deriving show]
    type t
    type game
    type move
    type user_id = int64
    type ask_new_game_response = Shared_types.ask_new_game_response
    type play_response = Play_response of Game.play_response | Not_logged_in | Games_manager_error of string
    val request_game : t -> int64 -> ask_new_game_response * t
    val new_manager : t
    val game_of_user : t -> user_id -> (game_id * game) option
    val play_move_on_user_game : user_id -> move -> t -> t * play_response
    val start_new_game: user_id -> user_id list -> t -> t * ask_new_game_response
    val remove_user: user_id -> t -> t * unit
  end

module type GAME = GAME with type play_response = Game.play_response

module Build_Games_manager (Game: GAME) :
GAMES_MANAGER with type game = Game.t and type move = Game.move =
  struct
    type user_id = int64 [@@deriving show]
    type game_id = int [@@deriving show]
    type game = Game.t
    type move = Game.move
    type ask_new_game_response = Shared_types.ask_new_game_response
    type play_response = Play_response of Game.play_response | Not_logged_in | Games_manager_error of string
    module GamesMap =
      Map.Make
        (struct
          type t = game_id
          let compare = Stdlib.compare
        end)

    type t =
      {
        pending_requests: UserSet.t;
        running: Game.t GamesMap.t;
        ingames_users: UserSet.t;
      }

    let create_new_game user1 user2 =
      Game.new_game user1 user2

    let key_of_users user1 user2 =
      if user1 < user2 then
        (user1, user2)
      else
        (user2, user1)

    let same_request gm user_id = UserSet.mem user_id gm.pending_requests

    let start_new_game game_master players gm =
      if List.exists (fun id -> UserSet.mem id gm.ingames_users) players then
        gm
      , New_game_error (Format.asprintf "some user is already in game")
      else
        let game_key = Uniq_id.get_uniq_id () in
        let running = GamesMap.add game_key (Game.new_game game_master players) gm.running in
        let ingames_users = UserSet.union (UserSet.of_list players) gm.ingames_users in
        {gm with running; ingames_users}
        , New_game_id game_key

    let request_game gm user_id : ask_new_game_response * t =
      if UserSet.mem user_id gm.ingames_users then
        New_game_error (Format.asprintf "user %a is already in game" pp_user_id user_id)
      , gm
      else if same_request gm user_id then
        New_game_error (Format.asprintf "user %a is already waiting for a game with same config" pp_user_id user_id)
      , gm
      else if not @@ UserSet.is_empty gm.pending_requests then
        let opponnent_id = UserSet.choose gm.pending_requests in
        let pending_requests = UserSet.remove opponnent_id gm.pending_requests in
        let game_key = Uniq_id.get_uniq_id () in
        let running = GamesMap.add game_key (Game.new_game user_id [user_id; opponnent_id]) gm.running in
        let ingames_users = UserSet.add user_id (UserSet.add opponnent_id gm.ingames_users) in
        New_game_id game_key
        , {pending_requests; running; ingames_users}
      else
        Waiting_for_game
      , {gm with pending_requests = UserSet.add user_id gm.pending_requests}

    let new_manager =
      {
        pending_requests = UserSet.empty;
        running =  GamesMap.empty;
        ingames_users = UserSet.empty;
      }

    let user_in_game_id gm user_id (game_id:int) =
      try
        let game = GamesMap.find game_id gm.running in
        Game.has_user user_id game
      with Not_found -> false

    let game_of_user gm user_id =
      let m = GamesMap.filter (fun game_id _ -> user_in_game_id gm user_id game_id) gm.running in
      GamesMap.choose_opt m

    let play_move_on_user_game uid move gm : t * play_response =
      match game_of_user gm uid with
      | None -> gm, Games_manager_error (Format.asprintf "User %a is not in game" pp_user_id uid)
      | Some (game_id, game) ->
         let game, response = Game.play uid game move in
         {gm with running = GamesMap.add game_id game gm.running}
         , Play_response response

   let remove_players_from_user_set =
     List.fold_left (fun set p -> UserSet.remove p set)

   let remove_user uid gm : t * unit =
      match game_of_user gm uid with
      | None -> gm, ()
      | Some (game_id, game) ->
         begin
         match Game.remove_user uid game with
         | Some game ->
            {gm with running = GamesMap.add game_id game gm.running;
                     ingames_users = UserSet.remove uid gm.ingames_users;}
         , ()
         | None ->
            let ingames_users = UserSet.diff gm.ingames_users (Game.players game) in
            {gm with running = GamesMap.remove game_id gm.running; ingames_users}
         , ()

         end
  end

include Build_Games_manager(Game)

let global_games_manager = Eliom_reference.eref ~scope:`Site new_manager
let global_games_manager_mutex = Lwt_mutex.create ()
let update_game_manager (f: t -> t * 'res) =
  (* with_lock : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t *)
  Lwt_mutex.with_lock global_games_manager_mutex
    (fun () ->
      let%lwt gm = Eliom_reference.get global_games_manager in
      let gm, response = f gm in
      let%lwt () = Eliom_reference.set global_games_manager gm in
      Lwt.return response
    )

let get_games_manager () =
  let%lwt () = Lwt_mutex.lock global_games_manager_mutex in
  Eliom_reference.get global_games_manager

let set_games_manager gm =
  let%lwt () = Eliom_reference.set global_games_manager gm in
  Lwt.return @@ Lwt_mutex.unlock global_games_manager_mutex

let play_my_move_on_my_game move : play_response Lwt.t =
  match Eliom_reference.Volatile.get User_id_ref.user_id_ref with
  | None -> Lwt.return Not_logged_in
  | Some my_id ->
     update_game_manager @@ play_move_on_user_game my_id move


let%server leave_my_game () : unit Lwt.t =
  match%lwt User_id_ref.get_id() with
  | None -> Lwt.return_unit
  | Some my_id ->
     update_game_manager @@ remove_user my_id

let%client leave_my_game =
  ~%(Eliom_client.server_function [%json: unit] leave_my_game)

let start_new_game game_master players =
  update_game_manager @@ start_new_game game_master players

