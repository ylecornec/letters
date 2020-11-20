(* include Game_types
 * module Winnable_game_of_scored_game(G: SCORED_GAME): WINNABLE_GAME =
 *   struct
 *     include G
 *     type abstract_play_response_type = abstract_play_response
 *     let winners_of_scores scores =
 *       fst @@ IdMap.fold
 *                (fun id id_score ((winners, max_score) as acc) ->
 *                  if id_score > max_score then
 *                    ([id], id_score)
 *                  else if id_score = max_score then
 *                    (id::winners, max_score)
 *                  else
 *                    acc)
 *                scores
 *                ([], 0)
 * 
 *     let winner_of_winners winners = List.hd winners
 * 
 *     let abstract_play_response play_response =
 *       match G.abstract_play_response play_response with
 *       | Simple abstract_play_response -> abstract_play_response
 *       | Ended scores -> Winner (winner_of_winners @@ winners_of_scores scores)
 * 
 *   end *)
