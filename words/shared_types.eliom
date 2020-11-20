type%shared game_id = int
type%shared ask_new_game_response = New_game_id of game_id | Waiting_for_game | New_game_error of string
