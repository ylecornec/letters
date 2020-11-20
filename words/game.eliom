(* include Anagram *)
include Game_types
(* include Hangman_game.Hangman(Anagram) *)

include Game_product
module Prod1 = Product(ControlerSequence)(Anagram)(Anagram)
module Prod2 = Product(ControlerSequence)(Anagram)(Prod1)
module Prod3 = Product(ControlerSequence)(Anagram)(Prod2)
module Prod4 = Product(ControlerSubgame)(Hangman_game.Hangman)(Prod3)
include Prod4



