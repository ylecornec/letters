type game_shape = | Anagram | Hangman | Product(game_shape, game_shape);
let rec components_of_game_shape = (encapsulation_acc, shape) => {
  switch(shape) {
  | Anagram => {(_parent_product_type, json) => {
      let view = AnagramJson.decode_view(json);
      <AnagramAnyView encapsulate_move=encapsulation_acc view/>
  }}
  | Hangman => {(_parent_product_type, json) => {
      let view = HangmanJson.decode_view(json);
      <HangmanAnyView encapsulate_move=encapsulation_acc view/>
  }}
  | Product(l, r) => { (parent_product_type, json_view) => {
      let encapsulation_acc_l = {
        json_command => {
        encapsulation_acc(Js.Json.array([|Js.Json.string("L_move"), json_command|]))}};
      let encapsulation_acc_r = {
        json_command => {
        encapsulation_acc(Js.Json.array([|Js.Json.string("R_move"), json_command|]))}};
      let decode_l = components_of_game_shape(encapsulation_acc_l, l);
      let decode_r = components_of_game_shape(encapsulation_acc_r, r);
      <Product parent_product_type json_view left_comp=decode_l right_comp=decode_r />
  }}
  }
}
