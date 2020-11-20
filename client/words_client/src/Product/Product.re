open ProductJson;
open BasicTypesJson;

[@bs.val] external window: Js.t({..}) = "window";
/* let base_url = "http://localhost:8080"; */
let base_url = "http://"++window##location##host;
let play_move_url = base_url++"/tuto/hello";
let game_state_url = base_url++"/current_game_json";


module Sequence = {
  [@react.component]
    let make = (~left_child, ~right_child) => {
      {React.array([| left_child,

                  React.string(Js.String.fromCharCode(8658)),

                    right_child |])}
    }
};

module SubGame = {
[@react.component]
  let make = (~left_child, ~right_child) => {
    {React.array([|
                  /* {React.string("play one move on the main game after winning the secondary")} */
                  right_child
                  , React.string(Js.String.fromCharCode(9660))
                  , left_child |])}
    }
};


[@react.component]
  let make = (~parent_product_type: option(product_type), ~json_view, ~left_comp, ~right_comp) => {
    let (json_l, json_r, product_type) = split_views(json_view);
    let className =
      if (parent_product_type == None) {"top_game"}
      else switch (product_type) {
      | Sequence => "game_horizontal"
      | SubGame => "game_vertical"
    };
    let left_child = left_comp(Some(product_type), json_l);
    let right_child = right_comp(Some(product_type), json_r);
    let comp = switch (product_type) {
      | Sequence => <Sequence left_child right_child/>
      | SubGame => <SubGame left_child right_child/>
    };
    if (Some(product_type) == parent_product_type){comp}
      else {<div className> comp </div>};
};
