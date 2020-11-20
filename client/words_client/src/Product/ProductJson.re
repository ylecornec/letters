open BasicTypesJson

type view('l, 'r) = {
  left_view: 'l ,
  right_view: 'r,
  product_type: product_type,
};

let decode_available_moves = json => {
  Js.log(json);
  Some(())
};

let decode_view(decode_l, decode_r) = json => {
  open Js.Json;
  open Js.Dict;
  switch (classify(json)) {
  | JSONObject(value) =>
    switch (Belt.Option.flatMap(get(value, "left_view"), decode_l)
            , Belt.Option.flatMap(get(value, "right_view"), decode_r)
            , Belt.Option.map(get(value, "product_type"), decode_product_type)
    ){
    | (Some(left_view), Some(right_view), Some(product_type)) =>
       {left_view, right_view, product_type}
    | _ => failwith("wrong fields in product view")
    }
  | _ => failwith("expected product view")
  }
};

let ignore_view_type = json => {
  open Js.Json;
  switch (classify(json)) {
  | JSONArray(a) when decodeString(a[0]) == Some("Game_master_view") => a[1]
  | JSONArray(a) when decodeString(a[0]) == Some("Player_view") => a[1]
  | _ => failwith("expected view")
  }
};

let split_views = json => {
  open Js.Json;
  open Js.Dict;
  switch (classify(ignore_view_type(json))) {
  | JSONObject(value) =>
    switch (get(value, "left_view"),
            get(value, "right_view"),
            Belt.Option.map(get(value, "product_type"), decode_product_type)
    ){
    | (Some(left_view_json), Some(right_view_json), Some(product_type)) =>
      {(left_view_json, right_view_json, product_type)}
    | _ => failwith("wrong fields in product view")
    }
  | _ => failwith("expected product view")
  }
};
