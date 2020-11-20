open AnagramJson;

[@bs.val] external document: Js.t({..}) = "document";
[@bs.val] external window: Js.t({..}) = "window";
/* let base_url = "http://localhost:8080"; */
let base_url = "http://"++window##location##host;
let play_move_url = base_url++"/tuto/hello";
let game_state_url = base_url++"/current_game_json";

/* let set_anagram = anagram => { */
/*   open Js.Promise; */
/*   let payload = Js.Dict.empty(); */
/*   Js.Dict.set(payload, "move", */
/*               Js.Json.array([| */
/*                              Js.Json.string("Set_anagram"), */
/*                              Js.Json.string(anagram) */
/*                              |])); */
/*     Fetch.fetchWithInit( */
/*     play_move_url, */
/*     Fetch.RequestInit.make( */
/*       ~method_=Post, */
/*       ~body=Fetch.BodyInit.make(Js.Json.stringify(Js.Json.object_(payload))), */
/*       ~headers=Fetch.HeadersInit.make({"Content-Type": "application/json"}), */
/*       () */
/*     ) */
/*   ) */
/*       |> then_(_jsonResponse =>{ */
/*           Js.log("click played") */
/*           resolve(); */
/*         }) */
/*       |> ignore */
/* }; */

module InputAnagram = {
  [@react.component]
    let make = (~encapsulate_move) => {
      let (value, onChange) = React.useState(() => "pls set anagram");
      let set_anagram = Utils.play_move(encapsulate_move, "Set_anagram");
      <form
          onSubmit= {e => {
        ReactEvent.Form.preventDefault(e);
        set_anagram(value);
      }} >
      <input
          onChange={
        event => {
            let value = ReactEvent.Form.target(event)##value;
            onChange(_ => value)
          }
      }
        />
    </form>;
  }
}

  module Loading = {
    [@react.component]
      let make = (~available_moves, ~encapsulate_move) => {
        let set_anagram = Utils.play_move(encapsulate_move, "Set_anagram");
        <div>
            {React.string("Please set anagram")}
        /* <InputAnagram encapsulate_move/> */
        <TextInput move_activity=available_moves.set callback=set_anagram />
      </div>
      }
  };


module Running = {
  [@react.component]
    let make = (~st: running_game) => {
      <div>
          {React.string("Solution : ")}
        {React.string(st.current_anagram)}
        <br/>
          {React.string("Displayed: ")}
        {React.string(st.displayed_anagram)}
      <br/>
    </div>
    }
};



[@react.component]
  let make = (~encapsulate_move, ~game_master_view) => {
    let available_moves = game_master_view.available_moves;
    <div className="game">
        {React.array(
    [|
     /* React.string("game master"), */
     switch (game_master_view.game_state){
     | Loading => <Loading available_moves encapsulate_move/>
     | Ended(_user_id) => <div>{React.string("User <todo> wins")} <Loading available_moves encapsulate_move/></div>
     | Running(st) => <Running st/>
     }
     |])}
    /* <button onClick>{React.string("incr")}</button> */
  </div>;
};
