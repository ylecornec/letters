[@bs.val] external window: Js.t({..}) = "window";
open BasicTypesJson;

let is_disable = move_activity => {
  switch move_activity {
    | Disabled => true
    | GmReset => true
    | _ => false
  }
};

[@react.component]
  let make = (~callback, ~move_activity) => {
    let (value, onChange) = React.useState(() => "");
    <form
        onSubmit= {e => {
      ReactEvent.Form.preventDefault(e);
      callback(value);
    }} >
    <input
        disabled=is_disable(move_activity)
        onChange={
      event => {
          let value = ReactEvent.Form.target(event)##value;
          onChange(_ => value)
        }
    }
      />
  </form>;
}
