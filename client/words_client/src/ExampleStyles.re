let reasonReactBlue = "#48a9dc";

// The {j|...|j} feature is just string interpolation, from
// bucklescript.github.io/docs/en/interop-cheatsheet#string-unicode-interpolation
// This allows us to conveniently write CSS, together with variables, by
// constructing a string
let style = {j|
  body {
    background-color: rgb(224, 226, 229);
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    /* height: 200vh; */
  }
  button {
    background-color: white;
    color: $reasonReactBlue;
    box-shadow: 0 0 0 1px $reasonReactBlue;
    border: none;
    padding: 8px;
    font-size: 16px;
  }
  button:active {
    background-color: $reasonReactBlue;
    color: white;
  }
  .container {
    margin: 12px 0px;
    box-shadow: 0px 4px 16px rgb(200, 200, 200);
    width: 720px;
    border-radius: 12px;
    font-family: sans-serif;
  }
  .containerTitle {
    background-color: rgb(242, 243, 245);
    border-radius: 12px 12px 0px 0px;
    padding: 12px;
    font-weight: bold;
  }
  .containerContent {
    background-color: white;
    padding: 16px;
    border-radius: 0px 0px 12px 12px;
  }
  .game_horizontal {
    display: flex;
    align-items: center;
    border-radius: 12px;
    border: solid;
    border-width: 3px;
    margin: 1em;
    padding: 1em;
    box-shadow: 0px 4px 16px rgb(200, 200, 200);
}

  .game_vertical {
    border-radius: 12px;
    border: solid;
    border-width: 3px;
    margin: 1em;
    padding: 1em;
    text-align: center;
    box-shadow: 0px 4px 16px rgb(200, 200, 200);
}

  .game {
    border-radius: 12px;
    border: solid;
    border-width: 3px;
    margin: 1em;
    padding: 1em;
    text-align: center;
    box-shadow: 0px 4px 16px rgb(200, 200, 200);
}

  .top_game {
    margin: 1em;
    padding: 1em;
    text-align: center;
}


|j};
