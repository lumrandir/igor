import "../css/app.scss"

import { Elm } from '../elm/src/Main.elm'

const app = Elm.Main.init({
  node: document.getElementById('elm-app'),
  flags: []
});
