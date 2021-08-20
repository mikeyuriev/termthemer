import { Elm } from '../elm/Main.elm'
import '../scss/main.scss'

const stateKey = 'state';

const app = Elm.Main.init({
  flags: localStorage[stateKey] ?? "",
  node: document.querySelector('#app'),
});

app.ports.saveState.subscribe((state) => {
  window.localStorage[stateKey] = state;
});

