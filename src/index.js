import './main.css';
import { Elm } from './Main.elm';
import { unregister } from './registerServiceWorker';

Elm.Main.init({
  node: document.getElementById('root')
});

unregister();
