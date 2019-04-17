import { Elm } from './src/Main.elm'

if (module.hot) {
    module.hot.dispose(() => {
        window.location.reload();
    });
}

Elm.Main.init({
  node: document.querySelector('main')
})
