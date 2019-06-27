import { Elm } from './Main.elm'

if (module.hot) {
    module.hot.dispose(() => {
        window.location.reload();
    });
}

const app = Elm.Main.init({
  node: document.querySelector('main')
})

app.ports.printModel.subscribe(model => console.log(model))
