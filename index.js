import { Elm } from './src/Main.elm'

if (module.hot) {
    module.hot.dispose(() => {
        window.location.reload();
    });
}

var app = Elm.Main.init({
  node: document.querySelector('main')
})

// console.log(app)

app.ports.cache.subscribe((data) => {
  console.log(data)
  return localStorage.setItem('cache', JSON.stringify(data))
})

// app.ports.getFromCache.sends((key) =>
//   localStorage.getItem(key)
// )

// app.ports.clearCache.subscribe(() =>
//   localStorage.clear()
// )
