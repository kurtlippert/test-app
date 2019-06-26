import { Elm } from './Main.elm'

if (module.hot) {
    module.hot.dispose(() => {
        window.location.reload();
    });
}

// const storedState = localStorage.getItem('cache')
// const startingState = storedState ? localStorage.getItem('cache') : null
const app = Elm.Main.init({
  node: document.querySelector('main'),
  // flags: startingState,
})

// app.ports.cache.subscribe((data) => {
//   console.log(data)
//   // localStorage.setItem('cache', JSON.stringify(data))
// })

// app.ports.getCache.subscribe(() => {
//   console.log(localStorage.getItem('cache'))
//   app.ports.fromCache.send(localStorage.getItem('cache'))
//   // return localStorage.getItem('cache')
// })

// app.ports.clearCache.subscribe(() =>
//   localStorage.clear()
// )
