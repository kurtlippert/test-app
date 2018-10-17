if (module.hot) {
    module.hot.dispose(() => {
        window.location.reload();
    });
}

import('./src/Main.elm')
    .then(({ Elm }) => {
        var node = document.querySelector('main');
        Elm.Main.init({ node: node });
    });