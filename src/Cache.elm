port module Cache exposing (cache, clearCache, getFromCache)

import Json.Encode as E


port cache : E.Value -> Cmd msg


port clearCache : () -> Cmd msg


port getFromCache : (E.Value -> msg) -> Sub msg
