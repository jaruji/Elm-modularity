port module Ports exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode exposing(..)

port request: () -> Cmd msg

port getProjectDirTree: (Maybe Encode.Value -> msg) -> Sub msg

port saveState: Encode.Value -> Cmd msg