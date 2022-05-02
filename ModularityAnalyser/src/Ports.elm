port module Ports exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode exposing(..)
import Analyser.Metric exposing (Metric)
import Dict exposing (Dict)

port request: () -> Cmd msg

port getProjectDirTree: (Maybe Encode.Value -> msg) -> Sub msg

port saveState: Encode.Value -> Cmd msg

port storeCheckpoint: Encode.Value -> Cmd msg

port loadCheckpoints: Encode.Value -> Dict String Metric