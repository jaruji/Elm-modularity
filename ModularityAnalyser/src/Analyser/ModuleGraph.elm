module Analyser.ModuleGraph exposing (..)

type alias Directory = {
    dirs: List Directory,
    files: List File,
    name: String
}

type alias File = {
    name: String
}