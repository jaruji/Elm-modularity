module Analyser.FileTree exposing (..)

type alias Folder =
    {
        name: String,
        files: List File,
        folders: List Folder
    }

type alias File  =
    {
        name: String
    }

createFileTree: List (List String) -> List Folder
createFileTree paths =
    []