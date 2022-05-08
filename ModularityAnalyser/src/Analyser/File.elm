module Analyser.File exposing (..)
import Elm.Parser exposing (parse)
import Elm.RawFile exposing (..)
import Elm.Syntax.File exposing (..)
import Analyser.AST.Declaration exposing (Declaration_)
import Dashboard.Components.FileSelector exposing (MyFile)
import Set exposing (Set, fromList, empty)
import Dict exposing (Dict, empty)

type alias File_ =
    {
        name: String, 
        content : String,
        ast : Maybe Elm.RawFile.RawFile,
        declarations: List Declaration_,
        calledModules: Dict String Int,
        calledByModules: Set String
    }

wrapElmFile: MyFile -> List Declaration_ -> Dict String Int -> Set String -> File_
wrapElmFile {buff, path, name, content, ast, status} declarations setM setD =
    { name = name, content = content, ast = ast, declarations = declarations, calledModules = setM, calledByModules = setD}

wrapOtherFile: MyFile -> File_
wrapOtherFile {buff, path, name, content, ast, status} =
    { name = name, content = content, ast = ast, declarations = [], calledModules = Dict.empty, calledByModules = Set.empty}
    