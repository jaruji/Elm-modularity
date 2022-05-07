module Analyser.File exposing (..)
import Elm.Parser exposing (parse)
import Elm.RawFile exposing (..)
import Elm.Syntax.File exposing (..)
import Analyser.AST.Declaration exposing (Declaration_)
import Dashboard.Components.FileSelector exposing (MyFile)
import Set exposing (Set, fromList, empty)

type alias File_ =
    {
        name: String, 
        content : String,
        ast : Maybe Elm.RawFile.RawFile,
        declarations: List Declaration_,
        calledModules: Set String,
        calledDeclarations: Set String
    }

wrapElmFile: MyFile -> List Declaration_ -> Set String -> Set String -> File_
wrapElmFile {buff, path, name, content, ast, status} declarations setM setD =
    { name = name, content = content, ast = ast, declarations = declarations, calledModules = setM, calledDeclarations = setD}

wrapOtherFile: MyFile -> File_
wrapOtherFile {buff, path, name, content, ast, status} =
    { name = name, content = content, ast = ast, declarations = [], calledModules =  Set.empty, calledDeclarations = Set.empty}
    