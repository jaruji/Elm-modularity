module Analyser.File exposing (..)
import Elm.Parser exposing (parse)
import Elm.RawFile exposing (..)
import Elm.Syntax.File exposing (..)
import Analyser.AST.Declaration exposing (Declaration_)
import Dashboard.Components.FileSelector exposing (MyFile)

type alias File_ =
    {
        name: String, 
        content : String,
        ast : Maybe Elm.RawFile.RawFile,
        declarations: List Declaration_
    }


wrapElmFile: MyFile -> List Declaration_ -> File_
wrapElmFile {buff, path, name, content, ast, status} declarations =
    { name = name, content = content, ast = ast, declarations = declarations }

wrapOtherFile: MyFile -> File_
wrapOtherFile {buff, path, name, content, ast, status} =
    { name = name, content = content, ast = ast, declarations = [] }
    