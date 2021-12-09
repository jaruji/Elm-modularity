module Dashboard.Analysis.Metrics exposing (..)

import Svg exposing (svg)
import Html exposing (..)
import Html.Attributes exposing (..)
import Dashboard.Components.FileSelector exposing (MyFile)
import Analyser.ASTHelper as ASTHelper exposing (..)

{--
    metrics:
        LOC
        Comments
        Number of types
        Number of functions
        Number of Boilerplate Msgs
        Locate MVU triplets
        Number of imports used together with number of calls for each import
--}

type alias Model = 
    {
        files: List MyFile
    }

type Msg
    = NoOp


init: List MyFile -> ( Model, Cmd Msg)
init files =
    ({files = files}, Cmd.none)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)

view: Model -> Html Msg
view model =
    section[ class "grid" ][
        h1[][ text "Elm metrics"],
        div[ class "subtext" ][ text "Software metrics used to compute and visualize the complexity of the project, relationships between modules and their modularity."],
        let
            files = model.files
        in
            case List.length files of
                0 ->
                    article[][ text "No Elm project loaded" ]
                _ ->
                    article[]((List.map (\file -> case file.ast of
                                            Just ast ->
                                                div[][
                                                    h2[][ text file.name ],
                                                    let 
                                                        parsedString = String.lines file.content
                                                    in
                                                        div[][
                                                            text ("Total lines: " ++ String.fromInt (List.length parsedString)),
                                                            br[][],
                                                            text ("LOC: " ++ String.fromInt (List.length (List.filter removeEmpty parsedString))),
                                                            div[][
                                                                text ( "Comment line count: " ++ String.fromInt (List.foldl numberOfCommentedLines 0 (ASTHelper.getCommentLines (ASTHelper.processRawFile ast))))
                                                            ]
                                                        ]
                                                ]
                                            Nothing ->
                                                text ""
                                        )
                            ) files)
    ]

removeEmpty: String -> Bool
removeEmpty string =
    if string == "" then
        False
    else
        True

numberOfCommentedLines: (Int, Int) -> Int -> Int
numberOfCommentedLines (start, end) acc =
    acc + end - start + 1

getModel: (Model, Cmd Msg) -> Model
getModel (model, cmd) =
  model