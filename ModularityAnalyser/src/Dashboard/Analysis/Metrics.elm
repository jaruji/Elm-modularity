module Dashboard.Analysis.Metrics exposing (..)

import Svg exposing (svg)
import Html exposing (..)
import Html.Attributes exposing (..)
import Dashboard.Components.FileSelector exposing (MyFile)
import Analyser.ASTHelper as ASTHelper exposing (..)
import RadarChart exposing (..)

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
                    table[ style "text-align" "left" ][
                        tr[][
                            th[][ text "Module"],
                            th[][ text "LOC" ],
                            th[][ text "Comments" ],
                            th[][ text "NoD" ],
                            th[][ text "NoF" ],
                            th[][ text "NoT" ],
                            th[][ text "NoA" ]
                        ],
                        List.map tableContent files |> tbody[]
                    ]
    ]

tableContent: MyFile -> Html msg
tableContent file =
    case file.ast of
        Just ast ->
            let 
                parsedString = String.lines file.content
                processedFile = ASTHelper.processRawFile ast
            in
                tr[][
                    td[][ text file.name ],
                    td[][ 
                        text (String.fromInt (List.length parsedString))
                    ],
                    -- td[][ 
                    --     text (String.fromInt (List.length (List.filter removeEmpty parsedString)))
                    -- ],
                    td[][
                        text (String.fromInt (List.foldl numberOfCommentedLines 0 (ASTHelper.getCommentLines processedFile)))
                    ],
                    td[][
                        text (String.fromInt (ASTHelper.numberOfDeclarations processedFile))
                    ],
                    td[][
                        text ( String.fromInt (ASTHelper.numberOfFunctions processedFile))
                    ],
                    td[][
                        text (String.fromInt (ASTHelper.numberOfTypes processedFile))
                    ],
                    td[][
                        text (String.fromInt (ASTHelper.numberOfTypeAliases processedFile))
                    ]
                ]
        Nothing ->
            text ""

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