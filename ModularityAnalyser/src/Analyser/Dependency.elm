module Analyser.Dependency exposing (..)

import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import Json.Decode.Pipeline as Pipeline exposing (required, optional, hardcoded, custom)
import Parser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, style, href, attribute)
import String exposing (toLower)

type alias Model = 
    {
        jsonType: String,
        version: String,
        direct: List Dependency,
        indirect: List Dependency
        -- testDirect: List Dependency,
        -- testIndirect: List Dependency
    }

type alias Dependency = 
    {
        package: String,
        version: String
    }

-- decode: Decode.Value -> Result Error String
-- decode =
--     Decode.decodeValue Decode.string

decodeElmJson =
     Decode.succeed Model
         |> required "type" Decode.string
         |> required "elm-version" Decode.string
         |> custom (at ["dependencies", "direct"] dependencyDecoder)
         |> custom (at ["dependencies", "indirect"] dependencyDecoder)
        --  |> custom (at ["test-dependencies", "direct"] dependencyDecoder)
        --  |> custom (at ["test-dependencies", "indirect"] dependencyDecoder)

dependencyDecoder =
    Decode.keyValuePairs (Decode.string) |> Decode.map (List.map (\(key, value) -> Dependency key value))

createDependency: List (String, String) -> List Dependency
createDependency list =
    List.map(\leaf -> Dependency (Tuple.first leaf) (Tuple.second leaf)) list
        
view: Model -> Html msg
view model =
    -- div[][
    --     text (model.jsonType ++ " "),
    --     text model.version,
    --     div[](List.map(\dep -> div[][ text dep.package, text dep.version, br[][]]) model.direct),
    --     hr[][],
    --     div[](List.map(\dep -> div[][ text dep.package, text dep.version, br[][]]) model.indirect)
    --     -- div[](List.map(\dep -> div[][ text dep.package, text dep.version, br[][]]) model.testDirect),
    --     --  hr[][],
    --     -- div[](List.map(\dep -> div[][ text dep.package, text dep.version, br[][]]) model.testIndirect),
    --     -- hr[][]
    -- ]
    div[][
        constructTable "Direct dependencies" model.direct,
        constructTable "Indirect dependencies" model.indirect
    ]

constructTable: String -> List Dependency -> Html msg
constructTable title dependencies =
    ul[ class "responsive-table" ][
        h2[ style "margin" "25px" ][ text title ],
        div[ class "explanation"][
            text "All ",
            text (title |> toLower),
            text """ of the project, 
            along with their respective version and links. """
        ],
        li[class "table-header"][
            div[class "col col-1"][ text "Name"],
            div[class "col col-2"][ text "Version" ],
            div[class "col col-3"][ text "Link" ]
        ],
        List.map(\leaf -> li[ class "table-row" ][
            div[class "col col-1"][ text leaf.package ],
            div[class "col col-2"][ text leaf.version ],
            div[class "col col-3"][ 
                a [ href ("https://package.elm-lang.org/packages/" ++ leaf.package ++ "/" ++ leaf.version), attribute "target" "_blank"][
                    text "Link"
                ]
            ]
        ]) dependencies |> div[]
    ]
    


-- parseElmJson = Parser Model
-- parseElmJson =
--     Parser.succeed Model
--         |. symbol "{"
--         |. spaces
--         |. keyword "type"
--         |. spaces
--         |. symbol ":"
--         |=  
