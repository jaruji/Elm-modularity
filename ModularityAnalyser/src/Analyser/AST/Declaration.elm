module Analyser.AST.Declaration exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Debug exposing (toString)
import List

type alias Declaration_ =
    {
        name: String,
        decType: Type,
        depth: Int,
        calledModules: List String,
        calledDecl: List String,
        lambdaCount: Int,
        lambdaLines: Int,
        debugString: String,
        lineCount: Int,
        caseCount: Int,
        indegree: Int,
        outdegree: Int
    }

init: String -> Type -> Int -> List String -> Int -> Int -> Declaration_
init name decType d cm ucmc lc =
    (Declaration_ name decType d cm [] lc 0 "" 0 0 0 0)

default: Declaration_
default =
    (Declaration_ "" Default 0 [] [] 0 0 "" 0 0 0 0)

viewDeclarations: Declaration_ -> Html msg
viewDeclarations decl =
    div[ class "div-special" ][
        div[ class "header" ][
            h3[][ text decl.name ],
            div[ class "subtext"][
                text (decl.decType |> toString)
            ]
        ],
        h4[][ text "Basic metrics"],
        table[ style "width" "60%", style "text-align" "left" ][
            tr[][
                th[][text "LOC"],
                th[][text "Lambda Count"],
                th[][text "Lambda Lines"],
                th[][text "Case branches"],
                th[][text "Indegree"],
                th[][text "Outdegree"]
            ],
            tbody[][
                tr[][
                    td[][ text (decl.lineCount |> toString) ],
                    td[][ text (decl.lambdaCount |> toString) ],
                    td[][ text (decl.lambdaLines |> toString) ],
                    td[][ text (decl.caseCount |> toString) ],
                    td[][ text ((List.length decl.calledModules) |> toString) ],
                    td[][ text ("?") ]
                ]
            ]
        ],
        div[][
            h4[][ text "Called modules" ],
            case List.length decl.calledModules of
                0 ->
                    text "None"
                _ ->
                    List.map(\val -> div[][ text val ] ) decl.calledModules |> div[ class "card", style "min-height" "100px" ]
        ],
        div[][
            h4[][ text "Operands (not primitives)" ],
            case List.length decl.calledDecl of
                0 ->
                    text "None"
                _ ->
                    List.map(\val -> text (val ++ ", ")) decl.calledDecl |> div[ class "card", style "min-height" "100px" ]
        ]
    ]

type Type
    = Function
    | Alias
    | Type
    | Port
    | Default