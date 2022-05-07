module Analyser.AST.Declaration exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Debug exposing (toString)

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
    div[][
        h3[][ text decl.name ],
        div[][
            text ("Type: " ++ (decl.decType |> toString))
        ],
         div[][
            text ("Called modules: " ++ (decl.calledModules |> toString))
        ],
        div[][
            text ("Operands (not primitives): " ++ (decl.calledDecl |> toString))
        ],
        div[][
            text ("LOC: " ++ (decl.lineCount |> toString))
        ],
        div[][
            text ("Lambda count: " ++ (decl.lambdaCount |> toString))
        ],
        div[][
            text ("Lambda lines: " ++ (decl.lambdaLines |> toString))
        ],
        div[][
            text ("Case branch count: " ++ (decl.caseCount |> toString))
        ],
        div[][
            text "Indegree: "
        ],
        div[][
            text "Outdegree: "
        ]
    ]

type Type
    = Function
    | Alias
    | Type
    | Port
    | Default