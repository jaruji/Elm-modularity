module Analyser.AST.Declaration exposing (..)
import Html exposing (..)
import Debug exposing (toString)

type alias Declaration_ =
    {
        name: String,
        decType: Type,
        depth: Int,
        calledModules: List String,
        uniqueCalledModulesCount: Int,
        lambdaCount: Int,
        lambdaLines: Int,
        debugString: String,
        lineCount: Int,
        caseCount: Int
    }

init: String -> Type -> Int -> List String -> Int -> Int -> Declaration_
init name decType d cm ucmc lc =
    (Declaration_ name decType d cm ucmc lc 0 "" 0 0)

default: Declaration_
default =
    (Declaration_ "" Default 0 [] 0 0 0 "" 0 0)

viewDeclarations: Declaration_ -> Html msg
viewDeclarations decl =
    div[][
        h2[][ text decl.name ],
        div[][
            text ("Type: " ++ (decl.decType |> toString))
        ],
        div[][
            text ("Depth: " ++ (decl.depth |> toString))
        ],
        div[][
            text ("Called modules: " ++ (decl.calledModules |> toString))
        ],
        div[][
            text ("Unique modules count: " ++ (decl.uniqueCalledModulesCount |> toString))
        ],
        div[][
            text ("Lambda Count: " ++ (decl.lambdaCount |> toString))
        ],
        div[][
            text ("Lambda lines: " ++ (decl.lambdaLines |> toString))
        ],
        div[][
            text ("LOC: " ++ (decl.lineCount |> toString))
        ],
        div[][
            text ("Case count: " ++ (decl.caseCount |> toString))
        ],
        div[][
            text ("Debug: " ++ (decl.debugString |> toString))
        ]
    ]

type Type
    = Function
    | Alias
    | Type
    | Port
    | Default