module Dashboard.Analysis.ModuleDiagram exposing (..)

import Browser
import Browser.Events
import Color
import Html.Attributes as Attributes exposing (class)
import Force exposing (State)
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Html exposing (div, article, Html, section, h1)
import Html.Events exposing (on)
import Json.Decode as Decode
import Time
import TypedSvg exposing (circle, g, line, svg, title, marker, polygon, defs, text_)
import TypedSvg.Attributes exposing (class, fill, stroke, viewBox, markerEnd, id, orient, markerWidth, markerHeight, refX, refY, points, dx, dy, fontSize)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, strokeWidth, x1, x2, y1, y2)
import TypedSvg.Core exposing (Attribute, Svg, text)
import TypedSvg.Types exposing (AlignmentBaseline(..), AnchorAlignment(..), Cursor(..), Length(..), Opacity(..), Paint(..), Transform(..))
import TypedSvg.Events exposing (onMouseOver, onClick)
import Dashboard.Components.FileSelector exposing (MyFile)
import Analyser.AST.Helper as Helper exposing (..)
import Elm.RawFile exposing (..)
import List.Extra exposing (zip, andThen, indexedFoldl)

type Msg
    = NoOp
    | MouseHover NodeId


type alias Model =
    { 
        graph : Graph Entity(),
        files: List MyFile
    }

type alias Entity =
    Force.Entity NodeId { value : String }

init : List MyFile -> ( Model, Cmd Msg )
init files =
    let
        graph =
            Graph.mapContexts initializeNode (buildGraph files)
            
    in
    ( { 
        graph = graph,
        files = List.filter (
            \file -> case file.ast of
                Just _ ->
                    True
                Nothing ->
                    False
        ) files
    }, Cmd.none )

update : Msg -> Model -> Model
update msg ({ graph } as model) =
    case msg of
        NoOp ->
            model
        MouseHover id ->
            model


initializeNode : NodeContext String () -> NodeContext Entity ()
initializeNode ctx =
    { node = { label = Force.entity ctx.node.id ctx.node.label, id = ctx.node.id }
    , incoming = ctx.incoming
    , outgoing = ctx.outgoing
    }

linkElement : Graph Entity () -> Edge () -> Svg msg
linkElement graph edge =
    let
        source =
            Maybe.withDefault (Force.entity 0 "") <| Maybe.map (.node >> .label) <| Graph.get edge.from graph

        target =
            Maybe.withDefault (Force.entity 0 "") <| Maybe.map (.node >> .label) <| Graph.get edge.to graph
    in
        line [ 
            strokeWidth 0.25,
            stroke <| Paint <| Color.grey,
            x1 source.x,
            y1 source.y,
            x2 target.x,
            y2 target.y,
            markerEnd "url(#arrowhead)"
        ][]

edgeColor : Paint
edgeColor =
    Paint <| Color.grey

arrowhead : Svg msg
arrowhead =
    marker [ 
        id "arrowhead",
        orient "auto",
        markerWidth <| Px 8.0,
        markerHeight <| Px 6.0,
        refX "29",
        refY "3"
    ][ polygon [
        points [ ( 0, 0 ), ( 8, 3 ), ( 0, 6 ) ],
        fill edgeColor
    ][]
    ]


nodeElement : Node Entity -> Svg Msg
nodeElement node =
    g [ TypedSvg.Attributes.class [ "node" ] ][ 
        circle[ 
            r 5.5,
            strokeWidth 0.25,
            fill (Paint Color.lightBlue),
            stroke (Paint Color.darkBlue),
            cx node.label.x,
            cy node.label.y,
            onMouseOver (MouseHover node.id)
        ][ 
            title[][ 
                text node.label.value 
            ]
        ],
        text_[ 
            dx <| Px node.label.x,
            dy <| Px node.label.y,
            TypedSvg.Attributes.alignmentBaseline AlignmentMiddle,
            TypedSvg.Attributes.textAnchor AnchorMiddle,
            fontSize <| Px 1.25,
            fill (Paint Color.black)
            ][ 
                text node.label.value 
            ]
        ]


view: Model -> Html Msg
view model =
    div[][
        div[ Attributes.class "header" ][
            h1[][ text "Module diagram"],
            div[ Attributes.class "subtext" ][ text "Visualization of relationships between modules."]
        ],
        article[][
            case List.length model.files of
                0 ->
                    article[ Attributes.class "main-header"][
                        text "No Elm project currently loaded."
                    ]
                _ -> 
                    div[][
                        div[ Attributes.class "main-header"][
                            text "Header"
                        ],
                        div[ Attributes.class "main-card"][
                            div[ Attributes.class "card" ][
                                viewGraph model
                            ]
                        ]
                    ]
        ]
    ]

    

viewGraph : Model -> Svg Msg
viewGraph model =
    svg [ viewBox -80 -60 150 150 ][ 
        defs [] [ arrowhead ],
        Graph.edges model.graph
            |> List.map (linkElement model.graph)
            |> g [ TypedSvg.Attributes.class [ "links" ] ]
        , Graph.nodes model.graph
            |> List.map nodeElement
            |> g [ TypedSvg.Attributes.class [ "nodes" ] ]
    ]



getGraphEdges: List RawFile -> List (Int, Int)
getGraphEdges files =
    let
        imports = 
            List.indexedMap 
                (\index file -> 
                    Helper.getImports file
                ) files
    in
        indexedFoldl
            (\index file res -> 
                indexedFoldl
                    (\index2 importList acc ->
                        List.foldl
                            (\arg acc2 ->
                                if arg == Helper.joinPath file then
                                    (index, index2) :: acc2
                                else
                                    acc2
                            ) acc importList
                    ) res imports
            ) [] files

buildGraph : List MyFile -> Graph String ()
buildGraph files =
    Graph.fromNodeLabelsAndEdgePairs 
    (getGraphNodes (List.filter hasAst files))
    --(List.map(\leaf -> leaf.content ++ "lel") files)
    --[]
    (getGraphEdges 
        (List.map 
            (\leaf ->
                case leaf.ast of
                    Just ast ->
                        ast
                    Nothing ->
                        Debug.todo "This error is impossible to get"
            )
        ( List.filter hasAst files )
    ))

getGraphNodes: List MyFile -> List String
getGraphNodes files =
    List.map(\file -> file.name) files

hasAst: MyFile -> Bool
hasAst file =
    case file.ast of
        Just _ ->
            True
        _ ->
            False

getModel: (Model, Cmd Msg) -> Model
getModel (model, cmd) =
  model