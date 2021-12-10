module Dashboard.Analysis.Modules exposing (..)

import Browser
import Browser.Events
import Color
import Force exposing (State)
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Html exposing (div, article, Html, section, h1)
import Html.Attributes exposing (class)
import Html.Events exposing (on)
import Json.Decode as Decode
import Time
import TypedSvg exposing (circle, g, line, svg, title, marker, polygon, defs, text_)
import TypedSvg.Attributes exposing (class, fill, stroke, viewBox, markerEnd, id, orient, markerWidth, markerHeight, refX, refY, points, dx, dy, fontSize)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, strokeWidth, x1, x2, y1, y2)
import TypedSvg.Core exposing (Attribute, Svg, text)
import TypedSvg.Types exposing (AlignmentBaseline(..), AnchorAlignment(..), Cursor(..), Length(..), Opacity(..), Paint(..), Transform(..))
import Dashboard.Components.FileSelector exposing (MyFile)
import Analyser.ASTHelper as ASTHelper exposing (..)
import Elm.RawFile exposing (..)
import List.Extra exposing (zip, andThen, indexedFoldl)

type Msg
    = NoOp


type alias Model =
    { 
        graph : Graph Entity(),
        files: List MyFile
    }

type alias Entity =
    Force.Entity NodeId { value : String }


initializeNode : NodeContext String () -> NodeContext Entity ()
initializeNode ctx =
    { node = { label = Force.entity ctx.node.id ctx.node.label, id = ctx.node.id }
    , incoming = ctx.incoming
    , outgoing = ctx.outgoing
    }


init : List MyFile -> ( Model, Cmd Msg )
init files =
    let
        graph =
            Graph.mapContexts initializeNode (buildGraph files)
            
    in
    ( Model graph files, Cmd.none )

update : Msg -> Model -> Model
update msg ({ graph } as model) =
    case msg of
        NoOp ->
            model


linkElement : Graph Entity () -> Edge () -> Svg msg
linkElement graph edge =
    let
        source =
            Maybe.withDefault (Force.entity 0 "") <| Maybe.map (.node >> .label) <| Graph.get edge.from graph

        target =
            Maybe.withDefault (Force.entity 0 "") <| Maybe.map (.node >> .label) <| Graph.get edge.to graph
    in
    line
        [ strokeWidth 0.25
        , stroke <| Paint <| Color.rgb255 170 170 170
        , x1 source.x
        , y1 source.y
        , x2 target.x
        , y2 target.y
        , markerEnd "url(#arrowhead)"
        ]
        []

edgeColor : Paint
edgeColor =
    Paint <| Color.rgb255 180 180 180

arrowhead : Svg msg
arrowhead =
    marker
        [ id "arrowhead"
        , orient "auto"
        , markerWidth <| Px 8.0
        , markerHeight <| Px 6.0
        , refX "29"
        , refY "3"
        ]
        [ polygon
            [ points [ ( 0, 0 ), ( 8, 3 ), ( 0, 6 ) ]
            , fill edgeColor
            ]
            []
        ]


nodeElement : Node Entity -> Svg Msg
nodeElement node =
    g [ TypedSvg.Attributes.class [ "node" ] ]
        [ circle
            [ r 5
            , strokeWidth 1
            , fill (Paint Color.blue)
            , stroke (Paint Color.black)
            --, cursor CursorPointer
            , cx node.label.x
            , cy node.label.y

            ]
            [ title [] [ text node.label.value ] ]
        , text_
            [ dx <| Px node.label.x
            , dy <| Px node.label.y
            , TypedSvg.Attributes.alignmentBaseline AlignmentMiddle
            , TypedSvg.Attributes.textAnchor AnchorMiddle

            , fontSize <| Px 1.25
            , fill (Paint Color.black)

            --, pointerEvents "none"
            ]
            [ text node.label.value ]
        ]


view: Model -> Html Msg
view model =
    section[ Html.Attributes.class "grid" ][
        h1[][ text "Module diagram"],
        div[ Html.Attributes.class "subtext" ][ text "Visualization of module relationships."],
        article[][
            case List.length model.files of
                0 ->
                    text "No Elm project loaded"
                _ ->
                    viewGraph model
        ]
    ]

    

viewGraph : Model -> Svg Msg
viewGraph model =
    svg [ viewBox -80 -60 150 150 ]
        [ defs [] [ arrowhead ],
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
                    ASTHelper.getImports file
                ) files
    in
        indexedFoldl
            (\index file res -> 
                indexedFoldl
                    (\index2 importList acc ->
                        List.foldl
                            (\arg acc2 ->
                                if arg == ASTHelper.joinPath file then
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