module Dashboard.Analysis.ModuleDiagram exposing (..)

import Browser
import Browser.Events
import Color
import Html.Attributes as Attributes exposing (class)
import Force exposing (State)
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId, Adjacency, get)
import Html exposing (div, article, Html, section, h1)
import Html.Events exposing (on)
import Json.Decode as Decode
import Time
import IntDict exposing (IntDict)
import TypedSvg exposing (circle, g, line, svg, title, marker, polygon, defs, text_, animateTransform)
import TypedSvg.Attributes exposing (class, fill, stroke, viewBox, markerEnd, id, orient, markerWidth, markerHeight, refX, refY, points, dx, dy, fontSize, cursor, style, opacity, animateTransformType, animationValues)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, strokeWidth, x1, x2, y1, y2)
import TypedSvg.Core exposing (Attribute, Svg, text, attribute)
import TypedSvg.Types exposing (AlignmentBaseline(..), AnchorAlignment(..), Cursor(..), Length(..), Opacity(..), Paint(..), Transform(..), AnimateTransformType(..))
import TypedSvg.Events exposing (onMouseOver, onClick, onMouseLeave, onMouseDown, onMouseUp, onMouseEnter)
import Dashboard.Components.FileSelector exposing (MyFile)
import Analyser.AST.Helper as Helper exposing (..)
import Elm.RawFile exposing (..)
import List.Extra exposing (zip, andThen, indexedFoldl)
import Material.Icons exposing (ten_k)
import Html.Lazy exposing (lazy, lazy2)
import Html.Events.Extra.Mouse exposing (eventDecoder, Event)

type alias Model =
    { 
        graph : Graph Node_(),
        files: List MyFile
    }

type alias Node_ =
    {
        id: NodeId,
        x: Float,
        y: Float,
        value: String,
        hovered: Bool,
        dragged: Bool
    }

type Msg
    = NoOp
    | MouseMove Event 
    | MouseHover NodeId
    | MouseLeave NodeId
    | MouseDown NodeId
    | MouseUp NodeId

defaultNode_: Node_
defaultNode_ =
    { id = 0, x = 0.0, y = 0.0, value = "", hovered = False, dragged = False}

-- type alias Entity =
--     Force.Entity NodeId { value : String }

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

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)
        MouseHover id ->
            case get id model.graph of 
                Nothing ->
                    (model, Cmd.none)
                Just ctx ->
                    let
                        node = ctx.node.label
                        hoveredNode = {node | hovered = True}
                    in
                        ({ model | graph = Graph.update ctx.node.id (Maybe.map(\val -> hoverNode ctx hoveredNode)) model.graph }, Cmd.none)
        MouseLeave id ->
            case get id model.graph of 
                Nothing ->
                    (model, Cmd.none)
                Just ctx ->
                    let
                        node = ctx.node.label
                        hoveredNode = {node | hovered = False, dragged = False}
                    in
                        ({ model | graph = Graph.update ctx.node.id (Maybe.map(\val -> hoverNode ctx hoveredNode)) model.graph }, Cmd.none)
        MouseUp id ->
            case get id model.graph of 
                Nothing ->
                    (model, Cmd.none)
                Just ctx ->
                    let
                        node = ctx.node.label
                        hoveredNode = {node | dragged = False}
                    in
                        ({ model | graph = Graph.update ctx.node.id (Maybe.map(\val -> hoverNode ctx hoveredNode)) model.graph }, Cmd.none)
        MouseDown id ->
            case get id model.graph of 
                Nothing ->
                    (model, Cmd.none)
                Just ctx ->
                    let
                        node = ctx.node.label
                        hoveredNode = {node | dragged = True}
                    in
                        ({ model | graph = Graph.update ctx.node.id (Maybe.map(\val -> hoverNode ctx hoveredNode)) model.graph }, Cmd.none)
        MouseMove data ->
            (model, Cmd.none)
        
hoverNode: NodeContext Node_ () -> Node_ -> NodeContext Node_ ()
hoverNode ctx val =
    let
        node = ctx.node
    in
        {ctx | node = { node | label = val } }

initializeNode : NodeContext String () -> NodeContext Node_ ()
initializeNode ctx =
    let
        -- this calculates the node position so it doesn't intercept any other nodes (property x and y)
        ref = Force.entity ctx.node.id ctx.node.label
    in
    { 
        node = { id = ctx.node.id, label = { id  = ctx.node.id, x = ref.x, y = ref.y, value = ctx.node.label, hovered = False, dragged = False } },
        incoming = ctx.incoming,
        outgoing = ctx.outgoing
    }

linkElement : Graph Node_ () -> Edge () -> Svg msg
linkElement graph edge =
    let
        source =
            Maybe.withDefault (defaultNode_) <| Maybe.map (.node >> .label) <| Graph.get edge.from graph

        target =
            Maybe.withDefault (defaultNode_) <| Maybe.map (.node >> .label) <| Graph.get edge.to graph
    in
        line [ 
            strokeWidth 0.15,
            x1 source.x,
            y1 source.y,
            x2 target.x,
            y2 target.y,
            markerEnd "url(#arrow)",
            if source.hovered == True then
                stroke <| Paint <| Color.red
            else if target.hovered == True then
                stroke <| Paint <| Color.green
                -- attribute "z-index" "5000"
            else
                stroke <| Paint <| Color.grey
                -- attribute "z-index" "5000"
        ][]

arrow : Svg msg
arrow =
    marker [ 
        id "arrow",
        orient "auto",
        markerWidth <| Px 8.0,
        markerHeight <| Px 6.0,
        refX "45",
        refY "3"
    ][ polygon [
        points [ ( 0, 0 ), ( 8, 3 ), ( 0, 6 ) ],
        fill (Paint <| Color.grey)
    ][]
    ]




nodeElement : Node Node_ -> Svg Msg
nodeElement node =
    g [ TypedSvg.Attributes.class [ "node" ], onMouseEnter (MouseHover node.id), onMouseLeave (MouseLeave node.id), cursor CursorPointer, TypedSvg.Attributes.class ["graph-node"],
        onMouseDown (MouseDown node.id), onMouseUp (MouseUp node.id)
    ][ 
            circle[ 
                r 5.5,
                strokeWidth 0.25,
                fill (Paint (Color.rgb255 135 206 250)),
                stroke (Paint Color.darkBlue),
                cx node.label.x,
                cy node.label.y,
                if node.label.hovered then
                    -- fill (Paint (Color.rgb255 66 102 175))
                    --r 6
                    attribute "opacity" "1.0"
                else
                    attribute "opacity" "1.0"
                , if node.label.dragged then
                    fill (Paint (Color.rgb255 66 102 175))
                else
                    attribute "opacity" "1.0"
            ]
            [ title[][ text node.label.value ]],
        text_[ 
            dx <| Px node.label.x,
            dy <| Px node.label.y,
            TypedSvg.Attributes.alignmentBaseline AlignmentMiddle,
            TypedSvg.Attributes.textAnchor AnchorMiddle,
            fontSize <| Px 1.25,
            attribute "unselectable" "on",
            fill (Paint Color.black),
            TypedSvg.Attributes.class [ "noselect" ]
            ]
            [ text ( trim node.label.value ) ]
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
                        div[ Attributes.class "explanation" ][
                            text "This graph visualizes the relationships between project modules."
                        ],
                        div[ Attributes.class "main-card"][
                            div[ Attributes.class "card" ][
                                lazy viewGraph model
                            ]
                        ]
                    ]
        ]
    ]

trim: String -> String
trim name =
    if String.length name > 12 then
        let
            newName = String.dropRight 4 name
        in
            if String.length newName > 12 then
                String.append  (String.slice 0 12 name) ".."
            else
                String.append newName "..."
    else
        name

viewGraph : Model -> Svg Msg
viewGraph model =
    svg [ viewBox -80 -60 150 150 ][ 
        defs [] [ arrow ],
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