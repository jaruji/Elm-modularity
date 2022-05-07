module Dashboard.Analysis.ModuleDiagram exposing (..)

import Browser
import Browser.Events
import Color
import Html.Attributes as Attributes exposing (class)
import Force exposing (State)
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId, Adjacency, get, stronglyConnectedComponents )
import Html exposing (div, article, Html, section, h1, button)
import Html.Events as Events exposing (on, onClick)
import Json.Decode as Decode
import Time
import IntDict exposing (IntDict)
import TypedSvg exposing (circle, g, line, svg, title, marker, polygon, defs, text_, animateTransform)
import TypedSvg.Attributes exposing (class, fill, stroke, viewBox, markerEnd, id, orient, markerWidth, markerHeight, refX, refY, points, dx, dy, fontSize, cursor, style, opacity, animateTransformType, animationValues)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, strokeWidth, x1, x2, y1, y2)
import TypedSvg.Core exposing (Attribute, Svg, text, attribute)
import TypedSvg.Types exposing (AlignmentBaseline(..), AnchorAlignment(..), Cursor(..), Length(..), Opacity(..), Paint(..), Transform(..), AnimateTransformType(..))
import TypedSvg.Events exposing (onMouseOver, onClick, onMouseLeave, onMouseDown, onMouseUp, onMouseEnter)
import Analyser.File exposing (File_)
import Analyser.AST.Helper as Helper exposing (..)
import Elm.RawFile exposing (..)
import List.Extra exposing (zip, andThen, indexedFoldl)
import Material.Icons exposing (ten_k)
import Html.Lazy exposing (lazy, lazy2)
import Html.Events.Extra.Mouse exposing (eventDecoder, Event)
import Set exposing (foldl)

type alias Model =
    { 
        graph : Graph Node_(),
        files: List File_,
        scc: State
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

type State
    = None
    | Done (Result (List (Graph Node_ ())) (Graph.AcyclicGraph Node_ ()))

type Msg
    = NoOp
    | MouseMove Event 
    | MouseHover NodeId
    | MouseLeave NodeId
    | MouseDown NodeId
    | MouseUp NodeId
    | CalculateSCC (Result (List (Graph Node_ ())) (Graph.AcyclicGraph Node_ ()))

defaultNode_: Node_
defaultNode_ =
    { id = 0, x = 0.0, y = 0.0, value = "", hovered = False, dragged = False}

-- type alias Entity =
--     Force.Entity NodeId { value : String }

init : List File_ -> ( Model, Cmd Msg )
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
        ) files,
        scc = None
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
        CalculateSCC graph ->
            ({ model | scc = Done graph }, Cmd.none)
        
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
                stroke <| Paint <| Color.darkGreen
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
    g [ 
        TypedSvg.Attributes.class [ "node" ],
        onMouseEnter (MouseHover node.id),
        onMouseLeave (MouseLeave node.id),
        cursor CursorPointer,
        TypedSvg.Attributes.class ["graph-node"],
        onMouseDown (MouseDown node.id),
        onMouseUp (MouseUp node.id)
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
            fill (Paint Color.black),
            TypedSvg.Attributes.class [ "noselect" ]
            ][ text ( trim node.label.value ) ]
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

viewGraph : Graph Node_ () -> Svg Msg
viewGraph graph =
    svg [ viewBox -80 -60 150 150 ][ 
        defs [] [ arrow ],
        Graph.edges graph
            |> List.map (linkElement graph)
            |> g [ TypedSvg.Attributes.class [ "links" ] ]
        , Graph.nodes graph
            |> List.map nodeElement
            |> g [ TypedSvg.Attributes.class [ "nodes" ] ]
    ]



getGraphEdges: List File_ -> List (Int, Int)
getGraphEdges files_ =
    let
        files = 
            List.filterMap(\file ->
                case file.ast of
                    Just _ ->
                        Just file
                    Nothing ->
                        Nothing 
            ) files_
    in
        indexedFoldl(\index file acc ->
            case file.ast of
                Just _ ->
                    acc 
                    ++
                    List.foldl(\modName acc1 -> 
                        acc1
                        ++
                        indexedFoldl(\index2 file2 acc2 ->
                            case file2.ast of
                                Just ast ->
                                    if((Helper.getModuleNameFromAst ast) == modName) then
                                        (index2, index) :: acc2
                                    else
                                        acc2
                                _ ->
                                    acc2
                        ) [] files
                    ) [] (Set.toList (file.calledModules))
                Nothing ->
                    acc
        ) [] files
    
getGraphNodes: List File_ -> List String
getGraphNodes files =
        List.filterMap(\file ->
            case file.ast of
                Just ast ->
                    Just (Helper.getModuleNameRaw ast)
                Nothing ->
                    Nothing 
        ) files

buildGraph : List File_ -> Graph String ()
buildGraph files =
    Graph.fromNodeLabelsAndEdgePairs 
    (getGraphNodes (List.filter hasAst files))
    (getGraphEdges files)


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
                                lazy viewGraph model.graph
                            ]
                        ],
                        div[ Attributes.class "main-card"][
                            div[ Attributes.class "card" ][
                                Html.button[ Attributes.class "button-special", Events.onClick (CalculateSCC (stronglyConnectedComponents model.graph)) ][ text "Get SCC" ],
                                case model.scc of
                                    None ->
                                        div[ Attributes.class "main-header" ][ text "Strongly connected components not generated" ]
                                    Done res ->
                                        case res of 
                                            Ok ac ->
                                                div[][
                                                    div[ Attributes.class "main-header" ][ text "Graph is acylic."]
                                                    -- lazy viewGraph ac somehow figure this out
                                                ]
                                            Err components ->
                                                div[][ 
                                                    div[ Attributes.class "main-header" ][ text "Graph has multiple components."],
                                                    (List.map (\component -> lazy viewGraph component) components) |> div[]
                                                ]
                                                
                            ]
                        ]
                    ]
        ]
    ]

hasAst: File_ -> Bool
hasAst file =
    case file.ast of
        Just _ ->
            True
        _ ->
            False

getModel: (Model, Cmd Msg) -> Model
getModel (model, cmd) =
  model