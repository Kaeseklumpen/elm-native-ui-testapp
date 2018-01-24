module Main exposing (..)

import List exposing (..)
import NativeUi as Ui exposing (Node)
import NativeUi.Elements as Elements exposing (..)
import NativeUi.Events exposing (..)
import NativeUi.Image as Image exposing (..)
import NativeUi.ListView as ListView exposing (..)
import NativeUi.Style as Style exposing (defaultTransform)
import String exposing (..)
import Time exposing (..)


dummyList : List Project
dummyList =
    [ { id = 1, name = "elm", status = False, tasks = [ { id = 1, name = "task1", time = 5.5 } ] }
    , { id = 2, name = "elmo", status = False, tasks = [ { id = 2, name = "task2", time = 6.5 } ] }
    ]


type alias Task =
    { id : Int
    , name : String
    , time : Time
    }



-- Project Type


type alias Project =
    { id : Int
    , name : String
    , status : Bool
    , tasks : List Task
    }



-- MODEL


type alias Model =
    { projects : List Project, tmpProjectName : Maybe String, tmpTaskName : Maybe String }


initModel : ( Model, Cmd Msg )
initModel =
    ( { projects = dummyList, tmpProjectName = Nothing, tmpTaskName = Nothing }, Cmd.none )


projectOutput : List Project -> List (Node Msg)
projectOutput l =
    case l of
        [] ->
            []

        x :: xs ->
            text
                [ Ui.style
                    [ Style.textAlign "center"
                    , Style.color "#00ccff"
                    , Style.marginBottom 20
                    , Style.width 100
                    , Style.borderStyle "solid"
                    , Style.borderBottomColor "#003340"
                    , Style.borderBottomWidth 1
                    ]
                , onPress (ClickProject x.id)
                ]
                [ Ui.string x.name
                ]
                :: projectOutput xs


tasksOutput : List Task -> List (Node Msg)
tasksOutput l =
    case l of
        [] ->
            []

        x :: xs ->
            text
                [ Ui.style
                    [ Style.textAlign "center"
                    , Style.color "#00ccff"
                    , Style.marginBottom 20
                    , Style.width 100
                    , Style.borderStyle "solid"
                    , Style.borderBottomColor "#003340"
                    , Style.borderBottomWidth 1
                    ]

                -- , onPress (ClickProject x.id)
                ]
                [ Ui.string x.name
                ]
                :: tasksOutput xs


newProject : Int -> String -> Project
newProject i name =
    { id = i + 1, name = name, tasks = [], status = False }


newTask : Int -> String -> Task
newTask i name =
    { id = i + 1, name = name, time = 0.0 }



-- UPDATE


type Msg
    = AddProject
    | AddTask Int
    | InputProjectName String
    | InputTaskName String
    | ClickProject Int
    | BackToProjects


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddProject ->
            case model.tmpProjectName of
                Nothing ->
                    ( model, Cmd.none )

                Just x ->
                    ( { model | projects = List.append model.projects [ newProject (List.length model.projects) x ] }, Cmd.none )

        AddTask projectId ->
            case model.tmpTaskName of
                Nothing ->
                    ( model, Cmd.none )

                Just x ->
                    ( { model
                        | projects =
                            List.map
                                (\project ->
                                    if project.id == projectId then
                                        { id = project.id, name = project.name, status = project.status, tasks = List.append project.tasks [ newTask (List.length project.tasks) x ] }
                                    else
                                        project
                                )
                                model.projects
                      }
                    , Cmd.none
                    )

        InputProjectName projectName ->
            if String.isEmpty projectName then
                ( { model | tmpProjectName = Nothing }, Cmd.none )
            else
                ( { model | tmpProjectName = Just projectName }, Cmd.none )

        InputTaskName taskName ->
            if String.isEmpty taskName then
                ( { model | tmpTaskName = Nothing }, Cmd.none )
            else
                ( { model | tmpTaskName = Just taskName }, Cmd.none )

        ClickProject projectId ->
            ( { model
                | projects =
                    List.map
                        (\project ->
                            { id = project.id
                            , name = project.name
                            , status = project.id == projectId
                            , tasks = project.tasks
                            }
                        )
                        model.projects
              }
            , Cmd.none
            )

        BackToProjects ->
            ( { model
                | projects =
                    List.map
                        (\project ->
                            { id = project.id
                            , name = project.name
                            , status = False
                            , tasks = project.tasks
                            }
                        )
                        model.projects
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Node Msg
view model =
    let
        filteredProject =
            List.filter (\project -> project.status) model.projects

        singleProject =
            List.head filteredProject
    in
    -- if no project is clicked --> show projectOverview
    if List.isEmpty filteredProject then
        -- View Projects
        Elements.view
            [ Ui.style
                [ Style.alignItems "center"
                , Style.backgroundColor "#ffffff"
                , Style.height 960.0 --Motorola E - DisplayHeight
                ]
            ]
            -- Lvl2 --> Header
            [ Elements.view
                [ Ui.style
                    [ Style.width 540 --Motorola E - DisplayWidth
                    , Style.height 80
                    , Style.backgroundColor "#003340"
                    ]
                ]
                [ text
                    [ Ui.style
                        [ Style.textAlign "center"
                        , Style.color "white"
                        , Style.marginTop 15
                        , Style.fontSize 35
                        ]
                    ]
                    [ Ui.string "Arbeitszeit" ]
                ]

            -- Project Title
            , Elements.view
                [ Ui.style
                    []
                ]
                [ textInput
                    [ Ui.style
                        [ Style.textAlign "center"
                        , Style.color "#003340"
                        , Style.width 100
                        ]
                    ]
                    [ Ui.string "Projecte"
                    ]
                ]

            -- Projects
            , Elements.view
                [ Ui.style
                    [ Style.flexDirection "row"
                    , Style.justifyContent "center"
                    , Style.height 330
                    , Style.overflow "visible"
                    ]
                ]
                [ --Projects
                  Elements.view
                    [ Ui.style
                        [ Style.marginTop 25
                        ]
                    ]
                    (projectOutput model.projects)
                ]

            -- Add projects --> Inputfield
            , Elements.view
                [ Ui.style
                    [ Style.flexDirection "row"
                    , Style.justifyContent "center"
                    ]
                ]
                [ Elements.view
                    [ Ui.style
                        [ Style.flexDirection "row"
                        , Style.justifyContent "center"
                        ]
                    ]
                    [ textInput
                        [ Ui.style
                            [ Style.textAlign "center"
                            , Style.marginBottom 10
                            , Style.width 100
                            ]
                        , onChangeText InputProjectName
                        ]
                        [ Ui.string "Add Project" ]
                    ]
                , Elements.view
                    [ Ui.style
                        [ Style.width 80
                        , Style.justifyContent "space-between"
                        ]
                    ]
                    [ if model.tmpProjectName == Nothing then
                        disabledButton "#f00" "+"
                      else
                        button AddProject "#00ccff" "+"
                    ]
                ]

            -- Lvl 2 --> Bottom
            , Elements.view
                [ Ui.style
                    [ Style.width 540 --Motorola E - DisplayWidth
                    , Style.height 60
                    , Style.bottom -20
                    , Style.backgroundColor "#003340"
                    ]
                ]
                []
            ]
        -- view of a project
    else
        -- View Tasks in Projects
        Elements.view
            [ Ui.style
                [ Style.alignItems "center"
                , Style.backgroundColor "#ffffff"
                , Style.height 960.0 --Motorola E - DisplayHeight
                ]
            ]
            -- Lvl2 --> Header
            [ Elements.view
                [ Ui.style
                    [ Style.width 540 --Motorola E - DisplayWidth
                    , Style.height 80
                    , Style.backgroundColor "#003340"
                    , Style.flexDirection "row"
                    , Style.justifyContent "center"
                    ]
                ]
                [ text
                    [ Ui.style
                        [ Style.color "white"
                        , Style.textAlign "left"
                        , Style.backgroundColor "white"
                        , Style.paddingTop 5
                        , Style.paddingBottom 5
                        , Style.width 30
                        , Style.height 30
                        , Style.marginTop 25
                        , Style.marginRight 30
                        , Style.fontWeight "bold"
                        , Style.shadowColor "#000"
                        , Style.color "#003340"
                        , Style.shadowOpacity 0.25
                        , Style.shadowOffset 1 1
                        , Style.shadowRadius 5
                        , Style.borderStyle "solid"
                        , Style.borderRadius 15.0
                        , Style.borderColor "white"
                        ]
                    , onPress BackToProjects
                    ]
                    [ Ui.string " <--" ]
                , text
                    [ Ui.style
                        [ Style.textAlign "center"
                        , Style.color "white"
                        , Style.marginTop 15
                        , Style.fontSize 35
                        ]
                    ]
                    [ Ui.string
                        (case singleProject of
                            Nothing ->
                                ""

                            Just x ->
                                x.name
                        )
                    ]
                ]

            -- Tasks Title
            , Elements.view
                [ Ui.style
                    []
                ]
                [ textInput
                    [ Ui.style
                        [ Style.textAlign "center"
                        , Style.color "#003340"
                        , Style.width 100
                        ]
                    ]
                    [ Ui.string "Aufgaben"
                    ]
                ]

            -- Tasks
            , Elements.view
                [ Ui.style
                    [ Style.flexDirection "row"
                    , Style.justifyContent "center"
                    , Style.height 330
                    , Style.overflow "visible"
                    ]
                ]
                [ Elements.view
                    [ Ui.style
                        [ Style.marginTop 25
                        ]
                    ]
                    (tasksOutput
                        (case singleProject of
                            Nothing ->
                                []

                            Just x ->
                                x.tasks
                        )
                    )
                ]

            -- Add tasks --> Inputfield
            , Elements.view
                [ Ui.style
                    [ Style.flexDirection "row"
                    , Style.justifyContent "center"
                    ]
                ]
                [ Elements.view
                    [ Ui.style
                        [ Style.flexDirection "row"
                        , Style.justifyContent "center"
                        ]
                    ]
                    [ textInput
                        [ Ui.style
                            [ Style.textAlign "center"
                            , Style.marginBottom 10
                            , Style.width 100
                            ]
                        , onChangeText InputTaskName
                        ]
                        [ Ui.string "Add Task" ]
                    ]
                , Elements.view
                    [ Ui.style
                        [ Style.width 80
                        , Style.justifyContent "space-between"
                        ]
                    ]
                    [ if model.tmpTaskName == Nothing then
                        disabledButton "#f00" "+"
                      else
                        button
                            (AddTask
                                (case singleProject of
                                    Nothing ->
                                        0

                                    Just x ->
                                        x.id
                                )
                            )
                            "#00ccff"
                            "+"
                    ]
                ]

            -- Lvl 2 --> Bottom
            , Elements.view
                [ Ui.style
                    [ Style.width 540 --Motorola E - DisplayWidth
                    , Style.height 60
                    , Style.bottom -20
                    , Style.backgroundColor "#003340"
                    ]
                ]
                []
            ]


button : Msg -> String -> String -> Node Msg
button msg color content =
    text
        [ Ui.style
            [ Style.color "white"
            , Style.textAlign "center"
            , Style.backgroundColor color
            , Style.paddingTop 5
            , Style.paddingBottom 5
            , Style.width 30
            , Style.fontWeight "bold"
            , Style.shadowColor "#000"
            , Style.shadowOpacity 0.25
            , Style.shadowOffset 1 1
            , Style.shadowRadius 5
            , Style.borderStyle "solid"
            , Style.borderRadius 15.0
            , Style.borderColor color
            ]
        , onPress msg
        ]
        [ Ui.string content ]


disabledButton : String -> String -> Node Msg
disabledButton color content =
    text
        [ Ui.style
            [ Style.color "white"
            , Style.textAlign "center"
            , Style.backgroundColor color
            , Style.paddingTop 5
            , Style.paddingBottom 5
            , Style.width 30
            , Style.fontWeight "bold"
            , Style.shadowColor "#000"
            , Style.shadowOpacity 0.25
            , Style.shadowOffset 1 1
            , Style.shadowRadius 5
            , Style.borderStyle "solid"
            , Style.borderRadius 15.0
            , Style.borderColor color
            ]
        ]
        [ Ui.string content ]



-- PROGRAM


main : Program Never Model Msg
main =
    Ui.program
        { init = initModel
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
