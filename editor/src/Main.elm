module Main exposing (main)

import Basics as B
import List as L exposing (..)
import Html exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (..)
import Browser
import Html.Events.Extra exposing (onEnter)
import String as S
import Browser.Dom as D
import Http as H

import Lib exposing (..)
import Task

main = Browser.element {init=init,subscriptions=subs,update=update,view=view}

subs : Model -> Sub Msg
subs _ = Sub.none


type alias Model = { text : String
                   , fileName : String
                   , lifeTime : Int
                   , isPublic : Bool
                   }

init : () -> (Model, Cmd Msg)
init _ = ({text="", fileName="", lifeTime=0, isPublic=False}, Cmd.none)

type Msg = NOP
         | TextEntered String
         | ChangeFileName String
         | ChangeLifeTime String
         | ChangePublic Bool
         | Upload

update : Msg -> Model -> (Model, Cmd Msg)
update m model = case m of
    NOP -> (model, Cmd.none)
    TextEntered s -> ({model|
        text=s
        }, Cmd.none)
    ChangeFileName s -> ({model|fileName=s}, Cmd.none)
    ChangeLifeTime s -> case String.toInt s of
        Just t -> ({model|lifeTime=t}, Cmd.none)
        Nothing -> (model, Cmd.none)
    ChangePublic p -> ({model|isPublic=p
                            , lifeTime=if p then model.lifeTime else B.min model.lifeTime maxPrivateLT}, Cmd.none)
    Upload -> (model, Cmd.batch [
            H.post {
                url="/upload",
                expect=H.expectWhatever (const NOP),
                body=H.fileBody
            }
        ])

--TODO: OnArrowUp/ArrowDown
view : Model -> Html Msg
view model = div [class "container"] [
        submitModal model,
        div [class "form-group"] [
            textarea [class "form-control", rows 20,
                onInput TextEntered, value model.text] []
        ],
        button [class "btn btn-primary", dataToggle "modal", dataTarget "#submitModal"] [h3 [] [text "Submit"]]
    ]


submitModal : Model -> Html Msg
submitModal model = div [class "modal fade", id "submitModal", data "backdrop" "static",
                    tabindex -1] [
            div [class "modal-dialog"] [
                div [class "modal-content"] [
                    div [class "modal-header"] [
                        h5 [class "modal-title", id "submitModalLabel"] [text "Submit"],
                        button [class "close", data "dismiss" "modal"] [text "×"]
                    ],
                    div [class "modal-body"] [
                        div [class "form-group"] [
                            label [for "submit-file-name"] [text "File Name"],br_,
                            input [type_ "input", id "submit-file-name form-control"
                                   , onInput ChangeFileName, value model.fileName] [],
                            small [class "form-text text-muted"] [text "Optional"], br_,

                            label [for "life-time"] [text "Life Time"], br_,
                            select [class "form-control", id "life-time", onInput ChangeLifeTime] (
                                L.map (\(t, n, pOnly) ->
                                    option [value (String.fromInt t), disabled (pOnly && not model.isPublic),
                                        selected (model.lifeTime == t)] [text n]) lifeTimes
                            ),
                            small [class "form-text text-muted"] [text "The uploaded file will automatically expire after the specified time."],br_
                        ],
                        div [class "form-group form-check"] [
                            input [class "form-check-input", id "public", type_ "checkbox", onCheck ChangePublic] [],
                            label [class "form-check-label", for "public"] [text "Mark this upload as public"]
                        ]
                    ],
                    div [class "modal-footer"][
                        button [class "btn btn-secondary", data "dismiss" "modal"] [text "Cancel"],
                        button [class "btn btn-primary", data "dismiss" "modal"] [text "Upload"]
                    ]
                ]
            ]
        ]

maxPrivateLT = Maybe.withDefault 0 <| maximum (concatMap (\(x, _, z) -> if z then [x] else []) lifeTimes)

lifeTimes : List (Int, String, Bool)
lifeTimes = [(300, "5min", False),
             (600, "10min", False),
             (900, "15min", False),
             (1200, "20min", False),
             (1800, "30min", False),
             (3600, "1h", False),
             (5400, "30min", False),
             (7200, "1h", False),
             (10800, "3h", False),
             (18000, "5h", False),
             (43200, "12h", False),
             (86400, "24h", False),
             (172800, "2d", True),
             (259200, "3d", True),
             (345600, "4d", True),
             (432000, "5d", True),
             (518400, "6d", True),
             (604800, "7d", True),
             (9223372036854775807, "∞", True)]

