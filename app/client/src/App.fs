module App

open Feliz
open Elmish
open Shared

type LoginPageState = {
  userName : string
}
type CallPageState = {
  userName : string
  userToCall : string
}

type State = 
  | LoginPage of LoginPageState
  | CallPage of CallPageState

type AudioType =
  | Local
  | Remote

type Msg =
  | UpdateUser of string
  | Login
  | Call
  | PlayAudio of AudioType
  | StopAudio

let init() = { userName = "" }, Cmd.none

let update (msg: Msg) (state: State) =
    match msg, state with
    | UpdateUser user, LoginPage lp -> LoginPage { lp with userName = user }, Cmd.none
    | Login, LoginPage lp -> CallPage { userName = lp.userName; userToCall = "" }, Cmd.none
    | UpdateUser user, CallPage cp -> CallPage { cp with userToCall = user }, Cmd.none
    | Call, _ -> state, Cmd.none
    | _ -> state, Cmd.none

let fableLogo() = StaticFile.import "./imgs/fable_logo.png"

let render (state: State) (dispatch: Msg -> unit) =
  Html.div [
    prop.classes [ "min-h-screen"; "bg-gray-100"; "py-6"; "flex"; "flex-col"; "justify-center"; "sm:py-12" ]
    prop.children [
      Html.div [
        prop.classes [ "relative"; "py-3"; "sm:max-w-xl"; "sm:mx-auto" ]
        prop.children [
          Html.div [
            prop.classes [ "absolute"; "inset-0"; "bg-gradient-to-r"; "from-blue-400"; "to-blue-800"; "shadow-lg"; "transform"; "-skew-y-6"; "sm:skew-y-0"; "sm:-rotate-6"; "sm:rounded-3xl" ]
          ]
          Html.div [
            prop.classes [ "relative"; "px-4"; "py-10"; "bg-white"; "shadow-lg"; "sm:rounded-3xl"; "sm:p-20" ]
            prop.children [
              Html.div [
                prop.classes [ "max-w-md"; "mx-auto" ]
                prop.children [
                  Html.div [
                    Html.img [
                      prop.src ( fableLogo() )
                      prop.classes [ "h-7"; "sm:h-8" ]
                    ]
                  ]
                  Html.div [
                    prop.classes [ "divide-y"; "divide-gray-200" ]
                    prop.children [
                      Html.div [
                        prop.classes [ "py-8"; "text-base"; "leading-6"; "space-y-4"; "text-gray-700"; "sm:text-lg"; "sm:leading-7" ]
                        prop.children [
                          Html.a [prop.href "#"; prop.text "SAFE-Stack"]
                          Html.p "+"
                          Html.a [prop.href "#"; prop.content "SAFE"]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ]
