module App

open Feliz
open Elmish
open Shared

type LoginPageState = {
  userName : string
  error: bool
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

let init() = LoginPage { userName = "" ; error = false }, Cmd.none

let update (msg: Msg) (state: State) =
    match msg, state with
    | UpdateUser user, LoginPage lp -> LoginPage { lp with userName = user }, Cmd.none
    | Login, LoginPage lp -> 
      if lp.userName = "" then LoginPage { lp with error = true }, Cmd.none
      else CallPage { userName = lp.userName; userToCall = "" }, Cmd.none
    | UpdateUser user, CallPage cp -> CallPage { cp with userToCall = user }, Cmd.none
    | Call, _ -> state, Cmd.none
    | _ -> state, Cmd.none

let fableLogo() = StaticFile.import "./imgs/fable_logo.png"

let renderLoginPage (state:LoginPageState) dispatch =
  Html.div [
    prop.classes [ "p-8"; "space-y-4"; "text-gray-800"; "leading-7" ; "border-0"; "flex"; "flex-col" ]
    prop.children [
      Html.h1 [
        prop.className "font-bold"
        prop.text "Sign in to Chat"
      ]
      Html.input [
        prop.classes ["border-2"; "px-1"; if state.error then "border-red-400" ]
        prop.placeholder "Enter Username"
        prop.type' "text"
        prop.onTextChange (fun s -> dispatch <| UpdateUser s)
      ]
      Html.button [
        prop.className "border-2 rounded-lg bg-yellow-50"
        prop.text "Sign in"
        prop.onClick (fun _ -> dispatch Login)
      ]
    ]
  ]

let renderCallPage (state:CallPageState) dispatch =
  Html.div [
    prop.text state.userName
  ]


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
                    prop.classes [ "divide-y"; "divide-gray-200"]
                    prop.children [
                      match state with
                      | LoginPage p -> renderLoginPage p dispatch
                      | CallPage p ->  renderCallPage p dispatch
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
