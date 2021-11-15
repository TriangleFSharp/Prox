module App

open Feliz
open Elmish
open Shared

type LoginPageState = {
  userName : string
  error: bool
}

type ChannelId = System.Guid
type Channel = {
  id : ChannelId
  name : string
  volume : float
}

let mkChannel name = { id = System.Guid.NewGuid(); Channel.name = name; volume = 0. }

type CallPageState = {
  userName : string
  channels : Channel list
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
  | ToggleChannel of string
  | SetVolume of channelId:string * value:float

let sampleChannels = 
  [1..7] 
  |> Seq.map (sprintf "Channel %i") 
  |> Seq.map mkChannel 
  |> Seq.toList

let init() = LoginPage { userName = "" ; error = false }, Cmd.none

let update (msg: Msg) (state: State) =
    match msg, state with
    | UpdateUser user, LoginPage lp -> LoginPage { lp with userName = user }, Cmd.none
    | Login, LoginPage lp -> 
      if lp.userName = "" then LoginPage { lp with error = true }, Cmd.none
      else CallPage { userName = lp.userName; channels = mkChannel lp.userName :: sampleChannels  }, Cmd.none
    | _ -> state, Cmd.none

let fableLogo() = StaticFile.import "./imgs/fable_logo.png"

let renderbutton (text:string) (color:string) clicky =
  Html.button [
    prop.className (sprintf "bg-%s-500 hover:bg-%s-700 text-white font-bold py-2 px-4 rounded" color color)
    prop.text text
    prop.onClick clicky
  ]

let renderLoginPage (state:LoginPageState) dispatch =
  Html.form [
    prop.onSubmit (fun _ -> dispatch Login)
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

let renderChannel (channel:Channel) =
  Html.div [ 
    prop.classes ["border-2 w-16 h-16 text-center text-white";if channel.volume > 0. then "bg-green-500 font-bold" else "bg-red-500"; ]
    prop.children [
      Html.span [ prop.text channel.name; prop.className "px-1" ]
      Html.input [
        prop.type' "range"
        prop.className "w-16"
        prop.min 0
        prop.max 100
        prop.step 5
        // prop.value (if channel.enable then 100 else 0)
      ]
    ]
  ]

let renderCallPage (state:CallPageState) dispatch =
  // New idea: Lets show each user in a box and let you toggle them on/off
  // Step 2 would be to show a volume slider <input type="range" min=0 max=100 value=7 step=5 />
  Html.div [
    prop.children [
    Html.div [
      prop.className "flex flex-wrap w-96 space-x-0 my-8"
      prop.children [
        for ch in state.channels do renderChannel ch
      ]
    ]
    renderbutton "Press to Talk" "green" ignore
  ]
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
