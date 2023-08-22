module Test

open Avalonia.FuncUI.DSL
open Avalonia.Controls

type State = { count : int }

type Msg =
    | Increment
    | Decrement
    
let update msg state =
    match msg with
    | Increment -> { state with count = state.count + 1  }
    | Decrement -> { state with count = state.count - 1  }

let render state dispatch =
    StackPanel.create [
        StackPanel.children [
            TextBlock.create [
                TextBlock.text (state.count |> string)
            ]
            Button.create [
                Button.content "Increment"
                Button.onClick (fun _ -> dispatch Increment)
            ]
            Button.create [
                Button.content "Decrement"
                Button.onClick (fun _ -> dispatch Decrement)
            ]
        ]
    ]