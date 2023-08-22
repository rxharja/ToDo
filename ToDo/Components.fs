module Components

    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    
    let actionButton attributes =
        [ Button.background "#18181b" ] @ attributes |> Button.create