module Todo

open System
open Components
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Todos
    
type state =
   { todos : Todo list
     beingEdited : EditingTodo list
     newTodo : string }

type Msg =
    | SetNewTodo of string
    | AddNewTodo
    | ToggleCompleted of Guid
    | DeleteTodo of Guid
    | CancelEdit of Guid
    | ApplyEdit of Guid
    | StartEditing of Guid
    | SetEditedDescription of Guid * string
    | ClearCompleted
    
let init () = { todos = Storage.get (); beingEdited = []; newTodo = "" }

let newList state map = { state with todos = map state.todos  }
             
let update msg state =
    match msg with
    | SetNewTodo text -> { state with newTodo = text }
    
    | DeleteTodo id -> { state with todos = Storage.remove id }
    
    | ToggleCompleted id ->
        state.todos
        |> findById id
        |> fun t -> { t with Completed = not t.Completed }
        |> fun t -> { state with todos = Storage.update t }
    
    | AddNewTodo when state.newTodo = "" -> state
    | AddNewTodo -> { state with newTodo = ""; todos = Storage.insert (newTodo state.newTodo) }
    
    | StartEditing id ->
        let next = state.todos
                   |> findById id
                   |> fun t -> { editingId = t.Id ; description = t.Description }
                   |> fun e -> e :: state.beingEdited
                   
        { state with beingEdited = next }
    
    | CancelEdit id ->
        { state with beingEdited = List.filter (fun e -> e.editingId <> id) state.beingEdited }
    
    | ApplyEdit id ->
        let edited = List.find (fun e -> e.editingId = id) state.beingEdited
        
        match edited with
        | edited when edited.description = "" -> state
        | edited -> let next = state.todos |> List.find (fun t -> t.Id = edited.editingId )
                    { state with todos = Storage.update { next with Description = edited.description }
                                 beingEdited = List.filter (fun e -> e.editingId <> id) state.beingEdited }
                         
    | SetEditedDescription (id,s) ->
        let next = List.map (fun e -> if e.editingId = id then { e with description = s } else e) state.beingEdited
        {state with beingEdited = next }

    | ClearCompleted -> { state with todos = state.todos |> List.filter completed |> List.map todoId |> Storage.removeMany }
        
let appTitle = 
    TextBlock.create [ 
        Grid.row 0
        TextBlock.classes ["title"]
        TextBlock.text "My To-Do List" 
    ]
    
let inputControl state dispatch =
    Grid.create [
        Grid.row 1
        Grid.margin (10,10)
        Grid.columnDefinitions (ColumnDefinitions.Parse "80*,20*")
        Grid.children [
            TextBox.create [
                TextBox.column 0
                TextBox.text state.newTodo
                TextBox.onKeyDown (fun a -> if a.Key = Key.Enter then dispatch AddNewTodo else ())
                TextBox.onTextChanged (SetNewTodo >> dispatch)
            ]
            Button.create [
                Button.horizontalAlignment HorizontalAlignment.Right
                Button.column 1
                Button.content "submit"
                Button.isEnabled (state.newTodo <> "")
                Button.onClick (fun _ -> dispatch AddNewTodo)
            ]
        ]
    ]

let todoItem { Description = desc; Completed = completed ; Id = id } dispatch =
    View.createWithKey (id.ToString())
        Grid.create [
            Grid.margin (10,2,10,2)
            Grid.columnDefinitions (ColumnDefinitions.Parse "20*,60*,10*,10*")
            Grid.children [
                CheckBox.create [
                    CheckBox.column 0
                    CheckBox.isChecked completed
                    CheckBox.onClick (fun _ -> dispatch (ToggleCompleted id))
                ]
                TextBlock.create [
                    TextBlock.verticalAlignment VerticalAlignment.Center 
                    TextBlock.textAlignment TextAlignment.Left 
                    TextBlock.column 1
                    TextBlock.text desc
                    TextBlock.classes (if completed then [ "complete" ] else [])
                ]
                actionButton [
                    Button.horizontalAlignment HorizontalAlignment.Right
                    Button.column 2
                    Button.content "✏️"
                    Button.onClick (fun _ -> dispatch (StartEditing id))
                ]
                actionButton [
                    Button.horizontalAlignment HorizontalAlignment.Right
                    Button.column 3
                    Button.content "🗑️"
                    Button.onClick (fun _ -> dispatch (DeleteTodo id) )
                ]
            ]
        ]
        
let editTodo edited (original: Todo) dispatch =
   Grid.create [
        Grid.margin (10,2,10,2)
        Grid.columnDefinitions (ColumnDefinitions.Parse "80*,10*,10*")
        Grid.children [
            TextBox.create [
                TextBox.column 0
                TextBox.text edited.description
                TextBox.onTextChanged (fun x -> SetEditedDescription (edited.editingId, x) |> dispatch)
                TextBox.onKeyDown (fun a ->
                    match a.Key with
                    | Key.Enter when original.Description <> edited.description ->
                        dispatch (ApplyEdit edited.editingId)
                    | Key.Escape -> dispatch (CancelEdit edited.editingId)
                    | _ -> () )
            ]
            actionButton [
                Button.horizontalAlignment HorizontalAlignment.Right
                Button.column 1
                Button.isEnabled (edited.description <> original.Description)
                Button.content "️☑️"
                Button.hotKey (KeyGesture Key.Enter)
                Button.onClick (fun _ -> dispatch (ApplyEdit edited.editingId))
            ]
            actionButton [
                Button.horizontalAlignment HorizontalAlignment.Right
                Button.column 2
                Button.content "️🗙"
                Button.hotKey (KeyGesture Key.Enter)
                Button.onClick (fun _ -> dispatch (CancelEdit edited.editingId))
            ]
        ]
    ] 

let todoView (state: state) (dispatch: Msg -> unit): IView list =
    state.todos
    |> List.map ( fun todo ->
        Border.create [
            Border.margin (0, 2)
            Border.background "#18181b"
            Border.cornerRadius 3 
            Border.child (match maybeEdited todo state.beingEdited with
                          | Some edit -> editTodo edit todo dispatch
                          | _ -> todoItem todo dispatch)
        ])
    
let todoList state dispatch = 
    ScrollViewer.create [
        DockPanel.dock Dock.Top
        ScrollViewer.content 
            (StackPanel.create [ 
                StackPanel.children (todoView state dispatch) 
            ])
    ]
    
let filterPanel state dispatch =            
    TabControl.create [
        Grid.row 2
        TabControl.viewItems [
            TabItem.create [
                TabItem.content (todoList state dispatch)
                TabItem.header "All"
            ]

            TabItem.create [
                TabItem.content (todoList (newList state (List.filter completed)) dispatch)
                TabItem.header "Completed"
            ]

            TabItem.create [
                TabItem.content (todoList (newList state (List.filter (not << completed))) dispatch)
                TabItem.header "Not Completed"
            ]
        ]
    ]

let total state dispatch =
    StackPanel.create [
        Grid.row 3
        StackPanel.margin (12,10)
        StackPanel.verticalAlignment VerticalAlignment.Bottom
        StackPanel.horizontalAlignment HorizontalAlignment.Right
        StackPanel.orientation Orientation.Horizontal
        StackPanel.children [
            if state.todos |> List.filter completed |> List.length > 0 
            then Button.create [
                    Button.verticalAlignment VerticalAlignment.Center
                    Button.horizontalAlignment HorizontalAlignment.Right
                    Button.content $"Clear {List.sumBy (completed >> toInt) state.todos}"
                    Button.onClick (fun _ -> dispatch ClearCompleted)
                 ]
        ]
    ]

let render state dispatch =
    Grid.create [
        Grid.rowDefinitions (RowDefinitions.Parse("50,50,350*,50"))
        Grid.columnDefinitions (ColumnDefinitions.Parse("*"))
        Grid.children [
            appTitle

            inputControl state dispatch

            filterPanel state dispatch

            total state dispatch
        ]
    ]