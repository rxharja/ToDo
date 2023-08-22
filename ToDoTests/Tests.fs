module Tests

open FsCheck.FSharp
open FsCheck.Xunit
open Todo


let genState = ArbMap.defaults |> ArbMap.arbitrary<state>

type EmptyTodo =
    static member State() = genState |> Arb.filter (fun s -> s.newTodo = "")
        
type NonEmptyTodo =
    static member State() = genState |> Arb.filter (fun s -> s.newTodo <> "")


[<Property( Arbitrary=[|typeof<EmptyTodo>|] )>]
let ``Submitting an empty todo does not change the state`` (state: state) =
    (update AddNewTodo state) = state
    
[<Property( Arbitrary=[|typeof<NonEmptyTodo>|] )>]
let ``Submitting an non-empty todo is the previous list plus the new todo `` (state: state) =
    let x = (update AddNewTodo state).todos
    List.length state.todos + 1 = List.length x
    
[<Property>]
let ``newTodo changes given an input`` state input =
    let newState = update (SetNewTodo input) state
    newState.newTodo = input