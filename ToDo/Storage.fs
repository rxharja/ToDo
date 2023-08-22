namespace Todos

open System

[<AutoOpen>]
module Domain =
    type Todo =
        { Id : Guid
          Description : string
          Created : DateTime
          Completed : bool }
        
    type EditingTodo =
        { editingId: Guid
          description: string }
        
    let todoId t = t.Id
    let todoDesc t = t.Description
    let completed t = t.Completed
    let created t = t.Created
    let editingId e = e.editingId
    let editingDesc e = e.description
    
    let findById id = List.find (fun x -> x.Id = id)
    let newTodo desc =
        { Id = Guid.NewGuid(); Description = desc; Completed = false; Created = DateTime.Now }
        
    let maybeEdited (todo: Todo) = List.tryFind (fun x -> x.editingId = todo.Id)
    let toInt bool = match bool with | true -> 1 | false -> 0
    

[<RequireQualifiedAccess>]
module Storage =
    open System.IO
    let private path = 
        Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "Todo.db")
    let private database = new LiteDB.LiteDatabase $"Filename={path}"
    let private todos = database.GetCollection<Todo> "todos"

    let get = todos.FindAll >> Seq.sortBy created >> List.ofSeq
    let insert : Todo -> Todo list = 
        todos.Insert >> ignore >> get
    let update : Todo -> Todo list = 
        todos.Update >> ignore >> get
    let remove : Guid -> Todo list = 
        todos.Delete >> ignore >> get
    let removeMany : Guid list -> Todo list = 
        List.map todos.Delete >> ignore >> get