namespace b0wter.FSharp.Collections
module Array =
    
    let private toArray (left: 'a list, right: 'a list) =
        (left |> Array.ofList, right |> Array.ofList)    
    
    let splitBy (matchLeft: bool) (p: 'a -> bool) (items: 'a []) =
        (items |> List.ofArray) 
        |> (List.splitBy matchLeft p)
        |> toArray
        
    let missing (next: 'a -> 'a) (current: 'a) (equals: 'a -> 'a -> bool) (list: 'a []) =
        (list |> List.ofArray)
        |> (List.missing next current equals)
        |> Array.ofList
        
