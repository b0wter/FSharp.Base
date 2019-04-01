namespace b0wter.FSharp.Collections
module Seq =
    
    let private toSeq (left: 'a list, right: 'a list) =
        (left |> Seq.ofList, right |> Seq.ofList)    
    
    let splitBy (matchLeft: bool) (p: 'a -> bool) (items: seq<'a>) =
        (items |> List.ofSeq) 
        |> (List.splitBy matchLeft p)
        |> toSeq
        
    let missing (next: 'a -> 'a) (current: 'a) (equals: 'a -> 'a -> bool) (list: seq<'a>) =
        (list |> List.ofSeq)
        |> (List.missing next current equals)
        |> Seq.ofList
        
       