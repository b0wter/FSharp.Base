module Base.Array
    open Base
    
    let private toArray (left: 'a list, right: 'a list) =
        (left |> Array.ofList, right |> Array.ofList)    
    
    let splitBy (items: 'a array) =
        List.splitBy >> toArray
        