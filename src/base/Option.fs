namespace b0wter.FSharp

module Option =
    
    let getOrElse p o =
        match o with
        | Some x -> x
        | _ -> p

