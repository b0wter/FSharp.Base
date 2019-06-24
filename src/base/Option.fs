namespace b0wter.FSharp

module Option =
    
    let getOrElse o p =
        match o with
        | Some x -> x
        | _ -> p

