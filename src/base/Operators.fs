namespace b0wter.FSharp

module Operators =
    
    let (|?) = Option.orElse
    let (|?|) = defaultArg
    let (|=) = fun o a -> Option.compareIfSome a o
    let (|=|) = Option.compare
