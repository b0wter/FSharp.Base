namespace b0wter.FSharp

module Operators =
    
    let (|?) = Option.orElse
    let (|?|) = defaultArg
    let (|=) = Option.compareIfSome
    let (|=|) = Option.compare
