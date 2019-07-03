namespace b0wter.FSharp

module Operators =
    
    let (|?) = Option.orElse
    let (|?|) = defaultArg
