namespace b0wter.FSharp

module Operators =
    
    let (|?) = b0wter.FSharp.Option.orElse
    let (|?|) = defaultArg
