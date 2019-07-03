namespace b0wter.FSharp

module Option =
    
    /// <summary>
    /// Gets the value from an option if it's Some. Otherwise returns p.
    /// </summary>
    let getOrElse ``else`` toTest =
        match toTest with
        | Some x -> x
        | _ -> ``else``
        
    /// <summary>
    /// Compares two options.
    /// Only returns true of both are Some and are equal.
    /// </summary>
    let compare (a: Option<_>) (b: Option<_>) : bool =
        match (a, b) with
        | Some a, Some b -> a = b
        | _ -> false
