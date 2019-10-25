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
        
    /// <summary>
    /// Compares the option value with th given value.
    /// Always returns false if the option is None.
    /// </summary>
    let compareIfSome (a: 'a) (o: 'a option) : bool =
        match o with
        | Some value -> a = value
        | None -> false

    /// Transforms a result into an option.
    let ofResult = function
        | Ok o -> Some o
        | Error _ -> None