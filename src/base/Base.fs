namespace b0wter.FSharp
module Base =

    /// <summary>
    /// checks if the sequence contains a None element.
    /// </summary>
    let containsNone (items: seq<'a option>) : bool =
        items |> Seq.exists (fun element -> match element with | Some _ -> false | None -> true)

    /// <summary>
    /// Checks if a collection of Result<...,...> contains at least one error value.
    /// </summary>
    let containsError (items: Result<'a, 'b> seq) : bool =
        items |> Seq.exists (fun element -> match element with | Ok _ -> false | Error _ -> true)
        
    /// <summary>
    /// Filters a collection of Result<...,...> and returns only Ok values.
    /// </summary>  
    let filterOks (items: Result<'a, 'b> seq) : 'a seq =
        items
        |> Seq.map (fun item -> match item with 
                                | Ok success -> Some success
                                | Error _    -> None)
        |> Seq.choose id                            
                                
    /// <summary>
    /// Filters a collection of Result<...,...> and returns only Error values.
    /// </summary>  
    let filterErrors (items: Result<'a, 'b> seq) : 'b seq =
        items
        |> Seq.map (fun item -> match item with 
                                | Ok _    -> None
                                | Error e -> Some e)
        |> Seq.choose id 

    /// <summary>
    /// Returns wether the given integer is an even number.
    /// </summary>
    let isEven i : bool =
        i % 2 = 0
        
    /// <summary>
    /// Returns wether the given integer is an odd number.
    /// </summary>
    let isOdd i : bool =
        (isEven i) |> not