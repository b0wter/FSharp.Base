namespace b0wter.FSharp

module Result =

    /// <summary>
    /// Takes a mapping for a value and an error an applies it based on the contents of the result.
    /// E.g.: ifValue is applied if the result is `Ok o` and ifError is applied if the result is `Error e`.
    /// The return value will still be wrapped in a Result-Type!
    /// </summary>
    let mapBoth (ifValue: 'value -> 'newValue) (ifError: 'error -> 'newError) (result: Result<'value, 'error>) : Result<'newValue, 'newError> =
        match result with
        | Ok o -> Ok (o |> ifValue)
        | Error e -> Error (e |> ifError)
        
    /// <summary>
    /// Takes a binding for a value and an error an applies it based on the contents of the result.
    /// E.g.: ifValue is applied if the result is `Ok o` and ifError is applied if the result is `Error e`.
    /// The return value will be wrapped in a non-nested Result-Type!
    /// </summary>        
    let bindBoth (ifValue: 'value -> Result<'newValue, 'newError>) (ifError: 'error -> Result<'newValue, 'newError>) (result: Result<'value, 'error>) : Result<'newValue, 'newError> =
        match result with
        | Ok o -> o |> ifValue
        | Error e -> e |> ifError