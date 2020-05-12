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

    /// Binds an async operation to a Result.
    let bindA (ifValue: 'value -> Async<Result<'newValue, 'error>>) (result: Result<'value, 'error>) : Async<Result<'newValue, 'error>> =
        async {
            match result with
            | Ok o -> return! (o |> ifValue)
            | Error e -> return (Error e )
        }

    /// Maps an async function on the Ok of a Result.
    let mapA (ifValue: 'value -> Async<'newValue>) (result: Result<'value, 'error>) : Async<Result<'newValue, 'error>> =
        async {
            match result with
            | Ok o -> 
                let! result = (o |> ifValue)
                return Ok result
            | Error e -> return (Error e)
        }

    /// Turns a list of results into a result containing a list of Ok values.
    let all (results: Result<'a, 'b> list) : Result<'a list, 'b> =
        let rec run (accumulator: 'a list) (remaining: Result<'a, 'b> list) =
            match remaining with
            | [] -> Ok accumulator
            | (Ok o) :: tail -> run (o :: accumulator) tail
            | (Error e) :: _ -> Error e
        run [] results |> Result.map (fun aa -> aa |> List.rev)
        