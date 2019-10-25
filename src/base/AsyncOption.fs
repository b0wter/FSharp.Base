namespace b0wter.FSharp

module AsyncOption =

    /// Maps a sync function on an async option.
    let map (syncOp: 'a -> 'a) (asyncOption: Async<'a option>) : Async<'a option> =
        async {
            let! o = asyncOption
            return o |> Option.map syncOp
        }

    /// Maps an async function on an async option.
    let mapA (asyncOp: 'a -> Async<'a>) (asyncOption: Async<'a option>) : Async<'a option> =
        async {
            let! o = asyncOption
            match o with
            | Some r -> return! (r |> asyncOp) |> Async.map Some
            | None -> return None
        }

    /// Binds a sync function on an async option.
    let bind (syncOp: 'a -> 'a option) (asyncOption: Async<'a option>) : Async<'a option> =
        async {
            let! o = asyncOption
            return o |> Option.bind syncOp
        }

    /// Binds an async function on an async option.
    let bindA (asyncOp: 'a -> Async<'a option>) (asyncOption: Async<'a option>) : Async<'a option> =
        async {
            let! o = asyncOption
            match o with
            | Some r -> 
                match! r |> asyncOp with
                | Some a -> return Some a
                | None -> return None
            | None -> return None
        }
