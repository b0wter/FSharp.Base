namespace b0wter.FSharp

module AsyncResult =

    let bind aAsyncResult bAsyncResult =
        async {
            let! b = bAsyncResult
            match b with
            | Ok o -> return! aAsyncResult o
            | Error e -> return Error e
        }

