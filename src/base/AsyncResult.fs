namespace b0wter.FSharp

open System

[<Obsolete("This code is obsolete. Please use the FsToolkit instead.")>]
module AsyncResult =

    /// Works like `Result.bind` except that the result and the function are async.
    let bindA asyncOp asyncResult =
        async {
            let! r = asyncResult
            match r with
            | Ok o -> return! asyncOp o
            | Error e -> return Error e
        }
        
    /// Works like `Result.bindError` except that the result and the function are async.
    let bindErrorA (asyncOp: 'b -> Async<Result<'a, 'c>>) (asyncResult: Async<Result<'a, 'b>>) : Async<Result<'a, 'c>> =
        async {
            let! r = asyncResult
            match r with
            | Ok o -> return Ok o
            | Error e -> return! e |> asyncOp
        }
        
    /// Works like `Result.bind` except that the initial result is async (`Async<Result<_,_>>`)
    /// while the function is not.
    let bind syncOp asyncResult =
        async {
            let! r = asyncResult
            match r with
            | Ok o -> return syncOp o
            | Error e -> return Error e
        }
        
    /// Works like `Result.bindError` except that the initial result is async (`Async<Result<_,_>>`)
    /// while the function is not.
    let bindError (syncOp: 'b -> Result<'a, 'c>) (asyncResult: Async<Result<'a, 'b>>) : Async<Result<'a, 'c>> =
        async {
            let! r = asyncResult
            match r with
            | Ok o -> return Ok o
            | Error e -> return e |> syncOp
        }

    /// Works like `Result.map` except that the result and the function are async.
    let mapA (asyncOp: 'a -> Async<'b>) (asyncResult: Async<Result<'a, 'c>>) =
        async {
            let! r = asyncResult
            match r with
            | Ok o -> let! a = asyncOp o
                      return Ok a
            | Error e -> return Error e
        }
        
    /// Works like `Result.mapError` except that the result and the function are async.
    let mapErrorA (asyncOp: 'b -> Async<'c>) (asyncResult: Async<Result<'a, 'b>>) : Async<Result<'a, 'c>> =
        async {
            let! r = asyncResult
            match r with
            | Ok o -> return Ok o
            | Error e -> let! c = (e |> asyncOp)
                         return Error c
        }
        
    /// Works like `Result.map` except that the initial result is async (`Async<Result<_,_>>`)
    /// while the function is not.
    let map syncOp asyncResult =
        async {
            let! r = asyncResult
            match r with
            | Ok o -> return (Ok <| syncOp o)
            | Error e -> return Error e
        }
        
    /// Works like `Result.mapError` except that the initial result is async (`Async<Result<_,_>>`)
    /// while the function is not.
    let mapError (syncOp: 'b -> 'c) (asyncResult: Async<Result<'a, 'b>>) : Async<Result<'a, 'c>> =
        async {
            let! r = asyncResult
            match r with
            | Ok o -> return Ok o
            | Error e -> return (Error <| syncOp e)
        }

    /// Takes two functions two map both possible values of a result.
    let mapBoth (okSyncOp: 'a -> 'c) (errorSyncOp: 'b -> 'd) (asyncResult: Async<Result<'a, 'b>>) : Async<Result<'c, 'd>> =
        async {
            let! r = asyncResult
            match r with
            | Ok o -> return (Ok <| okSyncOp o)
            | Error e -> return (Error <| errorSyncOp e)
        }

    /// Takes two async functions two map both possible values of a result.
    let mapBothA (okAsyncOp: 'a -> Async<'c>) (errorAsyncOp: 'b -> Async<'d>) (asyncResult: Async<Result<'a, 'b>>) : Async<Result<'c, 'd>> =
        async {
            let! r = asyncResult
            match r with
            | Ok o -> 
                let! a = o |> okAsyncOp
                return Ok a
            | Error e -> 
                let! b = e |> errorAsyncOp
                return Error b
        }

    /// Takes two functions two map both possible values of a result.
    /// The function to map Ok is async while the other is sync.
    let mapBothOkA (okAsyncOp: 'a -> Async<'c>) (errorSyncOp: 'b -> 'd) (asyncResult: Async<Result<'a, 'b>>) : Async<Result<'c, 'd>> =
        async {
            let! r = asyncResult
            match r with
            | Ok o ->
                let! a = o |> okAsyncOp
                return Ok a
            | Error e -> return (Error <| errorSyncOp e)
        }

    /// Takes two functions two map both possible values of a result.
    /// The function to map Error is async while the other is sync.
    let mapBothErrorA (okSyncOp: 'a -> 'c) (errorAsyncOp: 'b -> Async<'d>) (asyncResult: Async<Result<'a, 'b>>) : Async<Result<'c, 'd>> =
        async {
            let! r = asyncResult
            match r with
            | Ok o -> return (Ok <| okSyncOp o)
            | Error e -> 
                let! b = e |> errorAsyncOp
                return Error b
        }

    /// Transforms both values of a result.
    /// Binds Ok and Error.
    /// Both transformations are sync.
    let bindBoth (okSyncOp: 'a -> Result<'c, 'd>) (errorSyncOp: 'b -> Result<'c, 'd>) (asyncResult: Async<Result<'a, 'b>>) : Async<Result<'c, 'd>> =
        async {
            let! r = asyncResult
            match r with
            | Ok o -> return (okSyncOp o)
            | Error e -> return (errorSyncOp e)
        }

    /// Transforms both values of a result.
    /// Binds Ok and Error.
    /// Both transformations are async.
    let bindBothA (okAsyncOp: 'a -> Async<Result<'c, 'd>>) (errorAsyncOp: 'b -> Async<Result<'c, 'd>>) (asyncResult: Async<Result<'a, 'b>>) : Async<Result<'c, 'd>> =
        async {
            let! r = asyncResult
            match r with
            | Ok o -> 
                let! a = o |> okAsyncOp
                return a
            | Error e -> 
                let! b = e |> errorAsyncOp
                return b
        }

    /// Transforms both values of a result.
    /// Binds Ok and Error.
    /// Ok transformation is async.
    let bindBothOkA (okAsyncOp: 'a -> Async<Result<'c, 'd>>) (errorSyncOp: 'b -> Result<'c, 'd>) (asyncResult: Async<Result<'a, 'b>>) : Async<Result<'c, 'd>> =
        async {
            let! r = asyncResult
            match r with
            | Ok o ->
                let! a = o |> okAsyncOp
                return a
            | Error e -> return (errorSyncOp e)
        }

    /// Transforms both values of a result.
    /// Binds Ok and maps Error.
    /// Error transformation is async.
    let bindBothErrorA (okSyncOp: 'a -> Result<'c, 'd>) (errorAsyncOp: 'b -> Async<Result<'c, 'd>>) (asyncResult: Async<Result<'a, 'b>>) : Async<Result<'c, 'd>> =
        async {
            let! r = asyncResult
            match r with
            | Ok o -> return (okSyncOp o)
            | Error e -> 
                let! b = e |> errorAsyncOp
                return b
        }

    /// Transforms both values of a result.
    /// Binds Ok and maps Error.
    /// Error transformation is async.
    let bindOkmapError (okSyncOp: 'a -> Result<'c, 'd>) (errorSyncOp: 'b -> 'd) (asyncResult: Async<Result<'a, 'b>>) : Async<Result<'c, 'd>> =
        async {
            let! r = asyncResult
            match r with
            | Ok o -> return (okSyncOp o)
            | Error e -> return Error (errorSyncOp e)
        }

    /// Transforms both values of a result.
    /// Binds Ok and maps Error.
    /// Both transformation are async.
    let bindOkAmapErrorA (okAsyncOp: 'a -> Async<Result<'c, 'd>>) (errorAsyncOp: 'b -> Async<'d>) (asyncResult: Async<Result<'a, 'b>>) : Async<Result<'c, 'd>> =
        async {
            let! r = asyncResult
            match r with
            | Ok o -> 
                let! a = o |> okAsyncOp
                return a
            | Error e -> 
                let! b = e |> errorAsyncOp
                return Error b
        }

    /// Transforms both values of a result.
    /// Binds Ok and maps Error.
    /// Ok transformation is async, Error transfomration is sync.
    let bindOkAmapErrorOk (okAsyncOp: 'a -> Async<Result<'c, 'd>>) (errorSyncOp: 'b -> 'd) (asyncResult: Async<Result<'a, 'b>>) : Async<Result<'c, 'd>> =
        async {
            let! r = asyncResult
            match r with
            | Ok o ->
                let! a = o |> okAsyncOp
                return a
            | Error e -> return Error (errorSyncOp e)
        }

    /// Transforms both values of a result.
    /// Binds Ok and maps Error.
    /// Ok transformation is sync, Error transfomration is async.
    let bindOkmapErrorA (okSyncOp: 'a -> Result<'c, 'd>) (errorAsyncOp: 'b -> Async<'d>) (asyncResult: Async<Result<'a, 'b>>) : Async<Result<'c, 'd>> =
        async {
            let! r = asyncResult
            match r with
            | Ok o -> return (okSyncOp o)
            | Error e -> 
                let! b = e |> errorAsyncOp
                return Error b
        }

    /// Transforms both values of a result.
    /// Maps Ok and binds Error.
    /// Both transformations are sync.
    let mapOkBindError (okSyncOp: 'a -> 'c) (errorSyncOp: 'b -> Result<'c,'d>) (asyncResult: Async<Result<'a, 'b>>) : Async<Result<'c, 'd>> =
        async {
            let! r = asyncResult
            match r with
            | Ok o -> return Ok (okSyncOp o)
            | Error e -> return (errorSyncOp e)
        }

    /// Transforms both values of a result.
    /// Maps Ok and binds Error.
    /// Both transformations are async.
    let mapOkAbindErrorA (okAsyncOp: 'a -> Async<'c>) (errorAsyncOp: 'b -> Async<Result<'c,'d>>) (asyncResult: Async<Result<'a, 'b>>) : Async<Result<'c, 'd>> =
        async {
            let! r = asyncResult
            match r with
            | Ok o -> 
                let! a = o |> okAsyncOp
                return Ok a
            | Error e -> 
                let! b = e |> errorAsyncOp
                return b
        }

    /// Transforms both values of a result.
    /// Maps Ok and binds Error.
    /// Ok transformation is async, Error transformation is sync.
    let mapOkAbindErrorOk (okAsyncOp: 'a -> Async<'c>) (errorSyncOp: 'b -> Result<'c, 'd>) (asyncResult: Async<Result<'a, 'b>>) : Async<Result<'c, 'd>> =
        async {
            let! r = asyncResult
            match r with
            | Ok o ->
                let! a = o |> okAsyncOp
                return Ok a
            | Error e -> return (errorSyncOp e)
        }

    /// Transforms both values of a result.
    /// Maps Ok and binds Error.
    /// Ok transformation is sync, Error transformation is async.
    let mapOkbindErrorA (okSyncOp: 'a -> 'c) (errorAsyncOp: 'b -> Async<Result<'c, 'd>>) (asyncResult: Async<Result<'a, 'b>>) : Async<Result<'c, 'd>> =
        async {
            let! r = asyncResult
            match r with
            | Ok o -> return Ok (okSyncOp o)
            | Error e -> 
                let! b = e |> errorAsyncOp
                return b
        }
