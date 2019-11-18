namespace b0wter.FSharp

module Async =

    /// <summary>
    /// Performs a map operation on the result of an Async.
    /// </summary>
    let map f operation = async {
        let! x = operation
        let value = f x
        return value
    }    

    /// Performs a bind operation on the result of an Async.
    let bind (f: 'a -> Async<'b>) (input: Async<'a>) = async {
        let! i = input
        let! value = f i
        return value
    }