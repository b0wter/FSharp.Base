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