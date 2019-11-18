namespace b0wter.FSharp

module DateTime =
    open System

    /// Calls DateTime.Parse(...) and wraps the message of the exception as `Error`.
    let parse (s: string) : Result<DateTime, string> =
        try
            Ok <| System.DateTime.Parse(s)
        with
        | ex -> Error ex.Message

    /// Calls DateTime.TryParse(...) and returns `None` if the parsing failed.
    let tryParse (s: string) : Option<DateTime> =
        let success, result = DateTime.TryParse(s)
        if success then Some result else None    