namespace b0wter.FSharp

module String =

    /// Checks wether a string contains any chars that are non-whitespace-characters.
    let isNullOrWhiteSpace = System.String.IsNullOrWhiteSpace