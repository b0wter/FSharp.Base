namespace b0wter.FSharp

module String =

    /// Checks wether a string contains any chars that are non-whitespace-characters.
    let isNullOrWhiteSpace = System.String.IsNullOrWhiteSpace

    /// Wraps System.String.Join to allow it to be partially applied.
    let join (delimiter: string) (strings: string seq) = System.String.Join(delimiter, strings)