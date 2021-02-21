namespace b0wter.FSharp

open System

module String =

    /// Checks wether a string contains any chars that are non-whitespace-characters.
    let isNullOrWhiteSpace = System.String.IsNullOrWhiteSpace

    /// Wraps System.String.Join to allow it to be partially applied.
    let join (delimiter: string) (strings: string seq) = System.String.Join(delimiter, strings)

    /// Wraps System.String.Contains
    let contains toSearch (s: string) = s.Contains(toSearch)

    /// Wraps System.String.Replace
    let replace (old: string) ``new`` (s: string) = s.Replace(old, ``new``)

    /// Takes `i` characters from the string. If the string has less characters the total string is returned.
    /// If the string is null `System.String.Empty` is returned.
    let take (i: int) (s: string) =
        if s = null then
            String.Empty
        elif s.Length < i then
            s
        else
            s.Substring(0, i)
        
    /// Removes the first `i` characters from the string. If the string has less characters than `i` an empty
    /// string is returned.
    let skip (i: int) (s: string) =
        if s = null then
            String.Empty
        elif i < 0 then
            failwith "Cannot skip a negative number of characters."
        elif i = 0 then
            s
        elif i > s.Length then
            String.Empty
        else
            s.Substring(i)
