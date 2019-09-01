namespace b0wter.FSharp

module Parsers =
    open System

    let private returnIfSuccess (success: bool, result: 'a) : 'a option =
        if success then Some result else None

    /// <summary>
    /// Takes a string and returns Some int if the string contains an integer, otherwise None.
    /// </summary>
    let parseInt (s: string) : int option =
        returnIfSuccess <| Int32.TryParse(s)

    /// <summary>
    /// Takes a string and returns Some double if the string contains a floating point number, otherwise None.
    /// </summary>
    let parseFloat (s: string) : float option =
        returnIfSuccess <| Double.TryParse(s)

    /// <summary>
    /// Takes a string and returns Some Guid if the string contains a Guid, otherwise None.
    /// </summary>
    let parseGuid (s: string) : System.Guid option =
        returnIfSuccess <| Guid.TryParse(s)