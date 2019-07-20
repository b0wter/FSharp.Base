namespace b0wter.FSharp

module Parsers =
    open System

    let private returnIfSuccess (success: bool, result: 'a) : 'a option =
        if success then Some result else None

    let parseInt (s: string) : int option =
        returnIfSuccess <| Int32.TryParse(s)

    let parseFloat (s: string) : float option =
        returnIfSuccess <| Double.TryParse(s)

    let parseGuid (s: string) : System.Guid option =
        returnIfSuccess <| Guid.TryParse(s)