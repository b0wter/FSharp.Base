namespace b0wter.FSharp

module IO =
    open System.IO
    
    module File =
        
        let readAllText file =
            if File.Exists(file) then
                try
                    File.ReadAllText(file) |> Ok
                with
                    ex -> Error ex.Message
            else
                Error <| sprintf "The given file '%s' does not exist." file
            
        
