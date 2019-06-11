namespace b0wter.FSharp

module Patterns =
    
    /// <summary>
    /// Active pattern to match integers.
    /// </summary>
    let (|Int|_|) (str: string) =
        match System.Int32.TryParse(str) with
        | (true,int) -> Some(int)
        | _ -> None
   
    /// <summary>
    /// Active pattern to match floats.
    /// </summary>
    let (|Float|_|) (str: string) =
        match System.Double.TryParse(str) with
        | (true,float) -> Some(float)
        | _ -> None

