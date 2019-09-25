namespace b0wter.FSharp

module Union =

    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Reflection

    /// <summary>
    /// Checks if a value is a certain case of a union. Use like this:
    /// > myValue |> isUnionCase <@ MyUnionCase @>
    /// </summary>
    /// <remarks>
    /// I have not written this code myself, it's from
    /// https://stackoverflow.com/a/11798829
    /// </remarks>
    let rec isCase = function
        | Lambda (_, expr) | Let (_, _, expr) -> isCase expr
        | NewTuple exprs -> 
            let iucs = List.map isCase exprs
            fun value -> List.exists ((|>) value) iucs
        | NewUnionCase (uci, _) ->
            let utr = FSharpValue.PreComputeUnionTagReader uci.DeclaringType
            box >> utr >> (=) uci.Tag
        | _ -> failwith "Expression is no union case."

    