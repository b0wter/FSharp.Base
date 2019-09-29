namespace b0wter.FSharp

module Union =

    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Reflection


    /// <summary>
    /// Checks wether the given value is of the same case of a union type as the 
    /// case defined by the given expression.
    /// </summary>
    /// <example>
    /// <code>
    /// type TestUnion = First | Second of int | Third of string
    /// let thisIsTrue = First |> isCase <@ First @>
    /// let thisIsTrue = Second 5 |> isCase <@ Second @>
    /// let thisIsTrua = Third "myString" |> isCase <@ Second, Third @>
    /// </code>
    /// </example>
    /// <exception cref="System.Exception">If the expression is not an union case or does not result in an union case.</exception>
    /// <exception cref="System.Exception">If argument to check is not an union case or does not result in an union case.</exception>
    /// <remarks>Note, not all ways an expression may result in a union case are covered in this function.</remarks>
    let rec isCase = function
        | Lambda (_, expr) | Let (_, _, expr) -> isCase expr
        | NewUnionCase (case, _) ->
            // Returns a function that check wether the tag of the argument matches 
            // the tag of the union given in the expression.
            let readTag = FSharpValue.PreComputeUnionTagReader case.DeclaringType
            let comparator = (=) case.Tag
            (fun x -> 
                if FSharpType.IsUnion(x.GetType()) then
                    x :> obj |> (readTag >> comparator)
                else
                    failwith "Value (not expression) is not a union case.")
        | NewTuple expressions ->
            // a tuple may contain several union cases so we can simply 
            // map this functions over all expressions
            let mappedExpressions = expressions |> List.map isCase
            (fun x -> mappedExpressions |> List.exists (fun expression -> x |> expression))
        | _ -> failwith "Expression (not value) is not a union case." 