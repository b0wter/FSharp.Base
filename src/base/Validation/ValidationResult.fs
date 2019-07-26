namespace b0wter.FSharp.Validation

module ValidationResult =

    /// <summary>
    /// Simple union type with a success state and a typed error state.
    /// </summary>
    type Simple<'a>
        = Success
        | Failure of 'a

    /// <summary>
    /// Advanced union typed with a typed success and error state.
    /// </summary>
    type Complex<'a, 'b>
        = Success of 'a
        | Failure of 'b