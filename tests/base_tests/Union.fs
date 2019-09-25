namespace BaseTests.Result

module isUnionCase =

    open FsUnit
    open FsUnit.Xunit
    open Xunit
    open b0wter.FSharp

    type TestUnion
        = First
        | Second
        | Third

    [<Fact>]
    let ``Given a union type of matching case returns true`` () =
        let value = First
        value |> Union.isCase<@ First @> |> should be True

    [<Fact>]
    let ``Given a union type of non-matching case returns f`` () =
        let value = Second
        value |> Union.isCase<@ First @> |> should be False

    [<Fact>]
    let ``Given a non-union type throws an exception`` () =
        let value = Third
        (fun () -> value |> Union.isCase<@ int @> |> ignore) |> should (throwWithMessage "Expression is no union case.") typeof<System.Exception>