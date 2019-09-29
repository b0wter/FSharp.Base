namespace BaseTests.Result

module caseName =

    open FsUnit
    open FsUnit.Xunit
    open Xunit
    open b0wter.FSharp

    type TestUnion
        = First
        | Second of int
        | Third of string

    [<Fact>]
    let ``Given an union type, returns its name``() =
        Union.caseName<@ First@> |> should equal <| Some "First"

    [<Fact>]
    let ``Given a non-union type, returns None``() =
        Union.caseName<@ int @> |> should equal None

    [<Fact>]
    let ``Given a tuple of union, returns all names``() =
        Union.caseName<@ First, Second, Third @> |> should equal <| Some "First, Second, Third"

    [<Fact>]
    let ``Given a tuple of union and non-union types, returns only union-type names``() =
        Union.caseName<@ int, First, string, string, Second, float, Third, double @> |> should equal <| Some "First, Second, Third"

module isCase =

    open FsUnit
    open FsUnit.Xunit
    open Xunit
    open b0wter.FSharp

    type TestUnion
        = First
        | Second of int
        | Third of string

    [<Fact>]
    let ``Given a (parameterless) union type of matching case returns true`` () =
        First |> Union.isCase<@ First @> |> should be True

    [<Fact>]
    let ``Given a (parameterized) union type of matching case returns true`` () =
        Second 10 |> Union.isCase<@ Second @> |> should be True

    [<Fact>]
    let ``Given a (parameterized) union type (without parameter) of matching case returns true`` () =
        (fun () -> Second |> Union.isCase<@ Second @> |> ignore) |> should (throwWithMessage "Value (not expression) is not a union case.") typeof<System.Exception>

    [<Fact>]
    let ``Given an union type of non-matching case fails the assertion`` () =
        Second 5 |> Union.isCase<@ First @> |> should be False

    [<Fact>]
    let ``Given a non-union type as expression throws an exception`` () =
        let value = Third
        (fun () -> value |> Union.isCase<@ int @> |> ignore) |> should (throwWithMessage "Expression (not value) is not a union case.") typeof<System.Exception>

    [<Fact>]
    let ``Given a non-union type as value argument fails the assertion`` () =
        (fun () -> 5 |> Union.isCase<@ Second 5 @> |> ignore) |> should (throwWithMessage "Value (not expression) is not a union case.") typeof<System.Exception>

    [<Fact>]
    let ``Given a tuple of union types containing the searched case returns true`` () =
        Second 5 |> Union.isCase<@ First, Second @> |> should be True
