namespace BaseTests.String

module IsNullOrWhitespace =

    open FsUnit
    open FsUnit.Xunit
    open Xunit
    open b0wter.FSharp

    let nonWhiteSpaceTestCases () : obj array seq =
        seq { 
            yield [| "aaa" |]
            yield [| "b" |]
            yield [| "c c" |]
            yield [| " d" |]
            yield [| " e " |]
            yield [| "f " |]
            yield [| "g g" |]
        }

    let whiteSpaceTestCases () : obj array seq =
        seq { 
            yield [| "" |]
            yield [| " " |]
            yield [| "\t" |]
            yield [| "\r" |]
            yield [| "\n " |]
            yield [| "\r\n " |]
        }

    [<Theory>]
    [<MemberData("nonWhiteSpaceTestCases")>]
    let ``Given a string containing non-whitespace characters, returns false.`` s =
        s |> String.isNullOrWhiteSpace |> should be False
        
    [<Theory>]
    [<MemberData("whiteSpaceTestCases")>]
    let ``Given a string containing only whitespace characters, returns true.`` s =
        s |> String.isNullOrWhiteSpace |> should be True

module Join =

    open FsUnit
    open FsUnit.Xunit
    open Xunit
    open b0wter.FSharp

    type TestData = {
        parts: string list
        delimiter: string
        result: string
    }

    let testCases () : obj array seq =
        seq {
            yield [| { parts = [ "a"; "b"; "c" ]; delimiter = ""; result = "abc" } |]
            yield [| { parts = [ "d"; "e"; "f" ]; delimiter = ","; result = "d,e,f" } |]
            yield [| { parts = [ "a"; ]; delimiter = ","; result = "a" } |]
            yield [| { parts = [ ]; delimiter = ","; result = "" } |]
        }

    [<Theory>]
    [<MemberData("testCases")>]
    let ``Given a list of strings and a delimiter, returns a single string containing all elements.`` data =
        (String.join data.delimiter data.parts) |> should equal data.result