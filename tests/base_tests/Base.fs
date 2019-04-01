namespace BaseTests.Base

module ContainsNone =
    
    open Xunit
    open FsUnit.Xunit
    open b0wter.FSharp
    
    type TestCase = BaseTests.Helpers.TestData<int option, bool>
    
    let trueTestCases () : obj array seq =
        seq {
            yield [| { TestCase.items = [ None; Some 1; Some 2 ];           TestCase.expected = true } |]
            yield [| { TestCase.items = [ Some 1; Some 2; None ];           TestCase.expected = true } |]
            yield [| { TestCase.items = [ Some 1; None; Some 2 ];           TestCase.expected = true } |]
            yield [| { TestCase.items = ([ None; None ] : int option list); TestCase.expected = true } |]
        }
        
    let falseTestCases () : obj array seq =
        seq {
            yield [| [ Some 1; Some 2 ];         false |]
            yield [| [ ];                        false |]
        }
        
    [<Theory>]
    [<MemberData("trueTestCases")>]
    let ``Given lists with None-elements, should return true`` (data: TestCase) =
        data.items |> Base.containsNone |> should equal data.expected

module ContainsError =
    let a = "a"