namespace BaseTests.ListTests

module Missing =
    open FsUnit
    open FsUnit.Xunit
    open Xunit
    open b0wter.FSharp.Collections

    let private next (s: string) = 
        let nexts = [ "A", "B";
                      "B", "C";
                      "C", "D";
                      "D", "A" ] |> Map.ofList
        if nexts |> Map.containsKey s then nexts.[s] else nexts.["A"]

    [<Theory>]
    [<InlineData(1, "B")>]
    [<InlineData(2, "C")>]
    let ``When missing an element in the middle returns that element`` (exclude: (int * string)) =
        let items = [ "A"; "B"; "C"; "D" ] |> List.except [ exclude |> snd ]
        items |> (List.missing next items.Head (=)) |> (should equal [ exclude ])

    [<Fact>]
    let ``When missing the first element returns that element`` () =
        let items = [ "B"; "C"; "D" ]
        items |> (List.missing next "A" (=)) |> (should equal [(0, "A")])

    [<Fact>]
    let ``When the last element is wrong returns missing element`` () =
        let items = [ "A"; "B"; "C"; "A" ]
        items |> (List.missing next "A" (=)) |> (should equal [(3, "D")])

    [<Fact>]
    let ``When missing multiple elements returns all`` () =
        let items = [ "A"; "C"; "A" ] 
        items |> (List.missing next "A" (=)) |> (should equal [ (1, "B"); (2, "D") ])

    [<Fact>]
    let ``When run on empty list, returns an empty list`` () =
        let items = []
        items |> (List.missing next "A" (=)) |> (should be Empty)

    [<Fact>]
    let ``When run on a valid list, returns an empty list`` () =
        let items = [ "A"; "B"; "C"; "D"; "A" ]
        items |> (List.missing next "A" (=)) |> (should be Empty)
    
module SplitBy =

    open FsUnit
    open FsUnit.Xunit
    open Xunit
    open b0wter.FSharp.Collections

    type TestData<'a> = {
        input: 'a list
        keep: bool
        pick: 'a
        expectedLeft: 'a list
        expectedRight: 'a list
    }
        
    let testCases () : obj array seq =
        let items = [ "A"; "B"; "C"; "D" ]
        seq {
            yield [| { input = items; keep = false; pick = "A"; expectedLeft = [ ];                    expectedRight = [ "A"; "B"; "C"; "D" ] } |]
            yield [| { input = items; keep = true;  pick = "A"; expectedLeft = [ "A" ];                expectedRight = [ "B"; "C"; "D" ]      } |]
            yield [| { input = items; keep = false; pick = "B"; expectedLeft = [ "A" ];                expectedRight = [ "B"; "C"; "D" ]      } |]
            yield [| { input = items; keep = true;  pick = "B"; expectedLeft = [ "A"; "B" ];           expectedRight = [ "C"; "D" ]           } |]
            yield [| { input = items; keep = false; pick = "C"; expectedLeft = [ "A"; "B" ];           expectedRight = [ "C"; "D" ]           } |]
            yield [| { input = items; keep = true;  pick = "C"; expectedLeft = [ "A"; "B"; "C" ];      expectedRight = [ "D" ]                } |]
            yield [| { input = items; keep = false; pick = "D"; expectedLeft = [ "A"; "B"; "C" ];      expectedRight = [ "D" ]                } |]
            yield [| { input = items; keep = true;  pick = "D"; expectedLeft = [ "A"; "B"; "C"; "D" ]; expectedRight = [ ]                    } |]
        }
        
    [<Theory>]
    [<MemberData("testCases")>]
    let ``Given correct data, list should be split accordingly`` (data: TestData<string>) =
        let (left, right) = data.input |> (List.splitBy data.keep ((=) data.pick))
        left  |> (should equal data.expectedLeft)
        right |> (should equal data.expectedRight)

module MappedFilter = 

    open FsUnit
    open FsUnit.Xunit
    open Xunit
    open b0wter.FSharp.Collections
    
    [<Fact>]
    let ``Given a list and a valid predicate, returns the matching element as a list.`` () =
        let source = [ 1; 2; 3; 4; ]
        let mapping = fun (x: int) -> System.Math.Pow((float)x, 2.0)
        let target = 16.0
        let predicate = ((=) target)
        
        let result = source |> (List.mapFilter mapping predicate)
        result |> should equal [ 4 ]
        
    [<Fact>]
    let ``Given a list and a non-matching predicate, returns empty list.`` () =
        let source = [ 1; 2; 3; ]
        let mapping = fun (x: int) -> System.Math.Pow((float)x, 2.0)
        let target = 16.0
        let predicate = ((=) target)
        
        let result = source |> (List.mapFilter mapping predicate)
        result |> should be Empty
       
module Intersect =

    open FsUnit
    open FsUnit.Xunit
    open Xunit
    open b0wter.FSharp.Collections
    
    type TestCase = BaseTests.Helpers.TestData<string, string, string>
    
    let testCases () : obj array seq =
        seq {
            yield [| ["a"; "b"]; ["b"; "c"]; ["b"] |]
            yield [| ["a"; "b"]; ["a"; "c"]; ["a"] |]
            yield [| ([]: string list); ["a"; "c"]; ([]: string list) |]
            yield [| ["a"; "b"]; ["a"; "b"]; ["a"; "b"] |]
            yield [| ["a"; "b"]; ["c"; "d"]; ([]: string list) |]
        }
    
    [<Theory>]
    [<MemberData("testCases")>]
    let ``Given two lists returns all elements appearing in both lists.`` (left: string list) (right: string list) (expected: string list) =
        left |> List.intersect right |> should equal expected
    
module DifferenceTo =

    open FsUnit
    open FsUnit.Xunit
    open Xunit
    open b0wter.FSharp.Collections
    
    type TestCase = BaseTests.Helpers.TestData<string, string, string>
    
    let testCases () : obj array seq =
        seq {
            yield [| ["a"; "b"]; ["b"; "c"]; ["a"] |]
            yield [| ["a"; "b"]; ["a"; "c"]; ["b"] |]
            yield [| ([]: string list); ["a"; "c"]; ([]: string list) |]
            yield [| ["a"; "b"]; ["a"; "b"]; ([]: string list) |]
            yield [| ["a"; "b"]; ["c"; "d"]; ["a"; "b"] |]
        }
    
    [<Theory>]
    [<MemberData("testCases")>]
    let ``Given two lists returns all elements appearing only in the first list.`` (left: string list) (right: string list) (expected: string list) =
        left |> List.differenceTo right |> should equal expected
    
module SymmetricalDifference =

    open FsUnit
    open FsUnit.Xunit
    open Xunit
    open b0wter.FSharp.Collections
    
    type TestCase = BaseTests.Helpers.TestData<string, string, string>
    
    let testCases () : obj array seq =
        seq {
            yield [| ["a"; "b"]; ["b"; "c"]; ["a"; "c"] |]
            yield [| ["a"; "b"]; ["a"; "c"]; ["b"; "c"] |]
            yield [| ([]: string list); ["a"; "c"]; ["a"; "c"] |]
            yield [| ["a"; "b"]; ["a"; "b"]; ([]: string list) |]
            yield [| ["a"; "b"]; ["c"; "d"]; ["a"; "b"; "c"; "d"] |]
        }
    
    [<Theory>]
    [<MemberData("testCases")>]
    let ``Given two lists returns all elements appearing only in one list.`` (left: string list) (right: string list) (expected: string list) =
        left |> List.symmetricDifference right |> should equal expected

module ExceptBy =
    
    open FsUnit
    open FsUnit.Xunit
    open Xunit
    open b0wter.FSharp.Collections
    
    type TestCase = BaseTests.Helpers.TestData<string, string, string>
    
    let testCases () : obj array seq =
        seq {
            yield [| ["a"; "b"; "c"]; (fun (s: string) -> s = "c"); ["a"; "b"] |]
            yield [| ["a"; "b"; "c"; "c"]; (fun (s: string) -> s = "c"); ["a"; "b"] |]
            yield [| ["a"; "b"]; (fun (s: string) -> s = "<not in list>"); ["a"; "b"] |]
            yield [| ([]: string list); (fun (s: string) -> s = "<not in list>"); ([]: string list) |]
        }
    
    [<Theory>]
    [<MemberData("testCases")>]
    let ``Given a list and a predicate returns all elements not matching the predicate.`` (items: string list) (predicate: string -> bool) (expected: string list) =
        items |> List.exceptBy predicate |> should equal expected

module Half =
    
    open FsUnit
    open FsUnit.Xunit
    open FsUnitTyped
    open Xunit
    open b0wter.FSharp.Collections
    
    [<Fact>]
    let ``Given a list with even length, returns the first half on the left side and the second half on the right`` () =
        let input = [ 1; 2; 3; 4; 5; 6 ]
        let left = [ 1; 2; 3 ]
        let right = [ 4; 5; 6 ]
        
        let (resultL, resultR) = input |> List.half
        
        resultL |> should equal left
        resultR |> should equal right
        
    [<Fact>]
    let ``Given a list with odd length, left side contains one more element than the right side`` () =
        let input = [ 0; 1; 2; 3; 4; 5; 6; ]
        let left = [ 0; 1; 2; 3 ]
        let right = [ 4; 5; 6 ]
        
        let (resultL, resultR) = input |> List.half
        
        resultL |> should equal left
        resultR |> should equal right

    [<Fact>]
    let ``Given a list with one element, returns a single element on the left side and an empty list on the right side`` () =
        let input = [ 1 ]
        let left = [ 1 ]
        
        let (resultL, resultR) = input |> List.half
        
        resultL |> should equal left
        resultR |> should be Empty
    
    [<Fact>]
    let ``Given an empty list, returns an empty list for the left and right side`` () =
        let input = []
        
        let (resultL, resultR) = input |> List.half
        
        resultL |> should be Empty
        resultR |> should be Empty
