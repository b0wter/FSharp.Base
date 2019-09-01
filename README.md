[![Build Status](https://b0wter.visualstudio.com/b0wter.FSharp/_apis/build/status/Run%20unit%20tests?branchName=master)](https://b0wter.visualstudio.com/b0wter.FSharp/_build/latest?definitionId=27&branchName=master)

# FSharp.Base
This projects contains code that I found myself writing more often than I wanted to. It contains a lot of extension methods for collections (lists, arrays, seq) and option types.
Please note that most of the `Array` and `Seq` methods are simply wrappers around the `list` version. I have not found the time to write unique implementations (yet).
There are tests for all functions (except trivial ones and the array/seq wrappers).

## Usage
To use the extension methods for `list`, `array` or `seq` simply add the following import:
```
open b0wter.FSharp.Collections
```
You are then able to call the new methods like any other `list` method:
```
list first, second = myItems |> List.half
```
Using the `option` extension methods works the same:
```
open b0wter.FSharp
```

## Custom Operators
I've defined some custom operators for tasks I found myself doing over and over. First, import:
```
open b0wter.FSharp.Operators
```
You'll then have access to:
```
|?  = Option.orElse
|?| = defaultArg
|=  = Option.compareIfSome
|=| = Option.compare
```

## Lense
This package contains a very simple implementation of [lenses](https://medium.com/@dtipson/functional-lenses-d1aba9e52254). I've not written it myself. It's taken from [here](https://medium.com/@devboy_org/generic-lenses-for-f-record-types-6aea9f4cba40).
I like this implementation because it's small and easy to use. However, due to its reliance on reflection it is slower than other options out there.

Here is how you use it:
```
type Human  = {Name:string}
and  Seat   = {Number:int; UsedBy:Human option}
and  Bus    = {DriverSeat:Seat}
and  Company= {Name:string;SchoolBus:Bus}

let BusDriver = {Human.Name="BusDriver A."}
let SchoolBus = {DriverSeat={Seat.Number=1;UsedBy=Some BusDriver}}
let BusCompany= {SchoolBus=SchoolBus; Name="EasyBus"}
    
do printfn "company: %A" BusCompany
// company: {Name = "EasyBus"; SchoolBus = {DriverSeat = {
//  Number = 1; UsedBy = Some {Name = "BusDriver A.";};};};}
do printfn "swapDriver: %A" <| Lens.With <@ BusCompany.SchoolBus.DriverSeat.UsedBy @> (Some {Human.Name="BusDriver Z."})
// swapDriver: {Name = "EasyBus"; SchoolBus = {DriverSeat = {
//  Number = 1; UsedBy = Some {Name = "BusDriver Z.";};};};}
do printfn "rmDriver: %A" <| Lens.With <@ BusCompany.SchoolBus.DriverSeat.UsedBy @> None
// rmDriver: {Name = "EasyBus"; SchoolBus = {DriverSeat = {
//  Number = 1; UsedBy = null;};};}
do printfn "mvSeat: %A" <| Lens.With <@ BusCompany.SchoolBus.DriverSeat.Number @> 12345
// mvSeat: {Name = "EasyBus"; SchoolBus = {DriverSeat = {
//  Number = 12345; UsedBy = Some {Name = "BusDriver A.";};};};}
