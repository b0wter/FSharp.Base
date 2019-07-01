namespace b0wter.FSharp
//
// The code is taken from:
// https://medium.com/@devboy_org/generic-lenses-for-f-record-types-6aea9f4cba40
//
(* USAGE:

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
*)

module Lens =
        
    open FSharp.Quotations
    open FSharp.Quotations.Patterns
    open FSharp.Quotations.Evaluator
    open Microsoft.FSharp.Reflection
    open System.Reflection

    let private eval = QuotationEvaluator.EvaluateUntyped

    module internal Record =

        let Fields = FSharpType.GetRecordFields
        let Type (p:PropertyInfo) = p.DeclaringType
        let ValuesOr p v o = 
            Type p |> Fields 
            |> Array.map (fun x -> if x = p then v else x.GetValue(o))
        let Make t xs = FSharpValue.MakeRecord(t,xs,false)
        let With p v o = ValuesOr p v o |> Make (Type p)
        let rec Update<'a,'b> = function
            | PropertyGet(None,_,[]),v -> v
            | PropertyGet(Some(PropertyGet(_) as pg),p,[]),v -> 
                Update(pg,(With p v (eval pg)))
            | _ -> failwith "blaargh"

    let With (e:Expr<'a>) (v:'a) = Record.Update(e.Raw,v) :?> 'b

