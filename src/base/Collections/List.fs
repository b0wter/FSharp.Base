namespace b0wter.FSharp.Collections
module List =
    open b0wter.FSharp
    open Base

    /// <summary>
    /// Splits a list by using a predicate. The left list contains all elements up to the first element that matches the predicate.
    /// If <paramref name="matchLeft"/> is true the element will be added to the left list otherwise to the right.
    /// </summary>
    let splitBy (matchLeft: bool) (p: 'a -> bool) (items: 'a list) =
        match items |> (List.tryFindIndex p) with
        | None ->
            (items, [])
        | Some i ->
            let length = (if matchLeft then i + 1 else i) 
            (items |> List.take length, items |> List.skip length)

    /// <summary>
    /// Uses a generator as source of truth to check a list for missing items.
    /// Returns all missing items and their should be indices.
    /// </summary>
    let missing (next: 'a -> 'a) (current: 'a) (equals: 'a -> 'a -> bool) (list: 'a list) =
        let indexed = list |> List.indexed
        let rec run (acc: (int * 'a) list) (current: 'a) (rest: (int * 'a) list) =
            match rest with
            | [] -> acc
            | head :: tail when head |> snd |> (equals current) ->
                run acc (current |> next) tail
            | (index, _) :: tail ->
                run ((index, current) :: acc) (current |> next |> next) tail
                
        indexed |> (run [] current) |> List.rev
        
    /// <summary>
    /// Maps items of a list and runs a filter afterwards. The original item is returned for all mapped items that passed the filter.
    /// </summary>
    let mapFilter (transformation: 'a->'b) (predicate: 'b -> bool) (items: 'a list) : 'a list =
        items 
        |> List.map (fun item -> (item, item |> transformation ))
        |> List.filter (fun (_, mappedItem) -> mappedItem |> predicate)
        |> List.map fst
        
    /// <summary>
    /// Returns a list of elements that appear in both lists.
    /// </summary>
    let intersect (xs: list< 'a >) (ys: list< 'a >) =
        let hashes = System.Collections.Generic.HashSet< 'a >(ys, HashIdentity.Structural)
        xs |> List.filter (fun x -> hashes.Contains x)
        
    /// <summary>
    /// Returns a list of elements that appear only in the second list.
    /// </summary>
    /// <remarks>
    /// The second list is used for easier currying.
    /// </remarks>
    let differenceTo (xs: list<'a>) (ys: list<'a>) =
        let hashes = System.Collections.Generic.HashSet< 'a >(xs, HashIdentity.Structural)
        ys |> List.filter (fun y -> not (hashes.Contains y))
        
    /// <summary>
    /// Returns a list of elements that appear only in one of the two lists.
    /// </summary>
    let symmetricDifference (xs: list<'a>) (ys: list<'a>) =
        let deltaX = xs |> differenceTo ys
        let deltaY = ys |> differenceTo xs
        deltaY @ deltaX
        
    /// <summary>
    /// Returns a list without the elements specified by the given predicate.
    /// </summary>
    let exceptBy (predicate: 'a -> bool) (xs: list<'a>) =
        let toRemove = xs |> List.filter predicate
        if toRemove |> List.isEmpty then
            xs
        else
            xs |> List.except toRemove
           
    ///<sumary>
    /// Splits a list into n parts of equal length. If the number of elements is not divisable by n the lists will
    /// be filled starting from the first list. E.g. [1; 2; 3] split into two yields: [1; 2], [ 3 ].
    /// Returns empty lists if n is larger than the length of the list of items. E.g. [1; 2] divided into three
    /// yields: [ 1 ]; [ 2 ]; [ ].
    /// </summary> 
    let inParts (n: int) (xs: 'a list) : 'a list list =
        let partLenghts = System.Math.Ceiling((xs.Length |> float) / (n |> float)) |> int
        
        let take (elements: 'a list) (amount: int) : ('a list * 'a list) =
            let rec take (elements: 'a list) (acc: 'a list) (amount: int) : ('a list * 'a list) =
                match elements with
                | [] -> (acc, [])
                | head :: tail when amount > 0 -> take (tail) (head :: acc) (amount - 1)
                | _ when amount = 0 -> (acc, elements)
                | _ -> failwith "Reached invalid state while in `inParts`."
            let (left, right) = take elements [] amount
            (left |> List.rev, right)
            
        [1..n] |> List.fold (fun (rest: 'a list, acc: 'a list list) _ ->
                                    let (taken, tail) = take rest partLenghts
                                    (tail, taken :: acc)
                            )
                            (xs, ([]: 'a list list))
               |> snd
               |> List.rev
        
    /// <summary>
    /// Returns the list split into to equal parts.
    /// If the list has an uneven number of elements the left tuple element will be one element larger.
    /// </summary>
    let half (xs: list<'a>) =
        let length = xs.Length
        let left = if length |> isEven then length / 2 else length / 2 + 1
        (xs |> List.take left, xs |> List.skip left)

    /// <summary>
    /// Casts list of objects to any other type. Return objects if cast is possible, so new list can be shorter (or empty).
    /// </summary>
    /// <remarks>
    /// Copied from: http://www.fssnip.net/oD/title/Cast-object-list
    /// </remarks>
    let rec cast<'a> (myList: obj list) =          
        match myList with
        | head::tail -> 
            match head with 
            | :? 'a as a -> a::(cast tail) 
            | _ -> cast tail
        | [] -> [] 
        
    /// <summary>
    /// Replaces the 'old' object with the 'updated' object. Note that the 'updated' element will be
    /// at the same position as the 'old' object.
    /// </summary>
    let replace (old: 'a) (updated: 'a) (items: 'a list) =
        items |> List.map (fun i -> if i = old then updated else i)


    /// <summary>
    /// Removes an item from a list using a predicate.
    /// Does nothing if the item does not exist within the list.
    /// </summary>
    let removeBy (predicate: 'a -> bool) (items: 'a list) =
        let rec run (acc: 'a list) (rest: 'a list) =
            match rest with
            | head :: tail when head |> predicate -> (acc |> List.rev) @ tail
            | head :: tail -> run (head :: acc) tail
            | [ ] -> items
        run [] items
        
    /// <summary>
    /// Removes a given item from a list.
    /// Does nothing if the item does not exist within the list.
    /// </summary>
    let remove (item: 'a) =
        removeBy ((=) item)
