namespace b0wter.FSharp.Collections
module List =

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
    let exceptBy (predicate: 'a -> bool) (xs: list<'a>)=
        let toRemove = xs |> List.filter predicate
        if toRemove |> List.isEmpty then
            xs
        else
            xs |> List.except toRemove
            