namespace b0wter.FSharp.Collections
module Seq =
    
    let private toSeq (left: 'a list, right: 'a list) =
        (left |> Seq.ofList, right |> Seq.ofList)    
    
    /// <summary>
    /// Runs a function that takes a list and returns a list with seqs as parameters and return type.
    /// </summary>
    let runAsList (func: 'a list -> 'b list) (items: seq<'a>) : seq<'b> =
        (items |> List.ofSeq)
        |> func
        |> Seq.ofList
        
    /// <summary>
    /// Runs a function that takes two lists and returns a list with seqs as parameters and return type.
    /// </summary>
    let runAsList2 (func: 'a list -> 'b list -> 'c list) (xs: 'a seq) (ys: 'b seq) : 'c seq =
        func
            (xs |> List.ofSeq)
            (ys |> List.ofSeq)
        |> Seq.ofList
        
    /// <summary>
    /// Splits a seq by using a predicate. The left seq contains all elements up to the first element that matches the predicate.
    /// If <paramref name="matchLeft"/> is true the element will be added to the left seq otherwise to the right.
    /// </summary>
    let splitBy (matchLeft: bool) (p: 'a -> bool) (items: seq<'a>) =
        (items |> List.ofSeq) 
        |> (List.splitBy matchLeft p)
        |> toSeq
        
    /// <summary>
    /// Uses a generator as source of truth to check a seq for missing items.
    /// Returns all missing items and their should be indices.
    /// </summary>
    let missing (next: 'a -> 'a) (current: 'a) (equals: 'a -> 'a -> bool) (items: 'a seq) =
        runAsList (List.missing next current equals) items
        
    /// <summary>
    /// Maps items of a seq and runs a filter afterwards. The original item is returned for all mapped items that pass the filter.
    /// </summary>
    let mapFilter (transformation: 'a->'b) (predicate: 'b -> bool) (items: 'a seq) : 'a seq =
        runAsList (List.mapFilter transformation predicate) items
        
    /// <summary>
    /// Returns an seq of elements that appear in both arrays.
    /// </summary>
    let intersect (xs: 'a seq) (ys: 'a seq) =
        runAsList2 (List.intersect) xs ys
        
    /// <summary>
    /// Returns a seq of elements that appear only in the second array.
    /// </summary>
    /// <remarks>
    /// The second list is used for easier currying.
    /// </remarks>
    let differenceTo (xs: 'a seq) (ys: 'a seq) : 'a seq =
        runAsList2 (List.differenceTo) xs ys
        
    /// <summary>
    /// Returns a seq of elements that appear only in one of the two seqs.
    /// </summary>
    let symmetricDifference (xs: 'a seq) (ys: 'a seq) : 'a seq =
        runAsList2 (List.symmetricDifference) xs ys

    /// <summary>
    /// Returns a seq without the elements specified by the given predicate.
    /// </summary>
    let exceptBy (xs: 'a seq) (predicate: 'a -> bool) : 'a seq =
        runAsList (List.exceptBy predicate) xs
       