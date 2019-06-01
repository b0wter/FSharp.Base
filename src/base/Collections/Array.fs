namespace b0wter.FSharp.Collections
module Array =
    
    let private toArray (left: 'a list, right: 'a list) =
        (left |> Array.ofList, right |> Array.ofList)
        
    /// <summary>
    /// Runs a function that takes a list and returns a list with arrays as parameters and return type.
    /// </summary>
    let runAsList (func: 'a list -> 'b list) (items: 'a []) =
        (items |> List.ofArray)
        |> func
        |> Array.ofList
        
    /// <summary>
    /// Runs a function that takes two lists and returns a list with arrays as parameters and return type.
    /// </summary>
    let runAsList2 (func: 'a list -> 'b list -> 'c list) (xs: 'a array) (ys: 'b array) : 'c [] =
        func
            (xs |> List.ofArray)
            (ys |> List.ofArray)
        |> Array.ofList
    
    /// <summary>
    /// Splits a list by using a predicate. The left list contains all elements up to the first element that matches the predicate.
    /// If <paramref name="matchLeft"/> is true the element will be added to the left list otherwise to the right.
    /// </summary>
    let splitBy (matchLeft: bool) (p: 'a -> bool) (items: 'a []) =
        (items |> List.ofArray) 
        |> (List.splitBy matchLeft p)
        |> toArray
        
    /// <summary>
    /// Uses a generator as source of truth to check a list for missing items.
    /// Returns all missing items and their should be indices.
    /// </summary>
    let missing (next: 'a -> 'a) (current: 'a) (equals: 'a -> 'a -> bool) (items: 'a[]) =
        runAsList (List.missing next current equals) items
        
    /// <summary>
    /// Maps items of a list and runs a filter afterwards. The original item is returned for all mapped items that passed the filter.
    /// </summary>
    let mapFilter (transformation: 'a->'b) (predicate: 'b -> bool) (items: 'a []) : 'a[] =
        runAsList (List.mapFilter transformation predicate) items
        
    /// <summary>
    /// Returns an array of elements that appear in both arrays.
    /// </summary>
    let intersect (xs: 'a []) (ys: 'a[]) =
        runAsList2 (List.intersect) xs ys
        
    /// <summary>
    /// Returns an array of elements that appear only in the second array.
    /// </summary>
    /// <remarks>
    /// The second list is used for easier currying.
    /// </remarks>
    let differenceTo (xs: 'a []) (ys: 'a[]) : 'a[] =
        runAsList2 (List.differenceTo) xs ys
        
    /// <summary>
    /// Returns a list of elements that appear only in one of the two lists.
    /// </summary>
    let symmetricDifference (xs: 'a[]) (ys: 'a[]): 'a [] =
        runAsList2 (List.symmetricDifference) xs ys

    /// <summary>
    /// Returns a list without the elements specified by the given predicate.
    /// </summary>
    let exceptBy (predicate: 'a -> bool) (xs: 'a[]) : 'a[] =
        runAsList (List.exceptBy predicate) xs

    /// <summary>
    /// Returns the array split into to equal parts.
    /// If the array has an uneven number of elements the left tuple element will be one element larger.
    /// </summary>
    let half (xs: 'a[]) : ('a[] * 'a[]) =
        xs |> List.ofArray |> List.half |> toArray