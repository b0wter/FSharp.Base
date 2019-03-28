module Base.List

    let splitBy (matchLeft: bool) (p: 'a -> bool) (items: 'a list) =
        match items |> (List.tryFindIndex p) with
        | None ->
            (items, [])
        | Some i ->
            let length = (if matchLeft then i + 1 else i) 
            (items |> List.take length, items |> List.skip length)

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
        