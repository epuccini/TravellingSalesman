namespace GA.Salesman.List

open GA.Salesman.Global
open GA.Salesman.Array

module List =
    open System
    open System.Collections.Generic

    let genericTolist g =
        let mutable lst = []
        for element in g do
            lst <- element :: lst
        (List.rev lst)

    let listTogeneric l =
        let g : List<'T> = new List<'T>()
        for element in l do
            g.Add element
        g

    let shuffle lst =
        let a : _[] = List.toArray lst
        let rand = new System.Random()
        Array.iteri (fun i _ -> Array.swap a i (rand.Next(i, Array.length a))) a
        Array.toList a

    let shuffleRand lst (rand : Random) =
        let a : _[] = List.toArray lst
        Array.iteri (fun i _ -> Array.swap a i (rand.Next(i, Array.length a))) a
        Array.toList a

    let rec removeItem (lst : 'a list) item =
        match item, lst with
        | 0, x::xs -> xs
        | i, x::xs -> if x = item then xs else removeItem xs item
        | i, [] -> failwith "Argument out of range." 

    let lastElement ls = List.reduce (fun _ i -> i) ls
    
    let getAt (lst : 'a list) idx =
        let rec loop (lst : 'a list) cnt =
            match lst with
            | head :: tail -> if cnt <> idx then
                                 loop tail (cnt + 1)
                              else
                                 head
            | [] -> new 'a ()
        loop lst 0

    let getTupleAt (lst : 'a list) idx =
        let rec loop (lst : 'a list) cnt =
            match lst with
            | head :: tail -> if cnt <> idx then
                                 loop tail (cnt + 1)
                              else
                                 head
        loop lst 0

    let add lst e =
        let newList = e :: List.rev lst
        List.rev newList

    let setAt (lst : 'a list) idx e =
        let tempList : 'a list ref = ref []
        let rec loop innerList cnt =
            let currentIdx : int = List.length lst - cnt - 1
            match innerList with
            | head :: tail -> if ((List.length lst) - cnt - 1) <> idx then
                                tempList := head :: !tempList
                                loop tail (cnt + 1)
                              else
                                tempList := (e :: !tempList)
                                loop tail (cnt + 1)
            | [] -> !tempList
        loop (List.rev lst) 0 

    let insertAt (lst : 'a list) idx e =
        let tempList : 'a list ref = ref []
        let rec loop innerList cnt =
            let currentIdx : int = List.length lst - cnt - 1
            match innerList with
            | head :: tail -> if currentIdx <> idx then
                                tempList := head :: !tempList
                                loop tail (cnt + 1)
                              else
                                tempList := head :: !tempList
                                tempList := (e :: !tempList)
                                loop tail (cnt + 1)
            | [] -> if idx = List.length lst then
                        List.rev  (e :: List.rev !tempList)
                    else
                        !tempList
        loop (List.rev lst) 0
        
    let removeAt (lst : 'a list) idx =
        let tempList : 'a list ref = ref []
        let rec loop innerList cnt =
            let currentIdx : int = List.length lst - cnt - 1
            match innerList with
            | head :: tail -> if ((List.length lst) - cnt - 1) <> idx then
                                tempList := head :: !tempList
                                loop tail (cnt + 1)
                              else
                                loop tail (cnt + 1)
            | [] -> !tempList
        loop (List.rev lst) 0

    let indexOf lst elem =
        try
            let index = Array.findIndex (fun acc -> if acc = elem then true else false) <| List.toArray lst
            index
        with 
            | :? System.Collections.Generic.KeyNotFoundException -> -1
            | :? System.ArgumentOutOfRangeException as ex -> failwith ex.Message
            | :? System.Exception as ex -> failwith ex.Message
    
    let rswap (lst : 'a list , i : int, j : int, r : int) =
        let mutable op1 = getTupleAt lst i
        let mutable op2 = getTupleAt lst j
        let (distance1, city11, city12) = op1
        let (distance2, city21, city22) = op2
        match r with
            | 0 -> op1 <- getTupleAt lst j
                   op2 <- getTupleAt lst i

            | 1 -> op1 <- (distance1, city11, city22)
                   op2 <- (distance2, city12, city21)

            | 2 -> op1 <- (distance1, city11, city21)
                   op2 <- (distance2, city12, city22)

            | 3 -> op1 <- (distance2, city11, city12)
                   op2 <- (distance1, city21, city22)            

            | 4 -> op1 <- (distance2, city12, city11)
                   op2 <- (distance1, city22, city21)            

            | n -> op1 <- (distance2, city11, city21)
                   op2 <- (distance1, city12, city22)     

        setAt lst i op1 |> ignore
        setAt lst j op2 |> ignore
        lst

