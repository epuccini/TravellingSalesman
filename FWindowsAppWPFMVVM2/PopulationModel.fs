namespace GA.Salesman.Models

open System
open System.Collections.Generic
open GA.Salesman.Global
open GA.Salesman.Models
open GA.Salesman.List
open GA.Salesman.Array

type Population (tours : Tour list) =
    let _tours : List<Tour> = new List<Tour>(tours) 
    let mutable _current_tour : Tour = new Tour ()
    let _seedSize : int = 20000
    let _rand : Random = new System.Random(_seedSize)

    new () = new Population ( [] )
   
    member public this.CitySize 
        with get() = TILE_SIZE
    member public this.CurrentTour 
        with get() = _current_tour
    member public this.Tours
        with get() = _tours
    member public this.Rand
        with get() = _rand

    member public this.InitTours () =
        let first_tour : Tour = new Tour([])
        first_tour.InitCityList (_rand) |> ignore
        for i = 0 to TOUR_MAX do
            let tour : Tour = new Tour([])
            tour.CityList <- first_tour.CityList
            tour.ShuffleCityList (_rand) |> ignore
            tour.InitCoordinateList () |> ignore
            tour.InitComplexList () |> ignore
            _tours.Add tour
        _current_tour <- this.FittestTour ()

    member public this.DeleteTours () =
        _tours.Clear()

    member public this.FittestTour () =
        let mutable fittestIndex : int = 0
        let mutable fittestSum : float = (_tours.Item 0).DistanceSum ()
        let toursList = (List.genericTolist _tours) 
        let sumList = List.map (fun (acc : Tour) -> acc.DistanceSum()) toursList
        for i = 1 to ( _tours.Count - 1) do
            let tour : Tour = _tours.Item i
            let distanceSum = tour.DistanceSum()
            if distanceSum < fittestSum then
                fittestSum <- distanceSum
                fittestIndex <- i
        _tours.Item fittestIndex

    member public this.FittestTour (tours : Tour list) =
        let mutable fittestIndex : int = 0
        let mutable fittestSum : float = (List.getAt tours 0).DistanceSum ()
        let sumList = List.map (fun (acc : Tour) -> acc.DistanceSum()) tours
        for i = 1 to (List.length tours) - 1 do
            let tour : Tour = (List.getAt tours i)
            let distanceSum = tour.DistanceSum()
            if distanceSum < fittestSum then
                fittestSum <- distanceSum
                fittestIndex <- i
        tours.Item fittestIndex
      
    member public this.SetCurrentFittestTour () =
        _current_tour <- this.FittestTour ()
        _current_tour
    
    member private this.SetTours (lst : Tour list) =
        _tours.Clear ()
        for tour in lst do
            _tours.Add tour
        
    member public this.Randomize (lst : (float * City * City) list) rndFlag = 
        let size : int = List.length lst
        let mutable indicies : int list = [0 .. size]
        let originalList : (float * City * City) list = lst
        let mutable newList = lst
        let rnd : Random = Random size
        for i = 0 to size do
            let mutable rndNum : int = rnd.Next size
            while (List.indexOf indicies rndNum) >= 0 
            && (List.indexOf indicies i >= 0) 
            && i <> (List.indexOf indicies rndNum) 
            && indicies.Length > 0 do
                let idx1 : int = List.indexOf indicies i
                let idx2 : int = (List.indexOf indicies rndNum)
                let rs : Random = Random 0
                let r : int = int (rs.Next 5)
                // delete element
                indicies <- List.filter (fun acc -> 
                                            if acc <> idx1 && acc <> idx2 then 
                                                true 
                                            else false) indicies
                // next
                if idx1 >= 0 && idx2 >= 0 then
                    if rndFlag then
                        newList <- List.rswap (originalList, idx1, idx2, r)
                    else
                        newList <- List.rswap (originalList, idx1, idx2, 0)
                rndNum <- rnd.Next size
        newList
    
     member public this.Intersect (tour1 : Tour) (tour2 : Tour) =
        let newTour1 : Tour ref = ref (new Tour ([]))
        let newTour2 : Tour ref = ref (new Tour ([]))
        let rs = new System.Random()
        let a1 : _[] = List.toArray tour1.CityList
        let a2 : _[] = List.toArray tour2.CityList
        let lst1 : City list = Array.toList a1
        let lst2 : City list = Array.toList a2
        let mutable r1 : int = rs.Next (List.length tour1.CityList)
        let mutable r2 : int = rs.Next (List.length tour2.CityList)
        let mutable r3 : int = rs.Next (List.length tour1.CityList)
        let mutable r4 : int = rs.Next (List.length tour2.CityList)
        let mutable newCityList1 : City list = []
        let mutable newCityList2 : City list = []
        let mutable toBeRemoved1 : City list = []
        let mutable toBeRemoved2 : City list = []
        let createTours =
            if r2 < r1 then
                let temp : int = r2
                r2 <- r1
                r1 <- temp
            // create tour2
            for i = r2 downto r1 do
                toBeRemoved1 <- a1.[i] :: toBeRemoved1
            let targetList1 = toBeRemoved1
            let tour2Leftover : City list = List.filter (fun (acc : City) -> 
                                                if (List.exists (fun (element : City) -> 
                                                                    acc.Equals element) targetList1) = false then
                                                    true
                                                else
                                                    false) lst2
            newCityList2 <- tour2Leftover
            let toBeRemovedArray1 = List.toArray toBeRemoved1
            for i = 0 to (r2 - r1) do
                newCityList2 <- List.insertAt newCityList2 (r1 + i) toBeRemovedArray1.[i]
            // create tour1
            for i = r2 downto r1 do
                toBeRemoved2 <- a2.[i] :: toBeRemoved2
            let targetList2 = toBeRemoved2
            let tour1Leftover : City list = List.filter (fun (acc : City) -> 
                                                if (List.exists (fun (element : City) -> 
                                                                    acc.Equals element) targetList2) = false then
                                                    true
                                                else
                                                    false) lst1
            newCityList1 <- tour1Leftover
            let toBeRemovedArray2 = List.toArray toBeRemoved2
            for i = 0 to (r2 - r1) do
                newCityList1 <- List.insertAt newCityList1 (r1 + i) toBeRemovedArray2.[i]

            // precalculate and return
            (!newTour1).CityList <- newCityList1
            (!newTour2).CityList <- newCityList2
            (!newTour1).InitCoordinateList () |> ignore
            (!newTour2).InitCoordinateList () |> ignore
            (!newTour1).InitComplexList () |> ignore
            (!newTour2).InitComplexList () |> ignore
        (!newTour1, !newTour2)

     member private this.Mutate(tourList : Tour list) =
        let mutable newCandidateList : Tour list = tourList
        let rand = new System.Random()
        for tcnt = 0 to (List.length tourList) - 1 do
            let rate : int = rand.Next 100
            let tour : Tour = List.getAt tourList tcnt
            if (rate < MUTATION_RATE) then 
                for pos1 = 0 to (tour.CityList.Length - 1) do
                    let selector : int = rand.Next 5
                    let pos1 : int = rand.Next (tour.CityList.Length - 1)
                    let pos2 : int = rand.Next (tour.CityList.Length - 1)
                    let city1 : City = List.getAt tour.CityList pos1
                    let city2 : City = List.getAt tour.CityList pos2
                    tour.CityList <- List.setAt tour.CityList pos1 city2
                    tour.CityList <- List.setAt tour.CityList pos2 city1
                    newCandidateList <- List.setAt tourList tcnt tour
        newCandidateList

    member private this.Crossover (tourLeft : Tour) (tourRight : Tour) =
        let rand = new System.Random()
        let (crossoverTour1 : Tour, crossoverTour2 : Tour) = this.Intersect tourLeft tourRight
        (crossoverTour1, crossoverTour2)

    member private this.RemoveTour tours (tour : Tour) =
        List.filter (fun (acc : Tour) -> if acc <> tour then
                                                    true
                                                  else
                                                    false) tours

    member private this.RemoveTwoWorstTours (tourList : Tour list) =
        let sortedList : Tour list = List.sortBy (fun (acc : Tour) -> acc.DistanceSum()) tourList
        let mutable newList = []
        let listLength : int = List.length tourList
        for i = 0 to listLength - 3 do
            newList <- (List.getAt sortedList i) :: newList
        newList

    member private this.RemoveBadTours (tourList : Tour list) =
        let sortedList : Tour list = List.sortBy (fun (acc : Tour) -> acc.DistanceSum()) tourList
        let mutable newList = []
        let listLength : int = TOUR_MAX - 1
        for i = 0 to listLength do
            newList <- (List.getAt sortedList i) :: newList
        newList

    member private this.RemoveHalfTours (tourList : Tour list) =
        let sortedList : Tour list = List.sortBy (fun (acc : Tour) -> acc.DistanceSum()) tourList
        let mutable newList = []
        let listLength : int = ((List.length tourList) / 2) - 1
        for i = 0 to listLength do
            newList <- (List.getAt sortedList i) :: newList
        newList

    member private this.SelectCandidates (tourList : Tour list) (amount : int) (rs : System.Random) =
        let mutable candidateList : Tour list = []
        for i = 0 to amount - 1 do
            let r : int = rs.Next (0, amount)
            candidateList <- (List.getAt tourList r) :: candidateList
        candidateList
                
    member public this.Epoch(epochs : int) =
        // iterate steps
        let rs : Random = new System.Random()
        for e = 0 to epochs do
            // save fittest
            let mutable toursList = List.genericTolist _tours
            let fittest : Tour = this.FittestTour toursList

            // create random candidateList
            let mutable candidateList : Tour list = this.SelectCandidates toursList CANDIDATE_MAX rs

            // do crossover candidateList
            let mutable newCandidateList : Tour list = []
            for tour in candidateList do
                let (tour1, tour2) = this.Crossover fittest tour
                newCandidateList <- tour2 :: newCandidateList
                newCandidateList <- tour1 :: newCandidateList

            // mutate candidateList
            newCandidateList <- this.Mutate newCandidateList

            // append new candidateList
            for i = 0 to (List.length newCandidateList) - 1 do
                toursList <- (List.getAt newCandidateList i) :: toursList
            toursList <- this.RemoveBadTours toursList

            // set  tours
            this.SetTours toursList

            // restore fittest
            _tours.Add fittest

            // select fittest
            this.SetCurrentFittestTour() |> ignore

