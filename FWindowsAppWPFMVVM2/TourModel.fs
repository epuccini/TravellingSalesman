namespace GA.Salesman.Models

open GA.Salesman.Global
open System
open System.Collections.Generic
open GA.Salesman.List

type Coordinate (x : float, y : float) =
    let mutable _x : float = x
    let mutable _y : float = y
    new (a, b) = new Coordinate(a,b)
    member public this.X 
        with get () = _x
        and set (value) = _x <- value
    member public this.Y 
        with get () = _y
        and set (value) = _y <- value

type Tour (cities : City list) =
    let _seedValue : int = 1000
    let _coordinateList : List<(float * float)> = new List<(float * float)>()
    let mutable _complexList : List<((float * float) * City)> = new List<((float * float) * City)>()
    let mutable _cityList : City list = [] 
    let mutable _distance : float = 0.0

    new () = new Tour( [] )

    member public this.CitySize 
        with get() = CITY_SIZE
    member public this.CoordinateList 
        with get() = _coordinateList      
    member public this.ComplexList 
        with get() = _complexList      
    member public this.CityList 
        with get() = _cityList
        and set(value : City list) = _cityList <- value

    member public this.InitCoordinateList () =
        _coordinateList.Clear ()
        List.map (fun (acc : City) ->
                        _coordinateList.Add acc.Coordinates
                        acc.Coordinates) _cityList |> ignore
        _coordinateList
    member public this.InitComplexList () =
        _complexList.Clear ()
        List.map (fun (acc : City) ->
                        _complexList.Add (acc.Coordinates, acc)
                        (acc.Coordinates, acc)) _cityList |> ignore
        _complexList
    member public this.InitCityList (rand : System.Random) = 
        let indicies : int list = [ 0 .. CITY_MAX]
        _cityList <- List.map (fun acc ->
                    let rx : int = rand.Next (0, _seedValue)          
                    let ry : int = rand.Next (0, _seedValue)
                    new City((float rx), (float ry))
            ) indicies
        _cityList
    member public this.ShuffleCityList (rand : System.Random) =
        _cityList <- List.shuffleRand _cityList rand
        _cityList
    member public this.RemoveCoordinate (item : (float * float)) =
        _coordinateList.Remove item |> ignore
        _coordinateList
    member public this.RemoveComplex (item : ((float * float) * City)) =
        _complexList.Remove item |> ignore
        _complexList
    member public this.DistanceTo (x1, y1) (x2, y2) =
        let distance : float = sqrt ( (x2 - x1) ** 2.0 + (y2 + y1) ** 2.0 )
        distance
    member public this.CreateDistanceList () =
        let coordList : List<(float * float)> = this.InitCoordinateList ()
        let initCoordList = List.map (fun (acc : City) -> acc.Coordinates) _cityList
        let firstCity : City = List.getAt _cityList 0 
        let (firstX, firstY) = firstCity.Coordinates
        let lastCity : City = List.lastElement _cityList
        let (lastX, lastY) = lastCity.Coordinates
        let distanceList = List.map (fun (accX, accY) ->  
                                        if accX <> lastX || accY <> lastY then
                                            let (nextX, nextY) = coordList.Item 1
                                            this.RemoveCoordinate (coordList.Item 0) |> ignore
                                            this.DistanceTo (accX, accY) (nextX, nextY)   
                                        else 
                                            this.RemoveCoordinate (coordList.Item 0) |> ignore
                                            this.DistanceTo (accX, accY) (firstX, firstY) )
                                    initCoordList    
        distanceList

    member public this.CreateComplexList () =
        let workComplexList = this.InitComplexList ()
        let initComplexList = List.map (fun (acc : City) -> (acc.Coordinates, acc)) _cityList
        let firstCity : City = List.getAt _cityList 0 
        let (firstX, firstY) = firstCity.Coordinates
        let lastCity : City = List.lastElement _cityList
        let (lastX, lastY) = lastCity.Coordinates
        let complexList = List.map (fun ((accX, accY), currentCity) ->  
                                        if accX <> lastX || accY <> lastY then
                                            let ((nextX, nextY), nextCity) = workComplexList.Item 1
                                            this.RemoveComplex (workComplexList.Item 0) |> ignore
                                            (this.DistanceTo (accX, accY) (nextX, nextY), currentCity, nextCity)   
                                        else 
                                            this.RemoveComplex (workComplexList.Item 0) |> ignore
                                            (this.DistanceTo (lastX, lastY) (firstX, firstY), lastCity, firstCity) )  
                                    initComplexList    
        complexList
            
    member public this.DistanceSum () =  
        let distanceList = this.CreateDistanceList ()
        let distanceSum : float = List.reduce (+) distanceList
        _distance <- distanceSum
        distanceSum

    member public this.Fitness distance : float =
        let fitness : float = 1.0 / distance
        fitness
            
 