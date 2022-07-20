namespace GA.Salesman.Models

open System
open System.Collections.Generic

type City (x : float, y : float) =
    new () = new City (0.0, 0.0)

    member val X = x with get, set
    member val Y = y with get, set

    member val Coordinates = (x, y) with get, set

    member this.DistanceTo (toCity : City) =
        let dist : float = sqrt ( (this.X + this.Y) ** 2.0 + (toCity.X + toCity.Y) ** 2.0 )
        dist

    interface IComparable<City> with
        member this.CompareTo (city : City) =
            compare (this.X, this.Y) (city.X, city.Y)
    interface IComparable with
            member this.CompareTo obj =
                match obj with
                | :? City as other -> (this :> IComparable<_>).CompareTo other
                | _                    -> invalidArg "obj" "not a Category"
    interface IEquatable<City> with
        member this.Equals (city : City) =
            (this.X = city.X && this.Y = city.Y)

    member this.EqualsNot (city : City) =
        (this.X <> city.X || this.Y <> city.Y)

    override this.Equals obj =
            match obj with
            | :? City as other -> (this :> IEquatable<_>).Equals other

    override this.GetHashCode () =
        hash (this.X, this.Y)