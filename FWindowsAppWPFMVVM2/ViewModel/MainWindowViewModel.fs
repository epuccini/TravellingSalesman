namespace GA.Salesman.ViewModel

open System
open System.Collections.ObjectModel
open System.Windows
open System.Windows.Data
open System.Windows.Input
open System.ComponentModel
open System.Windows
open System.Windows.Controls
open System.Windows.Documents
open System.Windows.Input
open System.Windows.Media
open System.Windows.Shapes

open GA.Salesman.Global
open GA.Salesman.Models
open GA.Salesman.List

type MainWindowViewModel (window : Window, population : Population) =
    inherit ViewModelBase()
    
    let (?) (window : Window) name = window.FindName name |> unbox 

    let _population = population
    let _window : Window = window
    
    member public this.InitControls () =
        _population.InitTours ()
        _population.SetCurrentFittestTour () |> ignore

        let drawingCanvas : Canvas = _window?DrawingCanvas
        this.DrawCities drawingCanvas
        let distanceStart : float = _population.CurrentTour.DistanceSum ()
        this.SetupLabels (distanceStart, 0.0)
        let startButton : Button = _window?startButton
        let resetButton : Button = _window?resetButton
        resetButton.Opacity <- 0.5
        startButton.Click.Add (fun x -> do
                                    startButton.Opacity <- 0.5
                                    resetButton.Opacity <- 1.0
                                    let distanceNewStart : float = _population.CurrentTour.DistanceSum ()

                                    _population.Epoch EPOCH_MAX

                                    let distanceNewEnd: float = _population.CurrentTour.DistanceSum ()
                                    this.SetupLabels (distanceNewStart, distanceNewEnd)
                                    this.Update ())
        resetButton.Click.Add (fun x -> do
                                    startButton.Opacity <- 1.0
                                    resetButton.Opacity <- 0.5
                                    _population.DeleteTours ()
                                    _population.InitTours ()
                                    let distanceNewStart : float = _population.CurrentTour.DistanceSum ()
                                    this.SetupLabels (distanceNewStart, 0.0)
                                    this.Update ())
        0

    member public this.DrawBackground (canvas : Canvas) =
        let square = Shapes.Rectangle ()
        square.Width <- canvas.Height
        square.Height <- canvas.Height
        square.Stroke <- SolidColorBrush(Colors.LightGray)
        square.Fill <- SolidColorBrush(Colors.LightGray)
        Canvas.SetLeft (square, 0.0)
        Canvas.SetTop (square, 0.0)
        canvas.Children.Add(square) 

    member public this.DrawConnections (canvas : Canvas) (x1 : float) (y1 : float) (x2 : float) (y2 : float) =
        let line = Shapes.Line ()
        line.X1 <- x1
        line.X2 <- x2
        line.Y1 <- y1
        line.Y2 <- y2
        line.Stroke <- SolidColorBrush(Colors.Red)
        line.StrokeThickness <- 2.0
        canvas.Children.Add(line)

    member public this.DrawCity (canvas : Canvas) (x : float) (y : float) =
        let square = Shapes.Rectangle ()
        square.Width <- float TILE_SIZE
        square.Height <- float TILE_SIZE
        square.Stroke <- SolidColorBrush(Colors.Black)
        square.Fill <- SolidColorBrush(Colors.Black)
        Canvas.SetLeft (square, x - (float (TILE_SIZE / 2)))
        Canvas.SetTop (square, y - (float (TILE_SIZE / 2)))
        canvas.Children.Add(square) 

    member public this.SetupLabels (distanceStart : float, distanceEnd : float) =
        let distanceLabelStart : Label = _window?distanceLabelStart
        let distanceLabelEnd : Label = _window?distanceLabelEnd
        let labelTextStart = (sprintf "Start-Distance %f" distanceStart)
        let labelTextEnd = (sprintf "End-Distance %f" distanceEnd)
        distanceLabelStart.Content <- labelTextStart
        distanceLabelEnd.Content <- labelTextEnd

    member public this.Update () =
        let drawingCanvas : Canvas = _window?DrawingCanvas
        this.DrawCities drawingCanvas

    member public this.DrawCities (drawingCanvas : Canvas) =
        let cityList : City list = _population.CurrentTour.CityList
        let (lastX : float, lastY : float) = (List.lastElement cityList.Tail).Coordinates
        let (headX : float, headY : float) = cityList.Head.Coordinates
        // clear screen by removing all childs
        drawingCanvas.Children.Clear()
        // draw connections
        List.reduce 
            (fun (acc : City) (next : City) -> 
                this.DrawConnections drawingCanvas acc.X acc.Y next.X next.Y |> ignore
                new City (next.X, next.Y)) 
            cityList |> ignore  
        // draw last connection with first element     
        this.DrawConnections drawingCanvas lastX lastY headX headY  |> ignore
        // draw cities
        List.reduce 
            (fun (acc : City) (next : City) -> 
                this.DrawCity drawingCanvas acc.X acc.Y  |> ignore
                new City (next.X, next.Y)) 
            cityList |> ignore  
        // draw last city because of reduce      
        this.DrawCity drawingCanvas lastX lastY |> ignore     

