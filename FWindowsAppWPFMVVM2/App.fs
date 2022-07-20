namespace GA.Salesman.App

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Markup

open GA.Salesman.Models

module MainWindow =
    open GA.Salesman.ViewModel

    // Application Entry point
    [<STAThread>]
    [<EntryPoint>]
    let main(_) = 
        // Create the View and bind it to the View Model
        let mainWindow = Application.LoadComponent(
                                     new System.Uri("/App;component/MainWindow.xaml", UriKind.Relative)) :?> Window
        let populationModel : Population = new Population ( [] )
        mainWindow.DataContext <- new MainWindowViewModel (mainWindow, populationModel) 
        let application = new Application()
        (mainWindow.DataContext :?> MainWindowViewModel).InitControls () |> ignore
        application.Run(mainWindow) 
    