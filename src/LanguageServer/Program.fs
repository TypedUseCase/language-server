open System
open MF.ConsoleApplication
open Tuc.LanguageServer
open FsLibLog

module private Logger =
    open Serilog
    open Serilog.Core
    open Serilog.Events

    let init () =
        // default the verbosity to warning
        let verbositySwitch = LoggingLevelSwitch(LogEventLevel.Information) // todo - default was warning
        let outputTemplate = "[{Timestamp:HH:mm:ss.fff} {Level:u3}] [{SourceContext}] {Message:lj}{NewLine}{Exception}"
        let logConf =
            LoggerConfiguration()
                .MinimumLevel.ControlledBy(verbositySwitch)
                .Enrich.FromLogContext()
                //.Destructure.FSharpTypes()
                .WriteTo.Async(
                    fun c -> c.Console(outputTemplate = outputTemplate, standardErrorFromLevel = System.Nullable<_>(LogEventLevel.Verbose), theme = Serilog.Sinks.SystemConsole.Themes.AnsiConsoleTheme.Code) |> ignore
                ) // make it so that every console log is logged to stderr

        //Options.apply verbositySwitch logConf results

        let logger = logConf.CreateLogger()
        Serilog.Log.Logger <- logger
        LogProvider.setLoggerProvider (Providers.SerilogProvider.create())

[<EntryPoint>]
let entry argv =
    Logger.init()

    let logger = LogProvider.getLoggerByName "Tuc.LS"

    consoleApplication {
        title AssemblyVersionInformation.AssemblyProduct
        info ApplicationInfo.MainTitle
        version AssemblyVersionInformation.AssemblyVersion

        command "ls:start" {
            Description = "Start a language server for a TUC domain."
            Help = None
            Arguments = Command.LanguageServer.Start.arguments
            Options = Command.LanguageServer.Start.options
            Initialize = None
            Interact = None
            Execute = Command.LanguageServer.Start.execute logger
        }

        command "about" {
            Description = "Displays information about the current project."
            Help = None
            Arguments = []
            Options = []
            Initialize = None
            Interact = None
            Execute = Command.Common.about
        }
    }
    |> run argv
