open System
open MF.ConsoleApplication
open Tuc.LanguageServer

[<EntryPoint>]
let entry argv =
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
            Execute = Command.LanguageServer.Start.execute
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
