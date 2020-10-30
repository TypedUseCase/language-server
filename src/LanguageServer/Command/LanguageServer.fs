namespace Tuc.LanguageServer.Command

open MF.ConsoleApplication
open Tuc.LanguageServer.Console

[<RequireQualifiedAccess>]
module LanguageServer =
    open Tuc.Domain

    [<RequireQualifiedAccess>]
    module Start =
        let arguments = []
        let options = []

        let private start commands =
            let result = Tuc.LanguageServer.Lsp.start commands

            eprintfn "[E] LanguageServer.Lsp.start.result: %A." result
            printfn "[P] LanguageServer.Lsp.start.result: %A." result
            ()

        let private resolveDomainTypes (input, output) path =
            path
            |> FileOrDir.parse ".fsx"
            |> tee (FileOrDir.debug output "Domain")
            |> checkDomain (input, output)
            |> function
                | Ok domainTypes -> domainTypes
                | Error error ->
                    eprintfn "[E] LanguageServer.Lsp.start.result: %A." error
                    printfn "[P] LanguageServer.Lsp.start.result: %A." error
                    []

        let execute: ExecuteCommand = fun (input, output) ->
            output.Title "[LS] Start"

            start {
                ResolveDomainTypes = resolveDomainTypes (input, output)
            }

            // todo - remove
            System.DateTime.Now
            |> sprintf "start executed at %A\n"
            |> tee (fun t -> System.IO.File.AppendAllText("/Users/chromecp/fsharp/language-server/log.txt", t))
            |> tee (eprintfn "E: %s")
            |> ignore

            ExitCode.Success
