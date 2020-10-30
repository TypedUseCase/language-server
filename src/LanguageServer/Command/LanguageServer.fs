namespace Tuc.LanguageServer.Command

open MF.ConsoleApplication
open Tuc.LanguageServer.Console

open FsLibLog

[<RequireQualifiedAccess>]
module LanguageServer =
    open Tuc.Domain

    [<RequireQualifiedAccess>]
    module Start =
        let arguments = []
        let options = []

        let private start (logger: ILog) commands =
            let result = Tuc.LanguageServer.Lsp.start commands

            logger.info (Log.setMessage <| sprintf "[LS] Start with %A" result)

            eprintfn "[E] LanguageServer.Lsp.start.result: %A." result
            printfn "[P] LanguageServer.Lsp.start.result: %A." result
            ()

        let private resolveDomainTypes (logger: ILog) (input, output) path =
            logger.info (Log.setMessage "Resolve domain types")

            path
            |> FileOrDir.parse ".fsx"
            |> tee (FileOrDir.debug output "Domain")
            |> checkDomain (input, output)
            |> function
                | Ok domainTypes -> domainTypes
                | Error error ->
                    logger.error (Log.setMessage <| sprintf "[LS] Resolve domain types with error: %A" error)
                    []
            |> tee (fun dt -> logger.info (Log.setMessage <| sprintf "Resolved domain types [%d]" (dt |> List.length)))

        let execute (logger: ILog): ExecuteCommand = fun (input, output) ->
            output.Title "[LS] Start"
            logger.info (Log.setMessage "Start")

            start logger {
                ResolveDomainTypes = resolveDomainTypes logger (input, output)
            }

            logger.info (Log.setMessage <| sprintf "Started at %A" System.DateTime.Now)

            ExitCode.Success
