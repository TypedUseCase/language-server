namespace Tuc.LanguageServer.Command

open MF.ConsoleApplication
open Tuc.LanguageServer
open Tuc.LanguageServer.Console

open FsLibLog

[<RequireQualifiedAccess>]
module LanguageServer =
    open Tuc.Domain

    [<RequireQualifiedAccess>]
    module Start =
        let arguments = []
        let options = []

        let private start (logger: ILog) output commands =
            let result = Tuc.LanguageServer.Lsp.start output commands

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

        open LspHelpers

        let private parseTucs (logger: ILog) (input, output) domainTypes (textDocument: LanguageServerProtocol.Types.TextDocumentItem) =
            let file = textDocument.GetFilePath()
            logger.info (Log.setMessage <| sprintf "Parse tuc for %A" file)

            let logInfo = Log.setMessage >> logger.info

            try
                match Tuc.Parser.Parser.parse output true domainTypes file with
                | Ok parsed ->
                    parsed
                    |> List.length
                    |> sprintf "Doc %s parsed into %A tucs" file
                    |> logInfo

                    parsed
                | Error e ->
                    e
                    |> sprintf "Doc %s NOT parsed due to:\n%A" file
                    |> logInfo
                    []
            with e ->
                e
                |> sprintf "Doc %s NOT parsed due to exception:\n%A" file
                |> logInfo
                []

        let execute (logger: ILog): ExecuteCommand = fun (input, output) ->
            output.Title "[LS] Start"
            logger.info (Log.setMessage "Start")

            start logger output {
                Commands.defaults with
                    ResolveDomainTypes = resolveDomainTypes logger (input, output)  // todo - pass a new logger
                    ParseTucs = parseTucs logger (input, output)    // todo - pass a new logger
            }

            logger.info (Log.setMessage <| sprintf "Started at %A" System.DateTime.Now)

            ExitCode.Success
