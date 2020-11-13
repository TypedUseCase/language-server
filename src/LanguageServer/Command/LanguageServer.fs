namespace Tuc.LanguageServer.Command

open MF.ConsoleApplication
open Tuc.LanguageServer
open Tuc.LanguageServer.Console

open FsLibLog

[<RequireQualifiedAccess>]
module LanguageServer =
    open Tuc.Domain
    open Tuc.Parser

    [<RequireQualifiedAccess>]
    module Start =
        let arguments = []
        let options = []

        let private start (logger: ILog) output commands =
            let result = Tuc.LanguageServer.Lsp.start output commands
            logger.info (Log.setMessage <| sprintf "[TUC.LS] Start with %A" result)

        let private resolveDomainTypes (logger: ILog) (input, output) path =
            logger.info (Log.setMessage "Resolve domain types")

            try
                path
                |> FileOrDir.parse ".fsx"
                |> tee (FileOrDir.debug output "Domain")
                |> checkDomain (input, output)
                |> function
                    | Ok domainTypes -> domainTypes
                    | Error error ->
                        logger.error (Log.setMessage <| sprintf "Resolve domain types with error: %A" error)
                        []
                |> tee (fun dt -> logger.info (Log.setMessage <| sprintf "Resolved domain types [%d]" (dt |> List.length)))
            with e ->
                e
                |> sprintf "Path %A NOT resolved due to exception:\n%A" path
                |> Log.setMessage
                |> logger.error

                []

        open LspHelpers

        let private parseTucsFromFileLines (logger: ILog) (input, output) domainTypes file lines = async {
            let logInfo = Log.setMessage >> logger.info

            try
                logInfo <| sprintf "Parse tuc from %A" file

                match lines |> Parser.parseLines output false domainTypes file with
                | Ok parsed ->
                    parsed
                    |> List.length
                    |> sprintf "File %s parsed into %A tucs" file
                    |> logInfo

                    return lines, Ok parsed
                | Error e ->
                    e
                    |> sprintf "File %s NOT parsed due to:\n%A" file
                    |> Log.setMessage
                    |> logger.error

                    return lines, Error e
            with e ->
                e
                |> sprintf "File %s NOT parsed due to exception:\n%A" file
                |> Log.setMessage
                |> logger.error
                // todo - add a parse error for exception?

                return [||], Ok []
        }

        let private parseTucsFromFile (logger: ILog) (input, output) domainTypes (file: string) = async {
            let lines = System.IO.File.ReadAllLines file

            return! parseTucsFromFileLines logger (input, output) domainTypes file lines
        }

        let private parseTucs (logger: ILog) (input, output) domainTypes (textDocument: LanguageServerProtocol.Types.TextDocumentItem) = async {
            let file = textDocument.GetFilePath()
            let lines = textDocument.Text.Split("\n")

            return! parseTucsFromFileLines logger (input, output) domainTypes file lines
        }

        let execute (logger: ILog): ExecuteCommand = fun (input, output) ->
            output.Title "[TUC.LS] Start"
            logger.info (Log.setMessage "Start")

            Commands.create
                (resolveDomainTypes (LogProvider.getLoggerByName "TUC.LS.ResolveDomain") (input, output))
                (parseTucs (LogProvider.getLoggerByName "TUC.LS.ParseTuc") (input, output))
                (parseTucsFromFile (LogProvider.getLoggerByName "TUC.LS.ParseTuc") (input, output))
            |> start logger output

            logger.info (Log.setMessage <| sprintf "Started at %A" System.DateTime.Now)

            ExitCode.Success
