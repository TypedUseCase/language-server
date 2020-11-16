namespace Tuc.LanguageServer

module Tuc =
    open FsLibLog
    open Tuc.Parser
    open Tuc.Domain
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

    let parseTucsFromFile (logger: ILog) (input, output) domainTypes (file: string) = async {
        let lines = System.IO.File.ReadAllLines file

        return! parseTucsFromFileLines logger (input, output) domainTypes file lines
    }

    let parseTucs (logger: ILog) (input, output) domainTypes (textDocument: LanguageServerProtocol.Types.TextDocumentItem) = async {
        let file = textDocument.GetFilePath()
        let lines = textDocument.Text.Split("\n")

        return! parseTucsFromFileLines logger (input, output) domainTypes file lines
    }
