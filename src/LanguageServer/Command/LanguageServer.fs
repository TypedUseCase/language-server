namespace Tuc.LanguageServer.Command

open MF.ConsoleApplication
open Tuc.LanguageServer

open FsLibLog

[<RequireQualifiedAccess>]
module LanguageServer =

    [<RequireQualifiedAccess>]
    module Start =
        let arguments = []
        let options = []

        let private start (logger: ILog) output commands =
            let result = Tuc.LanguageServer.Lsp.start output commands
            logger.info (Log.setMessage <| sprintf "[TUC.LS] Start with %A" result)

        let execute (logger: ILog): ExecuteCommand = fun (input, output) ->
            output.Title "[TUC.LS] Start"
            logger.info (Log.setMessage "Start")

            Commands.create
                (Domain.resolveDomanTypesAndWatch (LogProvider.getLoggerByName "TUC.LS.ResolveDomain") (input, output))
                (Tuc.parseTucs (LogProvider.getLoggerByName "TUC.LS.ParseTuc") (input, output))
                (Tuc.parseTucsFromFile (LogProvider.getLoggerByName "TUC.LS.ParseTuc") (input, output))
            |> start logger output

            logger.info (Log.setMessage <| sprintf "Started at %A" System.DateTime.Now)

            ExitCode.Success
