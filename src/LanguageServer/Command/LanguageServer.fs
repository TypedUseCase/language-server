namespace Tuc.LanguageServer.Command

open MF.ConsoleApplication
open Tuc.LanguageServer.Console

[<RequireQualifiedAccess>]
module LanguageServer =
    open Tuc.Domain

    [<RequireQualifiedAccess>]
    module Start =
        let arguments = [
            Argument.domain
        ]

        let options = [
            Option.noValue "watch" (Some "w") "Whether to watch domain file(s) for changes."
        ]

        let execute: ExecuteCommand = fun (input, output) ->
            let domain = (input, output) |> Input.getDomain

            output.Title "[LS] Start"

            if output.IsVerbose() then
                output.Message <| sprintf "[LS] Domain: %A" domain

            let execute domain =
                System.DateTime.Now
                |> sprintf "start executed at %A\n"
                |> tee (fun t -> System.IO.File.AppendAllText("/Users/chromecp/fsharp/language-server/log.txt", t))
                |> tee (eprintfn "E: %s")
                |> ignore

                let domainResult = domain |> checkDomain (input, output)

                match input, domainResult with
                | _, Error error -> error |> showParseDomainError output    // todo - ignore errors ?

                | _, Ok domainTypes ->
                    domainTypes
                    |> tee (fun domainTypes ->
                        output.NewLine()
                        output.Section <| sprintf "[LS] Domain types [%d]" (domainTypes |> List.length)

                        output.Error <| sprintf "[LS][E] Parsed domainTypes %d" (domainTypes |> List.length)
                    )
                    |> List.iter (fun (DomainType domainType) -> domainType |> Dump.parsedType output)

            match input with
            | Input.HasOption "watch" _ ->
                let path, watchSubdirs =
                    match domain with
                    | File file -> file, WatchSubdirs.No
                    | Dir (dir, _) -> dir, WatchSubdirs.Yes

                (path, "*.fsx")
                |> watch output watchSubdirs (fun _ -> execute None)
                |> Async.Start

                executeAndWaitForWatch output (fun _ -> execute (Some domain))
                |> Async.RunSynchronously
            | _ ->
                execute (Some domain)

            ExitCode.Success
