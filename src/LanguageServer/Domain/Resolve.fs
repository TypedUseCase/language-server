namespace Tuc.LanguageServer

module Domain =
    open FsLibLog
    open Tuc.Domain
    open ErrorHandling
    open Console

    let private parseDomain (input, output) domain =
        domain
        |> FileOrDir.files
        |> List.map (Parser.parse output)

    let private checkDomain (input, output) domain =
        result {
            let parsedDomains =
                domain
                |> parseDomain (input, output)

            let! resolvedTypes =
                parsedDomains
                |> Resolver.resolve output
                |> Result.mapError UnresolvedTypes

            let! domainTypes =
                resolvedTypes
                |> Checker.check output
                |> Result.mapError UndefinedTypes

            return domainTypes
        }

    let private resolveDomainTypes (logger: ILog) (input, output) path =
        logger.info (Log.setMessage "Resolve domain types")

        try
            path
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

    let resolveDomanTypesAndWatch (logger: ILog) (input, output) execute path =
        try
            let domainPath =
                path
                |> FileOrDir.parse "Domain.fsx"
                |> tee (FileOrDir.debug output "Domain")

            let path, watchSubdirs =
                match domainPath with
                | File file -> file, WatchSubdirs.No
                | Dir (dir, _) -> dir, WatchSubdirs.Yes

            let execute () =
                resolveDomainTypes logger (input, output) domainPath
                |> execute

            (path, "*Domain.fsx")
            |> Watch.watch output watchSubdirs execute
            |> Async.Start

            Watch.executeAndWaitForWatch output execute

        with e ->
            e
            |> sprintf "Path %A NOT resolved due to exception:\n%A" path
            |> Log.setMessage
            |> logger.error

            Async.retn ()
