namespace Tuc.LanguageServer

module Console =
    open System.IO
    open MF.ConsoleApplication

    let commandHelp lines = lines |> String.concat "\n\n" |> Some

    /// Concat two lines into one line for command help, so they won't be separated by other empty line
    let inline (<+>) line1 line2 = sprintf "%s\n%s" line1 line2

    type FileOrDir =
        | File of string
        | Dir of string * string list

    [<RequireQualifiedAccess>]
    module FileOrDir =
        let parse (extension: string) = function
            | Some file when file |> File.Exists && file.EndsWith extension ->
                FileOrDir.File file

            | Some dir when dir |> Directory.Exists ->
                Dir (
                    dir,
                    [ dir ] |> FileSystem.getAllFiles |> List.filter (fun f -> f.EndsWith extension)
                )

            | invalidPath -> failwithf "Path to file(s) %A is invalid." invalidPath

        let debug output title = output.Options (sprintf "%s file(s):" title) << function
            | File file -> [[ file ]]
            | Dir (_, files) -> files |> List.map List.singleton

        let file = function
            | File file -> Some file
            | _ -> None

        let files = function
            | File file -> [ file ]
            | Dir (_, files) -> files

    open Tuc.Domain
    open ErrorHandling

    let parseDomain (input, output) domain =
        domain
        |> FileOrDir.files
        |> List.map (Parser.parse output)

    let checkDomain (input, output) domain =
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

    let showParseDomainError output = function
        | UnresolvedTypes unresolvedTypes ->
            unresolvedTypes
            |> List.map (TypeName.value >> List.singleton)
            |> output.Options (sprintf "Unresolved types [%d]:" (unresolvedTypes |> List.length))

            output.Error "You have to solve unresolved types first.\n"
        | UndefinedTypes undefinedTypes ->
            undefinedTypes
            |> List.map (TypeName.value >> List.singleton)
            |> output.Options (sprintf "Undefined types [%d]:" (undefinedTypes |> List.length))

            output.Error "You have to define all types first.\n"
