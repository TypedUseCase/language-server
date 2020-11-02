namespace Tuc.LanguageServer

open Tuc.Domain
open FsLibLog
open FSharp.Compiler.Range

type Commands = {
    ResolveDomainTypes: string option -> DomainType list    // todo - return Result<DomainType list, DomainParseError>
    TryGetFileCheckerOptionsWithLines: string -> ResultOrString<LineStr []>
    TryGetFileCheckerOptionsWithLinesAndLineStr: string * pos -> ResultOrString<LineStr [] * LineStr>
}

[<RequireQualifiedAccess>]
module Commands =
    let logger = LogProvider.getLoggerByName "State"
    let private state = State.Initial logger

    let defaults = {
        ResolveDomainTypes = fun _ -> []
        TryGetFileCheckerOptionsWithLines = Path.getFullPathSafe >> state.TryGetFileCheckerOptionsWithLines
        TryGetFileCheckerOptionsWithLinesAndLineStr = fun (file, pos) -> state.TryGetFileCheckerOptionsWithLinesAndLineStr(file |> Path.getFullPathSafe, pos)
    }
