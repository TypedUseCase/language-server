namespace Tuc.LanguageServer

open Tuc
open Tuc.Domain
open Tuc.Parser
open FsLibLog
open FSharp.Compiler.Range
open LanguageServerProtocol.Types

type Commands = {
    ResolveDomainTypes: string option -> DomainType list    // todo - return Result<DomainType list, DomainParseError>
    ParseTucs: DomainType list -> TextDocumentItem -> ParsedTuc list
    TryGetFileCheckerOptionsWithLines: string -> ResultOrString<LineStr []>
    TryGetFileCheckerOptionsWithLinesAndLineStr: string * pos -> ResultOrString<LineStr [] * LineStr>
}

[<RequireQualifiedAccess>]
module Commands =
    let logger = LogProvider.getLoggerByName "State"
    let private state = State.Initial logger

    let defaults = {
        ResolveDomainTypes = fun _ -> []
        ParseTucs = fun _ _ -> []
        TryGetFileCheckerOptionsWithLines = Path.getFullPathSafe >> state.TryGetFileCheckerOptionsWithLines
        TryGetFileCheckerOptionsWithLinesAndLineStr = fun (file, pos) -> state.TryGetFileCheckerOptionsWithLinesAndLineStr(file |> Path.getFullPathSafe, pos)
    }
