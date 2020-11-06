namespace Tuc.LanguageServer

open Tuc
open Tuc.Domain
open Tuc.Parser
open FsLibLog
open FSharp.Compiler.Range
open LanguageServerProtocol.Types
open LspHelpers
open TucHelpers

type Commands = {
    ResolveDomainTypes: string option -> DomainType list    // todo - return Result<DomainType list, DomainParseError>
    ParseTucs: DomainType list -> TextDocumentItem -> Async<unit>    // todo return Async<Result<ParsedTuc list, ParseError>>
    ParseTucsForFile: DomainType list -> DocumentUri -> Async<unit>    // todo return Async<Result<ParsedTuc list, ParseError>>
    TryGetFileCheckerOptionsWithLines: DocumentUri -> ResultOrString<LineStr []>
    TryGetLineSegment: DocumentUri * Tuc.Position -> ResultOrString<TucSegment option>
}

[<RequireQualifiedAccess>]
module Commands =
    let logger = LogProvider.getLoggerByName "State"
    let private state = State.Initial logger

    let create resolveDomainTypes parseTucsDoc parseTucsFile = {
        ResolveDomainTypes = resolveDomainTypes
        ParseTucs = fun domainTypes doc -> async {
            let! lines, parsedTucs = doc |> parseTucsDoc domainTypes

            let segments = parsedTucs |> TucSegment.collect
            logger.info (Log.setMessage (sprintf "Parsed segments [%A]" segments.Count))

            state.AddFileText(doc.GetFilePath(), lines, segments, None)
        }
        ParseTucsForFile = fun domainTypes file -> async {
            let! lines, parsedTucs = file |> parseTucsFile domainTypes

            let segments = parsedTucs |> TucSegment.collect
            logger.info (Log.setMessage (sprintf "Parsed segments [%A]" segments.Count))

            state.AddFileText(file, lines, segments, None)
        }
        TryGetFileCheckerOptionsWithLines = Path.getFullPathSafe >> state.TryGetFileLines
        TryGetLineSegment = fun (file, position) -> state.TryGetLineSegment(file |> Path.getFullPathSafe, position)
    }
