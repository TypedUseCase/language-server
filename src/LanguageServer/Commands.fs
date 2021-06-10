namespace Tuc.LanguageServer

open Tuc
open Tuc.Domain
open Tuc.Parser
open FsLibLog
open LanguageServerProtocol.Types
open LspHelpers
open TucHelpers

[<RequireQualifiedAccess>]
type CompletionTrigger =
    | Dot
    | Other of char
    | CtrlSpace

type Commands = {
    StartResolvingDomainTypes: (int -> Async<unit>) -> string option -> unit

    GetDomainTypes: unit -> DomainType list
    CountDomainTypes: unit -> int

    ParseTucs: DomainType list -> TextDocumentItem -> Async<Diagnostic[]>
    ParseTucsForFile: DomainType list -> DocumentUri -> Async<Diagnostic[]>

    TryGetFileCheckerOptionsWithLines: DocumentUri -> ResultOrString<LineStr []>
    TryGetLineSegment: DocumentUri * Tuc.Position -> ResultOrString<TucSegment option>

    SegmentsCount: DocumentUri -> int

    FindDefaultCompletionItems: DocumentUri * Tuc.Position -> CompletionTrigger -> CompletionItem []
    ClearFileCache: DocumentUri -> Async<unit>
}

[<RequireQualifiedAccess>]
module Commands =
    let logger = LogProvider.getLoggerByName "State"
    let private state = State.Initial logger

    let mutable private isResolving = false

    type private PartialRange = {
        Start: Tuc.Position
        End: Tuc.Position option
    }

    let create resolveDomainTypes parseTucsDoc parseTucsFile = {
        StartResolvingDomainTypes = fun notify root ->
            if isResolving then ()
            else
                root
                |> resolveDomainTypes (
                    tee (List.length >> notify >> Async.Start)
                    >> state.SetDomainTypes
                )
                |> Async.Start

        GetDomainTypes = state.GetDomainTypes
        CountDomainTypes = state.CountDomainTypes

        ParseTucs = fun domainTypes doc -> async {
            let! lines, parsedTucs = doc |> parseTucsDoc domainTypes

            match parsedTucs with
            | Ok parsedTucs ->
                let segments = parsedTucs |> TucSegment.collect
                logger.info (Log.setMessage (sprintf "Parsed segments [%A]" segments.Count))

                state.AddFileText(doc.GetFilePath(), lines, segments, parsedTucs, None)

                return [||]

            | Error errors ->
                doc.GetFilePath() |> state.ClearFile |> Async.Start

                return
                    errors
                    |> List.map Diagnostics.forParseError
                    |> List.toArray
        }

        ParseTucsForFile = fun domainTypes file -> async {
            let! lines, parsedTucs = file |> parseTucsFile domainTypes

            match parsedTucs with
            | Ok parsedTucs ->
                let segments = parsedTucs |> TucSegment.collect
                logger.info (Log.setMessage (sprintf "Parsed segments [%A]" segments.Count))

                state.AddFileText(file, lines, segments, parsedTucs, None)

                return [||]

            | Error errors ->
                file |> state.ClearFile |> Async.Start

                return
                    errors
                    |> List.map Diagnostics.forParseError
                    |> List.toArray
        }

        TryGetFileCheckerOptionsWithLines = Path.getFullPathSafe >> state.TryGetFileLines

        TryGetLineSegment = fun (file, position) -> state.TryGetLineSegment(file |> Path.getFullPathSafe, position)

        SegmentsCount = Path.getFullPathSafe >> state.CountSegments

        FindDefaultCompletionItems = fun (file, position) -> function
            | CompletionTrigger.Dot
            | CompletionTrigger.Other _ -> [||]
            | CompletionTrigger.CtrlSpace ->
                let rec findTucForPosition tuc: ParsedTuc list -> _ = function
                    | { Name = Parsed.KeyWord k } :: _ when k.KeyWordLocation.Location.Range.Start.Line = position.Line ->
                        None

                    | { Name = Parsed.KeyWord k } as tuc :: tucs when k.KeyWordLocation.Location.Range.Start.Line < position.Line ->
                        tucs |> findTucForPosition (Some tuc)

                    | _ -> tuc

                file
                |> state.GetParsedTucs
                |> findTucForPosition None
                |> Option.map (fun tuc ->
                    tuc.Participants
                    |> List.collect (Completion.forActiveParticipant)
                )
                |> Option.defaultValue []
                |> List.toArray

        ClearFileCache = state.ClearFile
    }
