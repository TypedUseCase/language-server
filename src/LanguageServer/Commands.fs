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
    | Other of string
    | CtrlSpace

type Commands = {
    ResolveDomainTypes: string option -> DomainType list    // todo - return Result<DomainType list, DomainParseError>
    ParseTucs: DomainType list -> TextDocumentItem -> Async<unit>    // todo return Async<Result<ParsedTuc list, ParseError>>
    ParseTucsForFile: DomainType list -> DocumentUri -> Async<unit>    // todo return Async<Result<ParsedTuc list, ParseError>>
    TryGetFileCheckerOptionsWithLines: DocumentUri -> ResultOrString<LineStr []>
    TryGetLineSegment: DocumentUri * Tuc.Position -> ResultOrString<TucSegment option>
    SegmentsCount: DocumentUri -> int
    FindDefaultCompletionItems: DocumentUri * Tuc.Position -> CompletionTrigger -> CompletionItem []
}

[<RequireQualifiedAccess>]
module Commands =
    let logger = LogProvider.getLoggerByName "State"
    let private state = State.Initial logger

    type private PartialRange = {
        Start: Tuc.Position
        End: Tuc.Position option
    }

    let create resolveDomainTypes parseTucsDoc parseTucsFile = {
        // todo resolve domain types better
        (*
            - async task, ktery bude bezet "porad" a bude watchovat domenove soubory v rootu a drzet si stav domenovych typu ve `state`
            - tady teda bude funkce na nastartovani toho tasku a zvenku jen dostane funkci, ktera bude resolvovat, tak jak ted
            - pak tady bude metoda, ktera bude ty domenove typy vracet ze statu, pripadne pak vracet diagnosticke chyby
         *)

        ResolveDomainTypes = resolveDomainTypes

        ParseTucs = fun domainTypes doc -> async {
            let! lines, parsedTucs = doc |> parseTucsDoc domainTypes

            let segments = parsedTucs |> TucSegment.collect
            logger.info (Log.setMessage (sprintf "Parsed segments [%A]" segments.Count))

            state.AddFileText(doc.GetFilePath(), lines, segments, parsedTucs, None)
        }

        ParseTucsForFile = fun domainTypes file -> async {
            let! lines, parsedTucs = file |> parseTucsFile domainTypes

            let segments = parsedTucs |> TucSegment.collect
            logger.info (Log.setMessage (sprintf "Parsed segments [%A]" segments.Count))

            state.AddFileText(file, lines, segments, parsedTucs, None)
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
    }
