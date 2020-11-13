namespace Tuc.LanguageServer

module TucHelpers =
    open System.Collections.Concurrent
    open Tuc
    open Tuc.Domain
    open Tuc.Parser
    open LspHelpers

    module Lsp = LanguageServerProtocol.Types

    type LineNumber = int

    type TucSegment = {
        Location: ParsedLocation
        Hover: Lsp.Hover option
        CompletionItem: Lsp.CompletionItem []
    }

    type TucSegments = ConcurrentDictionary<LineNumber, TucSegment list>

    let private addToLocationEnd addition (location: ParsedLocation): ParsedLocation =
        { location with
            Location =
                { location.Location with
                    Range =
                        { location.Location.Range with
                            End = { location.Location.Range.End with Character = location.Location.Range.End.Character + addition }
                        }
                }
        }

    [<RequireQualifiedAccess>]
    module TucSegment =
        let create location = {
            Location = location
            Hover = None
            CompletionItem = [||]
        }

        let lines: TucSegment -> int list = fun { Location = { Location = { Range = range } }} ->
            [ range.Start.Line .. range.End.Line ]
            |> List.distinct

        let rec private collectParts = function
            | Parsed.KeyWord k as p ->
                [
                    { create k.KeyWordLocation with Hover = p |> Hover.forKeyWord }
                    { create k.ValueLocation with Hover = p |> Hover.forKeyWordValue }
                ]
            | Parsed.KeyWordOnly k as p ->
                [
                    { create k.KeyWordLocation with Hover = p |> Hover.forKeyWord }
                ]
            | Parsed.KeyWordWithBody k as p ->
                [
                    yield { create k.KeyWordLocation with Hover = p |> Hover.forKeyWord }
                    yield { create k.ValueLocation with Hover = p |> Hover.forKeyWordValue }
                    yield! k.Body |> List.collect collectParts
                ]
            | Parsed.KeyWordIf k as p ->
                [
                    yield { create k.IfLocation with Hover = p |> Hover.forKeyWord }
                    yield { create k.ConditionLocation with Hover = p |> Hover.forKeyWordValue }
                    yield! k.Body |> List.collect collectParts

                    match k.ElseLocation, k.ElseBody with
                    | Some elseLocation, Some body ->
                        yield { create elseLocation with Hover = p |> Hover.forElse }
                        yield! body |> List.collect collectParts
                    | _ -> ()
                ]
            | Parsed.Lifeline l as p ->
                [
                    yield { create l.ParticipantLocation with Hover = p |> Hover.forParticipantActivation }
                    yield! l.Execution |> List.collect collectParts
                ]
            | Parsed.MethodCall m as p ->
                [
                    yield {
                        create m.ServiceLocation with
                            Hover = p |> Hover.forParticipantActivation
                            CompletionItem = p |> Completion.forParticipantActivation m.ServiceLocation.Location.Range.End
                        }
                    yield { create m.MethodLocation with Hover = p |> Hover.forMethod }
                    yield! m.Execution |> List.collect collectParts
                ]
            | Parsed.HandleEvent h as p ->
                [
                    yield { create h.StreamLocation with Hover = p |> Hover.forParticipantActivation }  // todo - tady se pro stream i servisu vypise to same (aktivace participanta), asi by to mohlo byt lepsi
                    yield {
                        create h.ServiceLocation with
                            Hover = p |> Hover.forParticipantActivation
                            CompletionItem = p |> Completion.forParticipantActivation h.ServiceLocation.Location.Range.End
                    }
                    yield { create h.MethodLocation with Hover = p |> Hover.forMethod }
                    yield! h.Execution |> List.collect collectParts
                ]
            | Parsed.PostData pd as p ->
                [
                    yield! pd.DataLocation
                        |> List.mapi (fun current d ->
                            let d = d |> addToLocationEnd ".".Length    // the . in the end is for a completion to work correctly

                            { create d with
                                Hover = p |> Hover.forData current
                                CompletionItem = p |> Completion.forData current d.Location.Range.End
                            }
                        )

                    yield { create pd.OperatorLocation with Hover = p |> Hover.forOperator }
                    yield { create pd.DataObjectLocation with Hover = p |> Hover.forParticipantActivation }
                ]
            | Parsed.ReadData rd as p ->
                [
                    yield { create rd.DataObjectLocation with Hover = p |> Hover.forParticipantActivation }
                    yield { create rd.OperatorLocation with Hover = p |> Hover.forOperator }
                    yield! rd.DataLocation
                        |> List.mapi (fun current d ->
                            let d = d |> addToLocationEnd ".".Length    // the . in the end is for a completion to work correctly

                            { create d with
                                Hover = p |> Hover.forData current
                                CompletionItem = p |> Completion.forData current d.Location.Range.End
                            }
                        )
                ]
            | _ -> []

        let collect (parsedTucs: ParsedTuc list): TucSegments =
            let activeParticipantDefinion (p: ParsedParticipantDefinition<_>) participant =
                let hover = participant |> Hover.forParticipantDefinition
                [
                    yield { create p.Context with Hover = hover }
                    yield! p.Domain |> Option.map (fun domain -> { create domain with Hover = hover }) |> Option.toList
                    yield!
                        p.Alias
                        |> Option.map (fun (aliasKeyWord, alias) ->
                            [
                                { create aliasKeyWord with Hover = participant |> Hover.forKeyWord }
                                { create alias with Hover = hover }
                            ]
                        )
                        |> Option.toList
                        |> List.concat
                ]

            parsedTucs
            |> List.collect (fun tuc ->
                [
                    yield!
                        match tuc.Name with
                        | Parsed.KeyWord k ->
                            [
                                { create k.KeyWordLocation with Hover = tuc.Name |> Hover.forKeyWord }
                                { create k.ValueLocation with Hover = tuc.Name |> Hover.forKeyWordValue }
                            ]
                        | _ -> []

                    yield!
                        match tuc.ParticipantsKeyWord with
                        | Parsed.KeyWordWithoutValue p ->
                            [
                                { create p.KeyWordLocation with Hover = tuc.ParticipantsKeyWord |> Hover.forKeyWord }
                            ]
                        | _ -> []

                    yield!
                        tuc.Participants
                        |> List.collect (function
                            | Parsed.ParticipantDefinition p as participant -> activeParticipantDefinion p participant
                            | Parsed.ComponentDefinition c as componentDefinition ->
                                let hover = componentDefinition |> Hover.forParticipantDefinition
                                [
                                    yield { create c.Context with Hover = hover }
                                    yield { create c.Domain with Hover = hover }

                                    yield! c.Participants |> List.collect (function
                                        | Parsed.ParticipantDefinition p as participant -> activeParticipantDefinion p (participant |> Parsed.map Participant)
                                        | _ -> []
                                    )
                                ]
                            | _ -> []
                        )

                    yield! tuc.Parts |> List.collect collectParts
                ]
            )
            |> List.fold (fun segments segment ->
                segment
                |> lines
                |> List.iter (fun line ->
                    segments.AddOrUpdate(line, [ segment ], (fun _ current -> current @ [ segment ])) |> ignore
                )
                segments
            ) (TucSegments())

        let private isInRange (position: Position) (range: Range) =
            // is somewhere between start and end lines
            if range.Start.Line < position.Line && position.Line < range.End.Line then true
            // is at start or end line, in the range boundaries
            elif
                (position.Line = range.Start.Line || position.Line = range.End.Line)
                && range.Start.Character <= position.Character && position.Character <= range.End.Character then true
            // is not there
            else false

        let tryFind position (segments: TucSegments) =
            segments.TryFind(position.Line)
            |> Option.bind (List.tryFind (fun s -> s.Location.Location.Range |> isInRange position))

        let hover { Hover = h } = h
        let completionItem { CompletionItem = ci } = ci
