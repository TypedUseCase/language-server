namespace Tuc.LanguageServer

[<RequireQualifiedAccess>]
module Completion =
    open Tuc
    open Tuc.Domain
    open Tuc.Parser
    open LspHelpers

    module Lsp = LanguageServerProtocol.Types
    open Lsp

    /// Documentation must be send like this, otherwise `CompletionItemResolve` ends with error
    let private documentation content =
        Newtonsoft.Json.Linq.JToken.FromObject content |> Some

    [<System.Obsolete("todo - fix first")>]
    let private textEdit position (value: string) =
        Some {
            // todo - if a textEdit woul be used, the range must be fixed, since it now rewrites a value after a completion...
            Range = rangeToProtocolRange {
                Start = position
                End = { position with Character = position.Character + value.Length }
            }
            NewText = value
        }

    let private formatFS =
        let indent = "    "

        function
        | DomainType (SingleCaseUnion { Domain = domain; Name = name; ConstructorName = cstrName; ConstructorArgument = arg }) ->
            sprintf "type %s = %s of %s" (name |> TypeName.value) cstrName (arg |> TypeDefinition.value) |> Some

        | DomainType (DiscriminatedUnion { Domain = domain; Name = name; Cases = cases }) ->
            [
                yield sprintf "type %s =" (name |> TypeName.value)
                yield! cases |> List.map (function
                    | { Name = name; Argument = TypeDefinition.Type (TypeName "unit") } -> sprintf "%s| %s" indent (name |> TypeName.value)
                    | { Name = name; Argument = arg } -> sprintf "%s| %s of %s" indent (name |> TypeName.value) (arg |> TypeDefinition.value)
                )
            ]
            |> String.concat "\n"
            |> Some

        | _ -> None

    let private dataCompletionItem i (name, dataType) =
        let label = name |> TypeName.value

        let ci =
            { CompletionItem.Create(label) with
                Kind = Some CompletionItemKind.Field
                SortText = Some (sprintf "%06d" i)
                InsertText = Some label
                // TextEdit = textEdit position label
                FilterText = Some label
            }

        match dataType |> Option.bind formatFS with
        | Some signature -> { ci with Data = documentation (sprintf "```fsharp\n%s\n```" signature) }
        | _ -> ci

    let rec forActiveParticipant: ParsedParticipant -> _ = function
        | Parsed.ComponentDefinition { Participants = participants } ->
            participants
            |> List.collect (Parsed.map Participant >> forActiveParticipant)

        | Parsed.ParticipantDefinition { Value = Participant (Service { Context = context; Domain = domain }) } ->
            let label = context
            let signature = sprintf "_Service: %s_.**%s**" (domain |> DomainName.value) context

            [
                {
                    CompletionItem.Create(label) with
                        Kind = Some CompletionItemKind.Variable
                        SortText = Some (sprintf "%06d" 0)
                        // TextEdit = textEdit position methodName
                        InsertText = Some label
                        FilterText = Some label
                        Data = documentation signature
                }
            ]

        | Parsed.ParticipantDefinition { Value = Participant (ActiveParticipant.Stream { Context = context; Domain = domain; StreamType = DomainType.StreamEvent (_, event) } as p) } ->
            let label = context
            let signature = sprintf "_Stream: %s_.**%s**" (domain |> DomainName.value) context

            [
                {
                    CompletionItem.Create(label) with
                        Kind = Some CompletionItemKind.Variable
                        SortText = Some (sprintf "%06d" 0)
                        // TextEdit = textEdit position methodName
                        InsertText = Some (p |> ActiveParticipant.value)
                        FilterText = Some label
                        Data = documentation signature
                }

                { dataCompletionItem 1 (TypeName event, None) with
                    Kind = Some CompletionItemKind.Event
                    // todo<later> - add a Event dataType to the ResolvedType.Stream and show a signature here
                    Data = documentation (sprintf "_Event:_ **%s**" event)
                }
            ]

        | Parsed.ParticipantDefinition { Value = Participant (DataObject { Context = context; Domain = domain; DataObjectType = DomainType.DataObjectData (_, data) } as p) } ->
            let label = context
            let signature = sprintf "_Data Object: %s_.**%s**" (domain |> DomainName.value) context

            [
                {
                    CompletionItem.Create(label) with
                        Kind = Some CompletionItemKind.Variable
                        SortText = Some (sprintf "%06d" 0)
                        // TextEdit = textEdit position methodName
                        InsertText = Some (p |> ActiveParticipant.value)
                        FilterText = Some label
                        Data = documentation signature
                }

                { dataCompletionItem 1 (TypeName data, None) with
                    // todo<later> - add a Event dataType to the ResolvedType.Stream and show a signature here
                    Data = documentation (sprintf "_Data:_ **%s**" data)
                }
            ]

        | _ -> []

    let forParticipantActivation position: ParsedTucPart -> _ = function
        | Parsed.MethodCall { Value = ServiceMethodCall { Service = Service { ServiceType = DomainType (Record { Methods = methods }) } }} ->
            methods
            |> Map.toList
            |> List.mapi (fun i (name, func) ->
                let label = name |> FieldName.value
                let signature =
                    sprintf "type %s = %s -> %s"
                        (name |> FieldName.value)
                        (func.Argument |> TypeDefinition.value)
                        (func.Returns |> TypeDefinition.value)

                {
                    CompletionItem.Create(label) with
                        Kind = Some CompletionItemKind.Method
                        SortText = Some (sprintf "%06d" i)
                        // TextEdit = textEdit position methodName
                        InsertText = Some label
                        FilterText = Some label
                        Data = documentation (sprintf "```fsharp\n%s\n```" signature)
                }
            )
            |> List.toArray

        | Parsed.HandleEvent { Value = HandleEventInStream { Service = Service { ServiceType = DomainType (Record { Handlers = handlers }) } }} ->
            handlers
            |> Map.toList
            |> List.mapi (fun i (name, handler) ->
                let label = name |> FieldName.value
                let signature =
                    sprintf "type %s = %s<%s>"
                        (name |> FieldName.value)
                        (handler.Name |> TypeName.value)
                        (handler.Handles |> TypeDefinition.value)

                {
                    CompletionItem.Create(label) with
                        Kind = Some CompletionItemKind.Method
                        SortText = Some (sprintf "%06d" i)
                        // TextEdit = textEdit position handlerName
                        InsertText = Some label
                        FilterText = Some label
                        Data = documentation (sprintf "```fsharp\n%s\n```" signature)
                }
            )
            |> List.toArray

        | _ -> [||]

    let forData current position: ParsedTucPart -> _ = function
        | Parsed.ReadData { Value = ReadData { Data = data }}
        | Parsed.PostData { Value = PostData { Data = data }}

        | Parsed.ReadData { Value = ReadEvent { Event = Event data }}
        | Parsed.PostData { Value = PostEvent { Event = Event data }}
            ->
                data
                |> Data.casesFor current
                |> List.mapi (fun i (name, dataType) -> dataCompletionItem i (name, Some dataType))
                |> List.toArray

        | _ -> [||]
