namespace Tuc.LanguageServer

[<RequireQualifiedAccess>]
module Hover =
    open Tuc
    open Tuc.Domain
    open Tuc.Parser
    open LspHelpers

    module Lsp = LanguageServerProtocol.Types
    open Lsp

    let private tucLang value = MarkedString.WithLanguage { Language = "tuc"; Value = value }
    let private fsLang value = MarkedString.WithLanguage { Language = "fsharp"; Value = value }
    let private mdString = MarkedString.String

    // todo - move this function to Tuc.Domain lib
    let private domainTypeToFS =
        let fsType lines =
            lines
            |> List.filter (System.String.IsNullOrWhiteSpace >> not)
            |> String.concat "\n"
            |> fsLang
            |> List.singleton

        let indent = "    "

        function
        | DomainType (SingleCaseUnion { Name = name; ConstructorName = "Initiator" }) -> fsType [ sprintf "type %s = Initiator" (name |> TypeName.value) ]
        | DomainType (Record { Name = name; Fields = fields; Methods = methods; Handlers = handlers }) ->
            fsType [
                sprintf "type %s = {" (name |> TypeName.value)
                fields |> Map.toList |> List.concatLines indent (fun (name, field) -> sprintf "%s: %s" (name |> FieldName.value) (field |> TypeDefinition.value))
                methods |> Map.toList |> List.concatLines indent (fun (name, func) -> sprintf "%s: %s -> %s" (name |> FieldName.value) (func.Argument |> TypeDefinition.value) (func.Returns |> TypeDefinition.value))
                handlers |> Map.toList |> List.concatLines indent (fun (name, handler) -> sprintf "%s: %s<%s>" (name |> FieldName.value) (handler.Name |> TypeName.value) (handler.Handles |> TypeDefinition.value))
                "}"
            ]

        | DomainType (Stream { Name = name; EventType = event }) ->
            let streamName = name |> TypeName.value
            let eventName = event |> TypeName.value
            [
                yield eventName |> sprintf "_Event: %s_" |> mdString

                yield! fsType [
                    sprintf "type %s = %s of Stream<%s>" streamName streamName eventName
                ]
            ]

        | DomainType (SingleCaseUnion { Domain = domain; Name = name; ConstructorName = cstrName; ConstructorArgument = arg }) ->
            [
                match domain with
                | Some domain -> yield domain |> DomainName.value |> sprintf "_Domain: %s_" |> mdString
                | _ -> ()

                yield! fsType [
                    sprintf "type %s = %s of %s" (name |> TypeName.value) cstrName (arg |> TypeDefinition.value)
                ]
            ]

        | DomainType (DiscriminatedUnion { Domain = domain; Name = name; Cases = cases }) ->
            [
                match domain with
                | Some domain -> yield domain |> DomainName.value |> sprintf "_Domain: %s_" |> mdString
                | _ -> ()

                yield! fsType [
                    yield sprintf "type %s =" (name |> TypeName.value)
                    yield! cases |> List.map (function
                        | { Name = name; Argument = TypeDefinition.Type (TypeName "unit") } -> sprintf "%s| %s" indent (name |> TypeName.value)
                        | { Name = name; Argument = arg } -> sprintf "%s| %s of %s" indent (name |> TypeName.value) (arg |> TypeDefinition.value)
                    )
                ]
            ]

        | _ -> []

    let private locationRange (location: Tuc.Location) = location.Range |> rangeToProtocolRange |> Some
    let private parsedLocationRange (parsedLocation: ParsedLocation) = parsedLocation.Location |> locationRange

    let private joinSingleLineLocationsRanges: Tuc.Location * Tuc.Location -> Lsp.Range option = function
        | Location.IsSingleLine first, Location.IsSingleLine second when first.Start.Line = second.Start.Line ->
            Some {
                Start = { Line = first.Start.Line; Character = first.Start.Character }
                End = { Line = first.Start.Line; Character = second.End.Character }
            }
        | _ -> None

    let forOperator = function
        | Parsed.ReadData { Operator = operator; OperatorLocation = location }
        | Parsed.PostData { Operator = operator; OperatorLocation = location }
            ->
                Some {
                    Contents = MarkedStrings [|
                        tucLang (operator |> Operator.value)
                        mdString (operator |> Operator.descriptionMarkDown)
                    |]
                    Range = location |> parsedLocationRange
                }
        | _ -> None

    let private formatKeyWord kw =
        MarkedStrings [|
            tucLang (kw |> KeyWord.value)
            mdString (kw |> KeyWord.descriptionMarkDown)
        |]

    let forKeyWord = function
        | Parsed.KeyWord { KeyWord = keyWord; KeyWordLocation = location }
        | Parsed.KeyWordOnly { KeyWord = keyWord; KeyWordLocation = location }
        | Parsed.KeyWordWithBody { KeyWord = keyWord; KeyWordLocation = location }
        | Parsed.KeyWordWithoutValue { KeyWord = keyWord; KeyWordLocation = location }
        | Parsed.KeyWordIf { IfKeyWord = keyWord; IfLocation = location } ->
            Some {
                Contents = keyWord |> formatKeyWord
                Range = location |> parsedLocationRange
            }
        | Parsed.ParticipantDefinition { Alias = Some (location, _) } ->
            Some {
                Contents = KeyWord.Alias |> formatKeyWord
                Range = location |> parsedLocationRange
            }
        | _ -> None

    let forElse = function
        | Parsed.KeyWordIf { ElseKeyWord = Some keyWord; ElseLocation = Some location } ->
            Some {
                Contents = keyWord |> formatKeyWord
                Range = location |> parsedLocationRange
            }
        | _ -> None

    let forKeyWordValue = function
        | Parsed.KeyWord { KeyWord = keyWord; KeyWordLocation = keyWordLocation; ValueLocation = { Value = value; Location = location } }
        | Parsed.KeyWordWithBody { KeyWord = keyWord; KeyWordLocation = keyWordLocation; ValueLocation = { Value = value; Location = location } }
        | Parsed.KeyWordIf { IfKeyWord = keyWord; IfLocation = keyWordLocation; ConditionLocation = { Value = value; Location = location } } ->
            Some {
                Contents = MarkedStrings [|
                    tucLang (sprintf "%s %s" (keyWord |> KeyWord.value) value)
                    mdString (keyWord |> KeyWord.descriptionMarkDown)
                |]
                Range = (keyWordLocation.Location, location) |> joinSingleLineLocationsRanges
            }
        | _ -> None

    let private singleParticipant parts =
        parts
        |> String.concat " "
        |> sprintf "participants\n  %s"
        |> tucLang

    let forParticipantDefinition: ParsedParticipant -> _ = function
        | Parsed.ParticipantDefinition { Context = context; Domain = domain; Alias = alias; Component = parentComponent; Value = value } ->
            Some {
                Contents =
                    MarkedStrings [|
                        yield singleParticipant [
                            yield context.Value

                            match domain with
                            | Some { Value = domain } -> yield domain
                            | _ -> ()

                            match alias with
                            | Some ({ Value = keyWord }, { Value = alias }) ->
                                yield keyWord
                                yield alias
                            | _ -> ()
                        ]

                        match domain, parentComponent with
                        | Some { Value = domain }, _ -> yield sprintf "_Domain: %s_" domain |> mdString
                        | _, Some { Domain = domain } -> yield sprintf "_Domain: %s_" (domain |> DomainName.value) |> mdString
                        | _ -> ()

                        match parentComponent with
                        | Some { Context = context; Domain = domain } -> yield sprintf "_Component: %s_.**%s**" (domain |> DomainName.value) context |> mdString
                        | _ -> ()

                        match value with
                        | Participant (ActiveParticipant.Service service) -> yield! service.ServiceType |> domainTypeToFS
                        | Participant (ActiveParticipant.Stream stream) -> yield! stream.StreamType |> domainTypeToFS
                        | Participant (ActiveParticipant.DataObject dataObject) -> yield! dataObject.DataObjectType |> domainTypeToFS
                        | _ -> ()
                    |]
                Range =
                    match domain, alias with
                    | None, None -> context |> parsedLocationRange
                    | Some domain, None -> (context.Location, domain.Location) |> joinSingleLineLocationsRanges
                    | _, Some (_, alias) -> (context.Location, alias.Location) |> joinSingleLineLocationsRanges
            }
        | Parsed.ComponentDefinition { Context = context; Domain = domain; Value = Component { Participants = participants; Type = componentType } } ->
            Some {
                Contents =
                    MarkedStrings [|
                        yield singleParticipant [
                            context.Value
                            domain.Value
                        ]
                        yield sprintf "_Domain: %s_" domain.Value |> mdString

                        yield
                            participants
                            |> List.concatLines " * " (function
                                | ActiveParticipant.Service { Context = context; Domain = domain }
                                | ActiveParticipant.Stream { Context = context; Domain = domain }
                                | ActiveParticipant.DataObject { Context = context; Domain = domain }
                                    -> sprintf "_%s_.**%s**" (domain |> DomainName.value) context
                            )
                            |> (+) "_Participants_:\n"
                            |> mdString

                        yield! componentType |> domainTypeToFS
                    |]
                Range = (context.Location, domain.Location) |> joinSingleLineLocationsRanges
            }
        | _ -> None

    let forParticipantActivation: ParsedTucPart -> _ = function
        | Parsed.Lifeline { ParticipantLocation = participantLocation; Value = Lifeline { Initiator = activeParticipant }}
        | Parsed.MethodCall { ServiceLocation = participantLocation; Value = ServiceMethodCall { Service = activeParticipant }}
        | Parsed.HandleEvent { ServiceLocation = participantLocation; Value = HandleEventInStream { Service = activeParticipant }}
        | Parsed.ReadData { DataObjectLocation = participantLocation; Value = ReadData { DataObject = activeParticipant }}
        | Parsed.PostData { DataObjectLocation = participantLocation; Value = PostData { DataObject = activeParticipant }}
        | Parsed.ReadData { DataObjectLocation = participantLocation; Value = ReadEvent { Stream = activeParticipant }}
        | Parsed.PostData { DataObjectLocation = participantLocation; Value = PostEvent { Stream = activeParticipant }}
            ->
                Some {
                    Contents =
                        MarkedStrings [|
                            match activeParticipant with
                            | ActiveParticipant.Service service ->
                                yield singleParticipant [
                                    yield activeParticipant |> ActiveParticipant.value
                                    yield service.Domain |> DomainName.value

                                    if service.Alias <> service.Context then
                                        yield "as"
                                        yield sprintf "%A" service.Alias
                                ]

                            | ActiveParticipant.Stream stream ->
                                yield singleParticipant [
                                    yield activeParticipant |> ActiveParticipant.value
                                    yield stream.Domain |> DomainName.value

                                    if stream.Alias <> stream.Context then
                                        yield "as"
                                        yield sprintf "%A" stream.Alias
                                ]

                            | ActiveParticipant.DataObject dataObject ->
                                yield singleParticipant [
                                    yield activeParticipant |> ActiveParticipant.value
                                    yield dataObject.Domain |> DomainName.value

                                    if dataObject.Alias <> dataObject.Context then
                                        yield "as"
                                        yield sprintf "%A" dataObject.Alias
                                ]

                            match activeParticipant with
                            | ActiveParticipant.Service { Domain = domain }
                            | ActiveParticipant.Stream { Domain = domain }
                            | ActiveParticipant.DataObject { Domain = domain }
                                -> yield domain |> DomainName.value |> sprintf "_Domain: %s_" |> mdString

                            // yield mdString "_Tuc: Lifeline_" // todo - if needed, it should have a value of Parsed.{...}

                            match activeParticipant with
                            | ActiveParticipant.Service service -> yield! service.ServiceType |> domainTypeToFS
                            | ActiveParticipant.Stream stream -> yield! stream.StreamType |> domainTypeToFS
                            | ActiveParticipant.DataObject dataObject -> yield! dataObject.DataObjectType |> domainTypeToFS
                        |]
                    Range = participantLocation |> parsedLocationRange
                }
        | _ -> None

    let forMethod: ParsedTucPart -> _ = function
        | Parsed.MethodCall { MethodLocation = methodLocation; Value = ServiceMethodCall { Service = service; Method = method }} ->
            Some {
                Contents =
                    MarkedStrings [|
                        service
                        |> ActiveParticipant.name
                        |> sprintf "_Service: %s_"
                        |> mdString

                        sprintf "type %s = %s -> %s"
                            (method.Name |> FieldName.value)
                            (method.Function.Argument |> TypeDefinition.value)
                            (method.Function.Returns |> TypeDefinition.value)
                        |> fsLang
                    |]
                Range = methodLocation |> parsedLocationRange
            }
        | Parsed.HandleEvent { MethodLocation = methodLocation; Value = HandleEventInStream { Stream = stream; Service = service; Handler = handler }} ->
            Some {
                Contents =
                    MarkedStrings [|
                        stream
                        |> ActiveParticipant.name
                        |> sprintf "_Stream: %s_"
                        |> mdString

                        service
                        |> ActiveParticipant.name
                        |> sprintf "_Handler: %s_"
                        |> mdString

                        sprintf "type %s = %s<%s>"
                            (handler.Name |> FieldName.value)
                            (handler.Handler.Name |> TypeName.value)
                            (handler.Handler.Handles |> TypeDefinition.value)
                        |> fsLang
                    |]
                Range = methodLocation |> parsedLocationRange
            }
        | _ -> None

    let forData current: ParsedTucPart -> _ =
        let dataPath index data =
            data
            |> Data.path
            |> List.mapi (fun i value ->
                if i = index then sprintf "**%s**" value
                else value
            )
            |> String.concat "."

        function
        | Parsed.ReadData { DataLocation = dataLocation; Value = ReadData { Data = data; DataObject = dataObject }}
        | Parsed.PostData { DataLocation = dataLocation; Value = PostData { Data = data; DataObject = dataObject }}
            ->
                let location = dataLocation.[current]
                let currentValue = location.Value.Trim '.'

                Some {
                    Contents = MarkedStrings [|
                        yield mdString (sprintf "_DataObject: %s_" (dataObject |> ActiveParticipant.name))
                        yield mdString (sprintf "_Data: %s_" (data |> dataPath current))

                        match data |> Data.case (current, currentValue) with
                        | Some case -> yield! case |> domainTypeToFS
                        | _ -> ()
                    |]
                    Range = location |> parsedLocationRange
                }

        | Parsed.ReadData { DataLocation = dataLocation; Value = ReadEvent { Event = Event data; Stream = stream }}
        | Parsed.PostData { DataLocation = dataLocation; Value = PostEvent { Event = Event data; Stream = stream }}
            ->
                let location = dataLocation.[current]
                let currentValue = location.Value.Trim '.'

                Some {
                    Contents = MarkedStrings [|
                        yield mdString (sprintf "_Stream: %s_" (stream |> ActiveParticipant.name))
                        yield mdString (sprintf "_Event: %s_" (data |> dataPath current))

                        match data |> Data.case (current, currentValue) with
                        | Some case -> yield! case |> domainTypeToFS
                        | _ -> ()
                    |]
                    Range = location |> parsedLocationRange
                }
        | _ -> None
