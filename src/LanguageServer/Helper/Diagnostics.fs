namespace Tuc.LanguageServer

[<RequireQualifiedAccess>]
module Diagnostics =
    open Tuc
    open Tuc.Domain
    open Tuc.Parser
    open LspHelpers

    module Lsp = LanguageServerProtocol.Types
    open Lsp

    let forParseError error =
        let diagnostic line startPos endPos (message: string) =
            {
                Range = {
                    Start = { Line = line - 1; Character = startPos }
                    End = { Line = line - 1; Character = endPos }
                }
                Severity = Some DiagnosticSeverity.Error
                Source = "Tuc.Parser"
                Message = message + "\n"
                Code = Some (error |> ParseError.errorName)
                RelatedInformation = Some [||]
                Tags = None
            }

        match error with
        // Tuc file
        | MissingTucName ->
            "There is no tuc name defined."
            |> diagnostic 1 0 5

        | TucMustHaveName (lineNumber, position, _line) ->
            "Tuc must have a name."
            |> diagnostic lineNumber position (position + "tuc ".Length)

        | MissingParticipants ->
            "There are no participants defined in the tuc file. (Or the \"participants\" keyword is wrongly written or indented)"
            |> diagnostic 2 0 "participants".Length   // todo<asap> - add line (for multi-tuc file)

        | MissingIndentation ->
            "There are no indented line in the tuc file."
            |> diagnostic 1 0 5

        | WrongIndentationLevel (indentationLevel, lines) ->
            sprintf "There is a wrong indentation level on these lines. (It should be multiples of %d leading spaces, which is based on the first indented line in the tuc file):%s"
                indentationLevel
                (lines |> List.formatLines "" id)
            |> diagnostic 1 0 5

        | TooMuchIndented (lineNumber, position, line) ->
            "This line is too much indented from the current context."
            |> diagnostic lineNumber position line.Length  // todo - fix end position, it would be wrong with comment

        // Participants

        | WrongParticipantIndentation (lineNumber, position, line) ->
            "Participant is wrongly indented. (It is probably indented too much)"
            |> diagnostic lineNumber position line.Length   // todo - fix end position, it would be wrong with comment

        | ComponentWithoutParticipants (lineNumber, position, _line, componentName) ->
            componentName
            |> sprintf "Component %A must have its participants defined, there are none here. (Or they are not indented maybe?)"
            |> diagnostic lineNumber position (position + componentName.Length)

        | UndefinedComponentParticipant (lineNumber, position, _line, componentName, definedFields, wantedService) ->
            let error =
                sprintf "This participant is not defined as one of the field of the component %s." componentName

            let formattedFields =
                definedFields
                |> List.formatAvailableItems
                    "There are no defined fields"
                    (List.formatLines "  - " id)
                    wantedService

            sprintf "%s\n\nComponent %s has defined fields: %s"
                error
                componentName
                formattedFields
            |> diagnostic lineNumber position (position + wantedService.Length)

        | UndefinedComponentParticipantInDomain (lineNumber, position, _line, domain, participant) ->
            domain
            |> sprintf "There is an undefined component participant in the %s domain. (It is not defined in the given Domain types, or it is not defined as a Record.)"
            |> diagnostic lineNumber position (position + participant.Length)

        | WrongComponentParticipantDomain (lineNumber, position, _line, componentDomain, participant) ->
            sprintf "This participant is not defined in the component's domain %s, or it has other domain defined." componentDomain
            |> diagnostic lineNumber position (position + participant.Length)

        | InvalidParticipant (lineNumber, position, line) ->
            "There is an invalid participant. Participant format is: ServiceName Domain (as \"alias\") (Alias part is optional)"
            |> diagnostic lineNumber position line.Length   // todo - fix end position, it would be wrong with comment

        | UndefinedParticipantInDomain (lineNumber, position, _line, domain, participant) ->
            domain
            |> sprintf "There is an undefined participant in the %s domain. (It is not defined in the given Domain types, or it is not defined as a Record.)"
            |> diagnostic lineNumber position (position + participant.Length)

        | UndefinedParticipant (lineNumber, position, _line, participant) ->
            "There is an undefined participant. (It is not defined in the given Domain types, or it is not defined as a Record.)"
            |> diagnostic lineNumber position (position + participant.Length)

        // Parts

        | MissingUseCase (lineNumber, TucName name) ->
            name
            |> sprintf "There is no use-case defined in the tuc %s."
            |> diagnostic (lineNumber + 1) 0 ("tuc ".Length + name.Length)

        | SectionWithoutName (lineNumber, _position, _line) ->
            "Section must have a name."
            |> diagnostic lineNumber 0 "section ".Length

        | IsNotInitiator (lineNumber, position, _line, service) ->
            "Only Initiator service can have a lifeline."
            |> diagnostic lineNumber position (position + service.Length)

        | MethodCalledWithoutACaller (lineNumber, position, _line, service, method) ->
            "Method can be called only in the lifeline of a caller."
            |> diagnostic lineNumber position (position + service.Length + ".".Length + method.Length)

        | DataPostedWithoutACaller (lineNumber, position, _line, data, dataObject) ->
            "Data can be posted only in the lifeline of a caller."
            |> diagnostic lineNumber position (position + data.Length + " -> ".Length + dataObject.Length)

        | DataReadWithoutACaller (lineNumber, position, _line, dataObject, data) ->
            "Data can be read only in the lifeline of a caller."
            |> diagnostic lineNumber position (position + dataObject.Length + " -> ".Length + data.Length)

        | EventPostedWithoutACaller (lineNumber, position, _line, event, stream) ->
            "Event can be posted only in the lifeline of a caller."
            |> diagnostic lineNumber position (position + event.Length + " -> ".Length + stream.Length)

        | EventReadWithoutACaller (lineNumber, position, _line, stream, event) ->
            "Event can be read only in the lifeline of a caller."
            |> diagnostic lineNumber position (position + stream.Length + " -> ".Length + event.Length)

        | CalledUndefinedMethod (lineNumber, position, _line, serviceName, definedMethods, method) ->
            let position = position + serviceName.Length + ".".Length
            let error =
                sprintf "There is an undefined method called on the service %s." serviceName

            let serviceHas =
                sprintf "Service %s has" serviceName

            let definedMethods =
                match definedMethods with
                | [] -> sprintf "%s not defined any methods." serviceHas
                | definedMethods ->
                    sprintf "%s defined methods:%s"
                        serviceHas
                        (definedMethods |> List.formatLines "  - " id)

            sprintf "%s\n\n%s" error definedMethods
            |> diagnostic lineNumber position (position + method.Length)

        | CalledUndefinedHandler (lineNumber, position, _line, serviceName, definedHandlers, handler) ->
            let position = position + serviceName.Length + ".".Length
            let error =
                sprintf "There is an undefined handler called on the service %s." serviceName

            let serviceHas =
                sprintf "Service %s has" serviceName

            let definedHandlers =
                match definedHandlers with
                | [] -> sprintf "%s not defined any handlers." serviceHas
                | definedHandlers ->
                    sprintf "%s defined handlers:%s"
                        serviceHas
                        (definedHandlers |> List.formatLines "  - " id)

            sprintf "%s\n\n%s" error definedHandlers
            |> diagnostic lineNumber position (position + handler.Length)

        | MissingEventHandlerMethodCall (lineNumber, position, _line, stream) ->
            "There must be exactly one handler call which handles the stream. (It must be on the subsequent line, indented by one level)"
            |> diagnostic (lineNumber - 1) position (position + stream.Length)

        | InvalidMultilineNote (lineNumber, position, _line) ->
            "Invalid multiline note. (It must start and end on the same level with \"\"\")"
            |> diagnostic lineNumber position (position + "\"\"\"".Length)

        | InvalidMultilineLeftNote (lineNumber, position, _line) ->
            "Invalid multiline left note. (It must start and end on the same level with \"<\")"
            |> diagnostic lineNumber position (position + "\"<\"".Length)

        | InvalidMultilineRightNote (lineNumber, position, _line) ->
            "Invalid multiline right note. (It must start and end on the same level with \">\")"
            |> diagnostic lineNumber position (position + "\">\"".Length)

        | DoMustHaveActions (lineNumber, position, _line) ->
            "Do must have an action on the same line, or there must be at least one action indented on the subsequent line."
            |> diagnostic lineNumber position (position + "do".Length)

        | DoWithoutACaller (lineNumber, position, _line) ->
            "Do can be only in the lifeline of a caller."
            |> diagnostic lineNumber position (position + "do".Length)

        | IfWithoutCondition (lineNumber, position, _line) ->
            "If must have a condition."
            |> diagnostic lineNumber position (position + "if ".Length)

        | IfMustHaveBody (lineNumber, position, _line) ->
            "If must have a body. (It must be indented)"
            |> diagnostic lineNumber position (position + "if".Length)

        | ElseOutsideOfIf (lineNumber, position, _line) ->
            "There must be an If before an Else."
            |> diagnostic lineNumber position (position + "else".Length)

        | ElseMustHaveBody (lineNumber, position, _line) ->
            "Else must have a body. (It must be indented)"
            |> diagnostic lineNumber position (position + "else".Length)

        | GroupWithoutName (lineNumber, position, _line) ->
            "Group must have a name."
            |> diagnostic lineNumber position (position + "group ".Length)

        | GroupMustHaveBody (lineNumber, position, _line) ->
            "Group must have a body. (It must be indented)"
            |> diagnostic lineNumber position (position + "group".Length)

        | LoopWithoutCondition (lineNumber, position, _line) ->
            "Loop must have a condition."
            |> diagnostic lineNumber position (position + "loop ".Length)

        | LoopMustHaveBody (lineNumber, position, _line) ->
            "Loop must have a body. (It must be indented)"
            |> diagnostic lineNumber position (position + "loop".Length)

        | NoteWithoutACaller (lineNumber, position, line) ->
            "Note can be only in the lifeline of a caller."
            |> diagnostic lineNumber position line.Length

        | UnknownPart (lineNumber, position, line) ->
            "There is an unknown part or an undefined participant. (Or wrongly indented)"
            |> diagnostic lineNumber position (line.Length - position)

        // others

        | WrongEventName (lineNumber, position, _line, message, event) ->
            sprintf "There is a wrong event - %s." message
            |> diagnostic lineNumber position (position + event.Length)

        | WrongDataName (lineNumber, position, line, message, data) ->
            sprintf "There is a wrong data - %s." message
            |> diagnostic lineNumber position (position + data.Length)

        | WrongEvent (lineNumber, position, line, cases, event) ->
            let error = "There is no such a case for an event."

            let defineCases =
                match cases with
                | [] -> "Event does not have defined any more cases."
                | cases ->
                    sprintf "Event has defined cases:%s"
                        (cases |> List.formatLines "  - " id)

            sprintf "%s\n\n%s" error defineCases
            |> diagnostic lineNumber position (position + event.Length)

        | WrongData (lineNumber, position, line, cases, data) ->
            let error = "There is no such a case for a data."

            let defineCases =
                match cases with
                | [] -> "Data does not have defined any more cases."
                | cases ->
                    sprintf "Data has defined cases:%s"
                        (cases |> List.formatLines "  - " id)

            sprintf "%s\n\n%s" error defineCases
            |> diagnostic lineNumber position (position + data.Length)
