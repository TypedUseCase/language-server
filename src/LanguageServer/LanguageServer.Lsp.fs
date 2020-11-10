namespace Tuc.LanguageServer

module Lsp =

    open Tuc.Domain
    //open Argu
    //open FsAutoComplete
    open FsLibLog
    //open FsAutoComplete.Logging
    //open FsAutoComplete.Utils
    //open FSharp.Compiler.SourceCodeServices
    open LanguageServerProtocol
    open LanguageServerProtocol.LspResult
    open LanguageServerProtocol.Server
    open LanguageServerProtocol.Types
    open LspHelpers
    open TucHelpers
    open Newtonsoft.Json.Linq
    //open ProjectSystem
    open System
    open System.IO
    open ErrorHandling

    module FcsRange = FSharp.Compiler.Range

    //open FSharp.Analyzers

    type TucLspClient(sendServerRequest: ClientNotificationSender) =
        inherit LspClient ()

        override __.WindowShowMessage(p) =
            sendServerRequest "window/showMessage" (box p) |> Async.Ignore

        override __.WindowLogMessage(p) =
            sendServerRequest "window/logMessage" (box p) |> Async.Ignore

        override __.TextDocumentPublishDiagnostics(p) =
            sendServerRequest "textDocument/publishDiagnostics" (box p) |> Async.Ignore

        ///Custom notification for workspace/solution/project loading events
        member __.NotifyWorkspace (p: PlainNotification) =
            sendServerRequest "fsharp/notifyWorkspace" (box p) |> Async.Ignore

        ///Custom notification for initial workspace peek
        member __.NotifyWorkspacePeek (p: PlainNotification) =
            sendServerRequest "fsharp/notifyWorkspacePeek" (box p) |> Async.Ignore

        member __.NotifyCancelledRequest (p: PlainNotification) =
            sendServerRequest "fsharp/notifyCancel" (box p) |> Async.Ignore

        member __.NotifyFileParsed (p: PlainNotification) =
            sendServerRequest "fsharp/fileParsed" (box p) |> Async.Ignore

        member __.NotifyTucFileParsed (p: PlainNotification) =
            sendServerRequest "tuc/fileParsed" (box p) |> Async.Ignore

        // TODO: Add the missing notifications
        // TODO: Implement requests

    type TucLspServer(output: MF.ConsoleApplication.Output, commands: Commands, lspClient: TucLspClient) =
        inherit LspServer()

        let logger = LogProvider.getLoggerByName "LSP"
        // let fantomasLogger = LogProvider.getLoggerByName "Fantomas"

        let mutable clientCapabilities: ClientCapabilities option = None
        // let mutable glyphToCompletionKind = glyphToCompletionKindGenerator None
        // let mutable glyphToSymbolKind = glyphToSymbolKindGenerator None
        //let subscriptions = ResizeArray<IDisposable>()

        let mutable config = FSharpConfig.Default
        let mutable rootPath : string option = None

        let mutable domainTypes: DomainType list = []   // todo - move to state?

        let mutable currentDoc: string option = None

        /// centralize any state changes when the config is updated here
        let updateConfig (newConfig: FSharpConfig) =
            let toCompilerToolArgument (path: string) = sprintf "--compilertool:%s" path
            config <- newConfig

        //TODO: Thread safe version
        // let fixes = System.Collections.Generic.Dictionary<DocumentUri, (LanguageServerProtocol.Types.Range * TextEdit) list>()
        // let analyzerFixes = System.Collections.Generic.Dictionary<(DocumentUri * string), (LanguageServerProtocol.Types.Range * TextEdit) list>()

        let logInfo message =
            logger.info (Log.setMessage message)

        ///Helper function for handling file requests using **recent** type check results
        //member x.fileHandler<'a> (f: SourceFilePath -> ParseAndCheckResults -> string [] -> AsyncLspResult<'a>) (file: SourceFilePath) : AsyncLspResult<'a> =

        override __.Initialize(p: InitializeParams) = async {
            logInfo <| sprintf "LanguageServer.Lsp.initialize: %A ..." p.RootUri

            let actualRootPath =
                match p.RootUri with
                | Some rootUri -> Some (Path.fileUriToLocalPath rootUri)
                | None -> p.RootPath

            rootPath <- actualRootPath
            //commands.SetWorkspaceRoot actualRootPath
            clientCapabilities <- p.Capabilities
            //glyphToCompletionKind <- glyphToCompletionKindGenerator clientCapabilities
            //glyphToSymbolKind <- glyphToSymbolKindGenerator clientCapabilities

            domainTypes <- commands.ResolveDomainTypes actualRootPath

            return
                { InitializeResult.Default with
                    Capabilities =
                        { ServerCapabilities.Default with
                            HoverProvider = Some true
                            // RenameProvider = Some true
                            // DefinitionProvider = Some true
                            // TypeDefinitionProvider = Some true
                            // ImplementationProvider = Some true
                            // ReferencesProvider = Some true
                            // DocumentHighlightProvider = Some true
                            // DocumentSymbolProvider = Some true
                            WorkspaceSymbolProvider = Some true
                            // DocumentFormattingProvider = Some true
                            // DocumentRangeFormattingProvider = Some false
                            // SignatureHelpProvider = Some {
                            //     SignatureHelpOptions.TriggerCharacters = Some [| "("; ","|]
                            // }
                            CompletionProvider =
                                Some {
                                    ResolveProvider = Some true
                                    TriggerCharacters = Some ([| "."; |])
                                }
                            // CodeLensProvider = Some {
                            //     CodeLensOptions.ResolveProvider = Some true
                            // }
                            // CodeActionProvider = Some true
                            TextDocumentSync =
                                Some {
                                    TextDocumentSyncOptions.Default with
                                        OpenClose = Some true
                                        Change = Some TextDocumentSyncKind.Full
                                        Save = Some { IncludeText = Some true }
                                }
                            //FoldingRangeProvider = Some true
                            //SelectionRangeProvider = Some true
                        }
                }
                |> success
        }

        override __.Initialized(p: InitializedParams) = async {
            logInfo <| sprintf "Initialized with %A Domain Types." (domainTypes |> List.length)

            return ()
        }

        ///Helper function for handling Position requests using **recent** type check results
        member x.positionHandler<'a, 'b when 'b :> ITextDocumentPositionParams> (empty: 'a) (f: 'b -> Tuc.Position -> TucSegment option ->  AsyncLspResult<'a>) (arg: 'b) : AsyncLspResult<'a> =
            async {
                let file = arg.GetFilePath()
                let pos = arg.GetTucPosition()
                logger.info (Log.setMessage "PositionHandler - Position request: {file} at {pos}" >> Log.addContextDestructured "file" file >> Log.addContextDestructured "pos" pos)

                match arg.GetLanguageId() with
                | Some "tuc" ->
                    return!
                        match commands.TryGetLineSegment(file, pos) with
                        | ResultOrString.Error s ->
                            logger.error (Log.setMessage "PositionHandler - Getting file for {file} failed due to {error}" >> Log.addContextDestructured "error" s >> Log.addContextDestructured "file" file)
                            AsyncLspResult.internalError s
                        | ResultOrString.Ok (segment) ->
                            try
                                async {
                                    let! r = Async.Catch (f arg pos segment)
                                    match r with
                                    | Choice1Of2 r -> return r
                                    | Choice2Of2 e ->
                                        logger.error (Log.setMessage "PositionHandler - Failed during child operation on file {file}" >> Log.addContextDestructured "file" file >> Log.addExn e)
                                        return LspResult.internalError e.Message
                                }
                            with e ->
                                logger.error (Log.setMessage "PositionHandler - Operation failed for file {file}" >> Log.addContextDestructured "file" file >> Log.addExn e)
                                AsyncLspResult.internalError e.Message
                | _ -> return success empty
            }

        // todo<tuc> - handle when tuc will be generated by extension
        (* override __.WorkspaceExecuteCommand(p) = async {
            logInfo <| sprintf "WorkspaceExecuteCommand with %A" p
        } *)

        override __.TextDocumentDidOpen(p: DidOpenTextDocumentParams) = async {
            logInfo <| sprintf "TextDocumentDidOpen %A" { p with TextDocument = { p.TextDocument with Text = "..." } }

            currentDoc <- Some (p.TextDocument.GetFilePath())

            match p.TextDocument.LanguageId with
            | "tuc" ->
                do! p.TextDocument |> commands.ParseTucs domainTypes
                do! lspClient.NotifyTucFileParsed({ Content = p.TextDocument.GetFilePath() })
            | "fsharp" -> logInfo "F# language"
            | _ -> ()
        }

        override __.TextDocumentDidChange(p: DidChangeTextDocumentParams) = async {
            logInfo <| sprintf "TextDocumentDidChange %A" p.TextDocument

            // todo - zmenit cache aktualniho souboru pri zmene, bude to trochu tezsi, protoze je tady jen verzovany dokument a ten funguje jinak...
            (* match p.TextDocument.LanguageId with
            | "tuc" ->
                let parsedTucs = p.TextDocument |> commands.ParseTucs domainTypes
                // todo - cache parsed tucs for a file

                ()

            | "fsharp" -> logInfo "F# language"
            | _ -> () *)

            return ()
        }

        override x.TextDocumentHover(p: TextDocumentPositionParams) =
            logInfo <| sprintf "TextDocumentHover %A" p.TextDocument
            let emptyResult = Some { Contents = MarkedStrings [||]; Range = None }

            match p.GetLanguageId() with
            | Some "tuc" ->
                p |> x.positionHandler emptyResult (fun p pos segment ->
                    async {
                        logger.info (Log.setMessage "Hover at {position}" >> Log.addContextDestructured "position" pos )

                        return
                            match segment with
                            | Some { Hover = Some hover } -> success (Some hover)
                            | _ -> success emptyResult   // todo - for an empty respons: success None, it returns with an LSP error
                    }
                )
            | _ -> success emptyResult |> Async.retn

        override __.TextDocumentDefinition(p: TextDocumentPositionParams) = async {
            // todo - uncomment some server capabilities? - this is for go to definition
            logInfo <| sprintf "TextDocumentDefinition %A" p.TextDocument

            return success None
        }

        override __.TextDocumentReferences(p: ReferenceParams) = async {
            // todo - uncomment some server capabilities? - this is for finding all references of something in the project
            logInfo <| sprintf "TextDocumentReferences %A" p.TextDocument

            return success None
        }

        override x.TextDocumentDidSave(p: DidSaveTextDocumentParams) = async {
            logInfo <| sprintf "TextDocumentDidSave %A" { p with Text = None }

            // todo - tady je zase jen Identifier na text doc, ktery ma jen URI, takze bud si nekde ukladat (pri open?) cache o souborech podle URI, kde bude vsechno a pak to vytahovat
            // nebo kouknout jeste, jestli to tam opravdu neni a jen to v F# neni naimplementovane

            match p.TextDocument.GetLanguageId() with
            | Some "tuc" ->
                let path = p.TextDocument.GetFilePath()

                do! path |> commands.ParseTucsForFile domainTypes
                do! lspClient.NotifyTucFileParsed({ Content = path })

            | Some "fsharp" -> domainTypes <- commands.ResolveDomainTypes rootPath
            | _ -> ()
        }

        override __.WorkspaceDidChangeWatchedFiles(p) = async {
            logger.info (Log.setMessage "WorkspaceDidChangeWatchedFiles Request: {parms}" >> Log.addContextDestructured "parms" p )
            domainTypes <- commands.ResolveDomainTypes rootPath
        }

        override __.WorkspaceDidChangeWorkspaceFolders(p) = async {
            logger.info (Log.setMessage "WorkspaceDidChangeWorkspaceFolders Request: {parms}" >> Log.addContextDestructured "parms" p )
        }

        (* override __.TextDocumentCodeLens(p) = async {
            logger.info (Log.setMessage "TextDocumentCodeLens Request: {parms}" >> Log.addContextDestructured "parms" p )
            // todo - show types in code lens for a method calls
            let typ =
                sprintf "// Domain Types: %d | Tuc Segments: %d"
                    (domainTypes |> List.length)
                    (p.TextDocument.GetFilePath() |> commands.SegmentsCount)

            let cl = [|
                {
                    Command = Some { Title = typ; Command = None; Arguments = None }
                    Data = None
                    Range = {
                        Start = { Line = 0; Character = 0 }
                        End = { Line = 0; Character = typ.Length }
                    }
                }
            |]

            return success (Some cl)
        }

        override __.CodeLensResolve(cl) = async {
            logInfo <| sprintf "CodeLensResolve %A" cl
            // todo - show types in code lens for a method calls

            return success cl
        } *)

        (* override __.TextDocumentCodeAction(c) = async {
            logInfo <| sprintf "TextDocumentCodeAction %A" c

            return success (Some <| TextDocumentCodeActionResult.CodeActions [||])
        } *)

        override x.TextDocumentCompletion(p: CompletionParams) =
            logger.info (Log.setMessage "TextDocumentCompletion Request: {context}" >> Log.addContextDestructured "context" p)
            let emptyResult = Some { IsIncomplete = true; Items = [||] }

            // todo:
            // - kdyz je trigger character null, tak je to invoke na ctrl+space (nabizi vsechno, i kw)
            // - kdyz je trigger character ., tak je to prirozene pri psani -> Servise -> . -> napovida...
            // - problem asi bude, ze tuc.parser asi nezvladne servisu, eventy,... kdyz konci teckou, bude to brat jako chybu, takze nebudou k dispozici segmenty
            // - poresit taky verze souboru, cekani, ...
            // - jinak muzou byt rovnou tak jako Hover = Hover option, i CompletionItems = CompletionItem list, ktere bude "predpripravene" a rovnou ve statu

            match p.GetLanguageId() with
            | Some "tuc" ->
                p |> x.positionHandler emptyResult (fun p pos segment -> async {
                    let trigger =
                        match p.Context with
                        | Some { triggerCharacter = (Some ".") } -> CompletionTrigger.Dot
                        | Some { triggerCharacter = (Some t) } -> CompletionTrigger.Other t
                        | _ -> CompletionTrigger.CtrlSpace

                    logger.info (
                        Log.setMessage "TextDocumentCompletion: Position {position} | Segment {segment} | Trigger {trigger}"
                        >> Log.addContextDestructured "position" pos
                        >> Log.addContextDestructured "segment" segment
                        >> Log.addContextDestructured "trigger" trigger
                    )

                    let ci =
                        {
                            IsIncomplete = false;
                            Items =
                                segment
                                |> Option.map (TucSegment.completionItem)
                                |> Option.defaultValue (trigger |> commands.FindDefaultCompletionItems (p.GetFilePath(), pos))
                        }

                    return success (Some ci)
                })
            | _ -> success emptyResult |> Async.retn

        override __.CompletionItemResolve(ci) = async {
            let ci =
                match ci with
                | { Data = Some data } ->
                    let docs = data.ToObject<string>()

                    { ci with Documentation = Some (Documentation.Markup (markdown docs)) }
                | _ -> ci

            return success ci
        }

        member __.Info(p: PlainNotification) = async {
            logger.info (Log.setMessage "Tuc.Info - {params}" >> Log.addContextDestructured "params" p)

            let info =
                [
                    "Domain Types", domainTypes |> List.length |> string
                    "Segments", commands.SegmentsCount p.Content |> string
                ]
                |> List.map (fun (k, v) -> sprintf "%s: %s" k v)
                |> String.concat " | "

            logInfo info

            return success { Content = info }
        }

    let startCore consoleOutput (commands : Commands) =
        use input = Console.OpenStandardInput()
        use output = Console.OpenStandardOutput()

        let requestsHandlings =
            defaultRequestHandlings<TucLspServer> ()
            //|> Map.add "fsharp/signature" (requestHandling (fun s p -> s.FSharpSignature(p) ))
            //|> Map.add "fsharp/signatureData" (requestHandling (fun s p -> s.FSharpSignatureData(p) ))
            //|> Map.add "fsharp/documentationGenerator" (requestHandling (fun s p -> s.FSharpDocumentationGenerator(p) ))
            //|> Map.add "fsharp/lineLens" (requestHandling (fun s p -> s.FSharpLineLense(p) ))
            //|> Map.add "fsharp/compilerLocation" (requestHandling (fun s p -> s.FSharpCompilerLocation(p) ))
            //|> Map.add "fsharp/compile" (requestHandling (fun s p -> s.FSharpCompile(p) ))
            //|> Map.add "fsharp/workspaceLoad" (requestHandling (fun s p -> s.FSharpWorkspaceLoad(p) ))
            //|> Map.add "fsharp/workspacePeek" (requestHandling (fun s p -> s.FSharpWorkspacePeek(p) ))
            //|> Map.add "fsharp/project" (requestHandling (fun s p -> s.FSharpProject(p) ))
            //|> Map.add "fsharp/fsdn" (requestHandling (fun s p -> s.FSharpFsdn(p) ))
            //|> Map.add "fsharp/dotnetnewlist" (requestHandling (fun s p -> s.FSharpDotnetNewList(p) ))
            //|> Map.add "fsharp/dotnetnewrun" (requestHandling (fun s p -> s.FSharpDotnetNewRun(p) ))
            //|> Map.add "fsharp/f1Help" (requestHandling (fun s p -> s.FSharpHelp(p) ))
            //|> Map.add "fsharp/documentation" (requestHandling (fun s p -> s.FSharpDocumentation(p) ))
            //|> Map.add "fsharp/documentationSymbol" (requestHandling (fun s p -> s.FSharpDocumentationSymbol(p) ))
            //|> Map.add "fsharp/loadAnalyzers" (requestHandling (fun s p -> s.LoadAnalyzers(p) ))
            //|> Map.add "fsharp/highlighting" (requestHandling (fun s p -> s.GetHighlighting(p) ))
            //|> Map.add "fsharp/fsharpLiterate" (requestHandling (fun s p -> s.FSharpLiterate(p) ))
            //|> Map.add "fsharp/pipelineHint" (requestHandling (fun s p -> s.FSharpPipelineHints(p) ))
            //|> Map.add "fake/listTargets" (requestHandling (fun s p -> s.FakeTargets(p) ))
            //|> Map.add "fake/runtimePath" (requestHandling (fun s p -> s.FakeRuntimePath(p) ))
            |> Map.add "tuc/info" (requestHandling (fun s p -> s.Info(p) ))



        LanguageServerProtocol.Server.start requestsHandlings input output TucLspClient (fun lspClient -> TucLspServer(consoleOutput, commands, lspClient))

    let start output (commands : Commands) =
        let logger = LogProvider.getLoggerByName "Startup"

        try
            let result = startCore output commands
            logger.info (Log.setMessage "Start - Ending LSP mode with {reason}" >> Log.addContextDestructured "reason" result)
            int result
        with
        | ex ->
            logger.error (Log.setMessage "Start - LSP mode crashed" >> Log.addExn ex)
            3
