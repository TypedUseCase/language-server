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
    open Newtonsoft.Json.Linq
    //open ProjectSystem
    open System
    open System.IO

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

        // TODO: Add the missing notifications
        // TODO: Implement requests

    type TucLspServer(commands: Commands, lspClient: TucLspClient) =
        inherit LspServer()

        let logger = LogProvider.getLoggerByName "LSP"
        // let fantomasLogger = LogProvider.getLoggerByName "Fantomas"

        let mutable clientCapabilities: ClientCapabilities option = None
        // let mutable glyphToCompletionKind = glyphToCompletionKindGenerator None
        // let mutable glyphToSymbolKind = glyphToSymbolKindGenerator None
        //let subscriptions = ResizeArray<IDisposable>()

        let mutable config = FSharpConfig.Default
        let mutable rootPath : string option = None

        let mutable domainTypes: DomainType list = []

        /// centralize any state changes when the config is updated here
        let updateConfig (newConfig: FSharpConfig) =
            let toCompilerToolArgument (path: string) = sprintf "--compilertool:%s" path
            config <- newConfig

        //TODO: Thread safe version
        let fixes = System.Collections.Generic.Dictionary<DocumentUri, (LanguageServerProtocol.Types.Range * TextEdit) list>()
        let analyzerFixes = System.Collections.Generic.Dictionary<(DocumentUri * string), (LanguageServerProtocol.Types.Range * TextEdit) list>()

        let logInfo message =
            logger.info (Log.setMessage message)

            //lspClient.WindowLogMessage({ Type = MessageType.Info; Message = sprintf "[cl.log] %s" message })
            //|> Async.Start

        let parseFile (p: DidChangeTextDocumentParams) =
            logInfo "LanguageServer.Lsp.parseFile ..."

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
        member x.positionHandler<'a, 'b when 'b :> ITextDocumentPositionParams> (f: 'b -> FcsRange.pos (* -> ParseAndCheckResults *) -> string -> string [] ->  AsyncLspResult<'a>) (arg: 'b) : AsyncLspResult<'a> =
            async {
                let pos = arg.GetFcsPos()
                let file = arg.GetFilePath()
                logger.info (Log.setMessage "PositionHandler - Position request: {file} at {pos}" >> Log.addContextDestructured "file" file >> Log.addContextDestructured "pos" pos)

                return!
                    match commands.TryGetFileCheckerOptionsWithLinesAndLineStr(file, pos) with
                    | ResultOrString.Error s ->
                        logger.error (Log.setMessage "PositionHandler - Getting file checker options for {file} failed" >> Log.addContextDestructured "error" s >> Log.addContextDestructured "file" file)
                        AsyncLspResult.internalError s
                    | ResultOrString.Ok (lines, lineStr) ->
                        try
                            let tyResOpt = None // commands.TryGetRecentTypeCheckResultsForFile(file, options)
                            match tyResOpt with
                            | None ->
                                logger.info (Log.setMessage "PositionHandler - Cached typecheck results not yet available for {file}" >> Log.addContextDestructured "file" file)
                                AsyncLspResult.internalError "Cached typecheck results not yet available"
                            | Some tyRes ->
                                async {
                                    let! r = Async.Catch (f arg pos (* tyRes *) lineStr lines)
                                    match r with
                                    | Choice1Of2 r -> return r
                                    | Choice2Of2 e ->
                                        logger.error (Log.setMessage "PositionHandler - Failed during child operation on file {file}" >> Log.addContextDestructured "file" file >> Log.addExn e)
                                        return LspResult.internalError e.Message
                                }
                        with e ->
                            logger.error (Log.setMessage "PositionHandler - Operation failed for file {file}" >> Log.addContextDestructured "file" file >> Log.addExn e)
                            AsyncLspResult.internalError e.Message
            }

        // todo<tuc> - handle when tuc will be generated by extension
        (* override __.WorkspaceExecuteCommand(p) = async {
            logInfo <| sprintf "WorkspaceExecuteCommand with %A" p
        } *)

        // todo<tuc> - handle when tuc will be generated by extension
        (* override __.TextDocumentDidOpen(p: DidOpenTextDocumentParams) = async {
            logInfo <| sprintf "TextDocumentDidOpen %A" { p with TextDocument = { p.TextDocument with Text = "..." } }
            return ()
        } *)

        // todo<tuc> - handle when tuc will be generated by extension
        (* override __.TextDocumentDidChange(p: DidChangeTextDocumentParams) = async {
            logInfo <| sprintf "TextDocumentDidChange %A" p.TextDocument
            return ()
        } *)

        override x.TextDocumentHover(p: TextDocumentPositionParams) = async {

            logInfo <| sprintf "TextDocumentHover %A" p.TextDocument

            let! r = async {
                let doc = p.TextDocument
                let file = doc.GetFilePath()
                let pos = p.GetFcsPos()

                let line = p.Position.Line
                let col = p.Position.Character
                let lines = file |> commands.TryGetFileCheckerOptionsWithLines

                let word =
                    match lines with
                    | Ok lines ->
                        let lineStr = lines.[line]
                        let word = lineStr.Substring(0, col)
                        word
                    | _ ->
                        "nothing" // todo - add a proper error for hover

                (* let hover =
                    [
                        "# Hover"
                        word
                        (*
                        "```typescript"
                        "someCode();"
                        "```"
                        *)
                    ]
                    |> String.concat "\n"
                    |> markdown *)

                return success (Some {
                    Contents = MarkedStrings [|
                        MarkedString.WithLanguage { Language = "tuc"; Value = word }
                        MarkedString.String "Description: Participants section where all use-case participants must be defined."
                    |]
                    Range = None
                    (* Range = Some {
                        Start = { Line = 2; Character = 0 }
                        End = { Line = 2; Character = 12 }
                    } *)
                })
            }

            let! _ =
                p
                |> x.positionHandler (fun p pos lineStr lines ->
                    async {
                        return success (Some {
                            Contents = MarkedString (MarkedString.String "hover from handler")
                            Range = None
                        })
                    }
                )

            return r
        }

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

        override __.TextDocumentDidSave(p: DidSaveTextDocumentParams) = async {
            logInfo <| sprintf "TextDocumentDidSave %A" { p with Text = None }


            // todo - if F# -> resolve domain types
            domainTypes <- commands.ResolveDomainTypes rootPath

            return ()
        }

        override __.WorkspaceDidChangeWatchedFiles(p) = async {
            logger.info (Log.setMessage "WorkspaceDidChangeWatchedFiles Request: {parms}" >> Log.addContextDestructured "parms" p )
            domainTypes <- commands.ResolveDomainTypes rootPath
        }

        override __.WorkspaceDidChangeWorkspaceFolders(p) = async {
            logger.info (Log.setMessage "WorkspaceDidChangeWorkspaceFolders Request: {parms}" >> Log.addContextDestructured "parms" p )
        }

        override __.TextDocumentCodeLens(p) = async {
            logger.info (Log.setMessage "TextDocumentCodeLens Request: {parms}" >> Log.addContextDestructured "parms" p )
            // todo - show types in code lens for a method calls

            return success None
        }

        override __.CodeLensResolve(cl) = async {
            logInfo <| sprintf "CodeLensResolve %A" cl
            // todo - show types in code lens for a method calls

            return success cl
        }

        override __.TextDocumentCompletion(p: CompletionParams) = async {
            //logInfo <| sprintf "TextDocumentCompletion %A" p
            logger.info (Log.setMessage "TextDocumentCompletion Request: {context}" >> Log.addContextDestructured "context" p)

            // Sublime-lsp doesn't like when we answer null so we answer an empty list instead
            let noCompletion = success (Some { IsIncomplete = true; Items = [||] })
            let doc = p.TextDocument
            let file = doc.GetFilePath()
            let pos = p.GetFcsPos()

            logInfo <| sprintf "[CI] file: %s | pos: %A" file pos

            (*
            [Info  - 3:47:27 PM] [cl.log] TextDocumentCompletion
            {
                TextDocument = { Uri = "file:///Users/chromecp/fsharp/tuc-extension/etc/events.tuc" }
                Position = { Line = 17
                Character = 7
            }
            Context = Some {
                triggerKind = Invoked
                triggerCharacter = None
            } }
            [Info  - 3:47:27 PM] [cl.log] [CI] file: /Users/chromecp/fsharp/tuc-extension/etc/events.tuc | pos: (18,8)
             *)

            (*
                // todo
                - kouknout na TryGetFileCheckerOptionsWithLines
                    - vraci to lines (asi otevreneho souboru)
                    - z toho pak vytahnout aktualni "slovo", ke kteremu se ma napovidat
                    - ale asi bude lepsi to proste vyparsovat rucne primo z TextDocument

                - najak mapovat DomainTypes -> CompletionItem a filterovat podle hledaneho slova (asi? - kouknout na FSAC)
                    - poresit taky jestli funguje watch na .fsx fily (to by se asi melo spis resit v didChange na fsx)

             *)


            (* let! res =
                match commands.TryGetFileCheckerOptionsWithLines file with
                | ResultOrString.Error s -> AsyncLspResult.internalError s
                | ResultOrString.Ok (options, lines) ->
                    let line = p.Position.Line
                    let col = p.Position.Character
                    let lineStr = lines.[line]
                    let word = lineStr.Substring(0, col)
                    let ok = line <= lines.Length && line >= 0 && col <= lineStr.Length + 1 && col >= 0
                    if not ok then
                        logger.info (Log.setMessage "TextDocumentCompletion Not OK:\n COL: {col}\n LINE_STR: {lineStr}\n LINE_STR_LENGTH: {lineStrLength}"
                                     >> Log.addContextDestructured "col" col
                                     >> Log.addContextDestructured "lineStr" lineStr
                                     >> Log.addContextDestructured "lineStrLength" lineStr.Length)

                        AsyncLspResult.internalError "not ok"
                    elif (lineStr.StartsWith "#" && (KeywordList.hashDirectives.Keys |> Seq.exists (fun k -> k.StartsWith word ) || word.Contains "\n" )) then
                        let completionList = { IsIncomplete = false; Items = KeywordList.hashSymbolCompletionItems }
                        async.Return (success (Some completionList))
                    else
                        async {
                            let! tyResOpt =
                                match p.Context with
                                | None -> commands.TryGetRecentTypeCheckResultsForFile(file, options) |> async.Return
                                | Some ctx ->
                                    //ctx.triggerKind = CompletionTriggerKind.Invoked ||
                                    if  (ctx.triggerCharacter = Some ".") then
                                        commands.TryGetLatestTypeCheckResultsForFile(file)
                                    else
                                        commands.TryGetRecentTypeCheckResultsForFile(file, options) |> async.Return

                            match tyResOpt with
                            | None ->
                              logger.info (Log.setMessage "TextDocumentCompletion - no type check results")
                              return LspResult.internalError "no type check results"
                            | Some tyRes ->
                                let! res = commands.Completion tyRes pos lineStr lines file None (config.KeywordsAutocomplete) (config.ExternalAutocomplete)
                                let res =
                                    match res with
                                    | CoreResponse.Res(decls, keywords) ->
                                        let items =
                                            decls
                                            |> Array.mapi (fun id d ->
                                                let code =
                                                    if System.Text.RegularExpressions.Regex.IsMatch(d.Name, """^[a-zA-Z][a-zA-Z0-9']+$""") then d.Name
                                                    elif d.NamespaceToOpen.IsSome then d.Name
                                                    else PrettyNaming.QuoteIdentifierIfNeeded d.Name
                                                let label =
                                                    match d.NamespaceToOpen with
                                                    | Some no -> sprintf "%s (open %s)" d.Name no
                                                    | None -> d.Name

                                                { CompletionItem.Create(d.Name) with
                                                    Kind = glyphToCompletionKind d.Glyph
                                                    InsertText = Some code
                                                    SortText = Some (sprintf "%06d" id)
                                                    FilterText = Some d.Name
                                                    Label = label
                                                }
                                            )
                                        let its = if not keywords then items else Array.append items KeywordList.keywordCompletionItems
                                        let completionList = { IsIncomplete = false; Items = its}
                                        success (Some completionList)
                                    | _ ->
                                      logger.info (Log.setMessage "TextDocumentCompletion - no completion results")
                                      noCompletion
                                return res
                        } *)

            return success (Some { IsIncomplete = false; Items = [|
                { CompletionItem.Create("CI item") with
                    Kind = Some CompletionItemKind.Class
                    InsertText = Some "Insert text"
                    SortText = Some (sprintf "%06d" 0)
                    FilterText = Some "CI item"
                    // Label = ""
                }
            |] })
        }

        override __.CompletionItemResolve(ci) = async {
            //logInfo <| sprintf "CompletionItemResolve %A" ci
            logger.info (Log.setMessage "CompletionItemResolve Request: {parms}" >> Log.addContextDestructured "parms" ci )

            // todo - tahle metoda je mozna zbytecna, protoze ted budu mit asi vsechno rovnou v ci

            (* let! res = commands.Helptext ci.InsertText.Value
            let res =
                match res with
                | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                    ci
                | CoreResponse.Res (HelpText.Simple (name, str)) ->
                    let d = Documentation.Markup (markdown str)
                    {ci with Detail = Some name; Documentation = Some d  }
                | CoreResponse.Res (HelpText.Full (name, tip, additionalEdit)) ->
                    let (si, comment) = (TipFormatter.formatTip tip) |> List.collect id |> List.head
                    //TODO: Add insert namespace
                    let d = Documentation.Markup (markdown comment)
                    {ci with Detail = Some si; Documentation = Some d  } *)
            return success ci
        }

    let startCore (commands : Commands) =
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



        LanguageServerProtocol.Server.start requestsHandlings input output TucLspClient (fun lspClient -> TucLspServer(commands, lspClient))

    let start (commands : Commands) =
        let logger = LogProvider.getLoggerByName "Startup"

        try
            let result = startCore commands
            logger.info (Log.setMessage "Start - Ending LSP mode with {reason}" >> Log.addContextDestructured "reason" result)
            int result
        with
        | ex ->
            logger.error (Log.setMessage "Start - LSP mode crashed" >> Log.addExn ex)
            3
