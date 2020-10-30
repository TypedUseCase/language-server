namespace Tuc.LanguageServer

module Lsp =

    open Tuc.Domain
    //open Argu
    //open FsAutoComplete
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

        // let logger = LogProvider.getLoggerByName "LSP"
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


        let parseFile (p: DidChangeTextDocumentParams) =
            eprintfn "LanguageServer.Lsp.parseFile ..."

        ///Helper function for handling file requests using **recent** type check results
        //member x.fileHandler<'a> (f: SourceFilePath -> ParseAndCheckResults -> string [] -> AsyncLspResult<'a>) (file: SourceFilePath) : AsyncLspResult<'a> =

        override __.Initialize(p: InitializeParams) = async {
            //logger.info (Log.setMessage "Initialize Request")
            eprintfn "[E] LanguageServer.Lsp.initialize: %A ..." p.RootUri
            printfn "[P] LanguageServer.Lsp.initialize: %A ..." p.RootUri

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

            lspClient.WindowLogMessage({ Type = MessageType.Info; Message = "Initialize message"})
            |> Async.Start

            return
                { InitializeResult.Default with
                    Capabilities =
                        { ServerCapabilities.Default with
                            // HoverProvider = Some true
                            // RenameProvider = Some true
                            // DefinitionProvider = Some true
                            // TypeDefinitionProvider = Some true
                            // ImplementationProvider = Some true
                            // ReferencesProvider = Some true
                            // DocumentHighlightProvider = Some true
                            // DocumentSymbolProvider = Some true
                            // WorkspaceSymbolProvider = Some true
                            // DocumentFormattingProvider = Some true
                            // DocumentRangeFormattingProvider = Some false
                            // SignatureHelpProvider = Some {
                            //     SignatureHelpOptions.TriggerCharacters = Some [| "("; ","|]
                            // }
                            // CompletionProvider =
                            //     Some {
                            //         ResolveProvider = Some true
                            //         TriggerCharacters = Some ([| "."; "'"; |])
                            //     }
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
            //logger.info (Log.setMessage "Initialized request")
            eprintfn "[E] LanguageServer.Lsp.initialized. DomainTypes: [%A]" (domainTypes |> List.length)
            printfn "[P] LanguageServer.Lsp.initialized. DomainTypes: [%A]" (domainTypes |> List.length)

            return ()
        }

        override __.TextDocumentDidOpen(p: DidOpenTextDocumentParams) = async {
            eprintfn "[E] LanguageServer.Lsp.TextDocumentDidOpen."
            printfn "[P] LanguageServer.Lsp.TextDocumentDidOpen."

            return ()
        }

        override __.TextDocumentDidChange(p) = async {
            eprintfn "[E] LanguageServer.Lsp.TextDocumentDidChange."
            printfn "[P] LanguageServer.Lsp.TextDocumentDidChange."

            return ()
        }

        //TODO: Investigate if this should be done at all
        override __.TextDocumentDidSave(p) = async {
            eprintfn "[E] LanguageServer.Lsp.TextDocumentDidSave."
            printfn "[P] LanguageServer.Lsp.TextDocumentDidSave."

            return ()
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
        //let logger = LogProvider.getLoggerByName "Startup"

        try
            let result = startCore commands
            eprintfn "[E] LanguageServer.Lsp.start ...."
            printfn "[P] LanguageServer.Lsp.start ...."
            //logger.info (Log.setMessage "Start - Ending LSP mode with {reason}" >> Log.addContextDestructured "reason" result)
            int result
        with
        | ex ->
            //logger.error (Log.setMessage "Start - LSP mode crashed" >> Log.addExn ex)
            eprintfn "[E] LanguageServer.Lsp.start - error: %s." ex.Message
            printfn "[P] LanguageServer.Lsp.start - error: %s." ex.Message
            3
