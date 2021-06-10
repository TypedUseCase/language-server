namespace Tuc.LanguageServer

module LspHelpers =

    open System
    open System.IO
    open LanguageServerProtocol.Types
    //open FsAutoComplete.Utils
    open FSharp.Compiler.SourceCodeServices
    open FSharp.Reflection
    open System.Collections.Generic
    open System.Text

    [<RequireQualifiedAccess>]
    module Location =
        let (|IsSingleLine|_|): Tuc.Location -> Tuc.Range option = function
            | { Range = range } when range.Start.Line = range.End.Line -> Some range
            | _ -> None

    [<AutoOpen>]
    module Conversions =
        module Lsp = LanguageServerProtocol.Types

        let protocolPosToPos (pos: Lsp.Position): Tuc.Position =
            { Line = pos.Line; Character = pos.Character }

        let posToProtocolPos (pos: Tuc.Position): Lsp.Position =
            { Line = pos.Line; Character = pos.Character }

        let protocolRangeToRange (range: Lsp.Range): Tuc.Range =
            { Start = range.Start |> protocolPosToPos; End = range.End |> protocolPosToPos }

        let rangeToProtocolRange (range: Tuc.Range): Lsp.Range =
            { Start = range.Start |> posToProtocolPos; End = range.End |> posToProtocolPos }

        type TextDocumentIdentifier with
            member doc.GetFilePath() = Path.fileUriToLocalPath doc.Uri
            member doc.GetLanguageId() =
                match doc.GetFilePath() |> Path.GetExtension with
                | ".tuc" -> Some "tuc"
                | ".fs" | ".fsx" -> Some "fsharp"
                | _ -> None

        type VersionedTextDocumentIdentifier with
            member doc.GetFilePath() = Path.fileUriToLocalPath doc.Uri

        type TextDocumentItem with
            member doc.GetFilePath() = Path.fileUriToLocalPath doc.Uri

        type ITextDocumentPositionParams with
            member p.GetFilePath() = p.TextDocument.GetFilePath()
            member p.GetTucPosition() = protocolPosToPos p.Position
            member doc.GetLanguageId() =
                match doc.GetFilePath() |> Path.GetExtension with
                | ".tuc" -> Some "tuc"
                | ".fs" | ".fsx" -> Some "fsharp"
                | _ -> None

        let fcsSeverityToDiagnostic = function
            | FSharpDiagnosticSeverity.Error -> Some DiagnosticSeverity.Error
            | FSharpDiagnosticSeverity.Warning -> Some DiagnosticSeverity.Warning
            | FSharpDiagnosticSeverity.Hidden -> None
            | FSharpDiagnosticSeverity.Info -> Some DiagnosticSeverity.Information

        let fcsErrorToDiagnostic (error: FSharpDiagnostic) =
            {
                Range =
                    {
                        Start = { Line = error.StartLineAlternate - 1; Character = error.StartColumn }
                        End = { Line = error.EndLineAlternate - 1; Character = error.EndColumn }
                    }
                Severity = fcsSeverityToDiagnostic error.Severity
                Source = "F# Compiler"
                Message = error.Message
                Code = Some (string error.ErrorNumber)
                RelatedInformation = Some [||]
                Tags = None
            }

        let getText (lines: string []) (r: Lsp.Range) =
            lines.[r.Start.Line].Substring(r.Start.Character, r.End.Character - r.Start.Character)

    [<AutoOpen>]
    module internal GlyphConversions =
        let internal glyphToKindGenerator<'kind when 'kind : equality>
            (clientCapabilities: ClientCapabilities option)
            (setFromCapabilities: ClientCapabilities -> 'kind [] option)
            (defaultSet: 'kind [])
            (getUncached: FSharpGlyph -> 'kind[]) =

            let completionItemSet = clientCapabilities |> Option.bind(setFromCapabilities)
            let completionItemSet = defaultArg completionItemSet defaultSet

            let bestAvailable (possible: 'kind[]) =
                possible
                |> Array.tryFind (fun x -> Array.contains x completionItemSet)

            let unionCases = FSharpType.GetUnionCases(typeof<FSharpGlyph>)
            let cache = Dictionary<FSharpGlyph, 'kind option>(unionCases.Length)
            for info in unionCases do
                let glyph = FSharpValue.MakeUnion(info, [||]) :?> FSharpGlyph
                let completionItem = getUncached glyph |> bestAvailable
                cache.Add(glyph, completionItem)

            fun glyph ->
                cache.[glyph]

        type CompletionItemKind = LanguageServerProtocol.Types.CompletionItemKind

        /// Compute the best possible CompletionItemKind for each FSharpGlyph according
        /// to the client capabilities
        let glyphToCompletionKindGenerator (clientCapabilities: ClientCapabilities option) =
            glyphToKindGenerator
                clientCapabilities
                (fun clientCapabilities ->
                    clientCapabilities.TextDocument
                    |> Option.bind(fun x -> x.Completion)
                    |> Option.bind(fun x -> x.CompletionItemKind)
                    |> Option.bind(fun x -> x.ValueSet))
                CompletionItemKindCapabilities.DefaultValueSet
                (fun code ->
                    match code with
                    | FSharpGlyph.Class -> [| CompletionItemKind.Class |]
                    | FSharpGlyph.Constant -> [| CompletionItemKind.Constant |]
                    | FSharpGlyph.Delegate -> [| CompletionItemKind.Function |]
                    | FSharpGlyph.Enum -> [| CompletionItemKind.Enum |]
                    | FSharpGlyph.EnumMember -> [| CompletionItemKind.EnumMember; CompletionItemKind.Enum |]
                    | FSharpGlyph.Event -> [| CompletionItemKind.Event |]
                    | FSharpGlyph.Exception -> [| CompletionItemKind.Class |]
                    | FSharpGlyph.Field -> [| CompletionItemKind.Field |]
                    | FSharpGlyph.Interface -> [| CompletionItemKind.Interface; CompletionItemKind.Class |]
                    | FSharpGlyph.Method -> [| CompletionItemKind.Method |]
                    | FSharpGlyph.OverridenMethod-> [| CompletionItemKind.Method |]
                    | FSharpGlyph.Module -> [| CompletionItemKind.Module; CompletionItemKind.Class |]
                    | FSharpGlyph.NameSpace -> [| CompletionItemKind.Module |]
                    | FSharpGlyph.Property -> [| CompletionItemKind.Property |]
                    | FSharpGlyph.Struct -> [| CompletionItemKind.Struct; CompletionItemKind.Class |]
                    | FSharpGlyph.Typedef -> [| CompletionItemKind.Class |]
                    | FSharpGlyph.Type -> [| CompletionItemKind.Class |]
                    | FSharpGlyph.Union -> [| CompletionItemKind.Class |]
                    | FSharpGlyph.Variable -> [| CompletionItemKind.Variable |]
                    | FSharpGlyph.ExtensionMethod -> [| CompletionItemKind.Method |]
                    | FSharpGlyph.Error
                    | _ -> [||])

        /// Compute the best possible SymbolKind for each FSharpGlyph according
        /// to the client capabilities
        let glyphToSymbolKindGenerator (clientCapabilities: ClientCapabilities option) =
            glyphToKindGenerator
                clientCapabilities
                (fun clientCapabilities ->
                    clientCapabilities.TextDocument
                    |> Option.bind(fun x -> x.DocumentSymbol)
                    |> Option.bind(fun x -> x.SymbolKind)
                    |> Option.bind(fun x -> x.ValueSet))
                SymbolKindCapabilities.DefaultValueSet
                (fun code ->
                    match code with
                    | FSharpGlyph.Class -> [| SymbolKind.Class |]
                    | FSharpGlyph.Constant -> [| SymbolKind.Constant |]
                    | FSharpGlyph.Delegate -> [| SymbolKind.Function |]
                    | FSharpGlyph.Enum -> [| SymbolKind.Enum |]
                    | FSharpGlyph.EnumMember -> [| SymbolKind.EnumMember; SymbolKind.Enum |]
                    | FSharpGlyph.Event -> [| SymbolKind.Event |]
                    | FSharpGlyph.Exception -> [| SymbolKind.Class |]
                    | FSharpGlyph.Field -> [| SymbolKind.Field |]
                    | FSharpGlyph.Interface -> [| SymbolKind.Interface; SymbolKind.Class |]
                    | FSharpGlyph.Method -> [| SymbolKind.Method |]
                    | FSharpGlyph.OverridenMethod-> [| SymbolKind.Method |]
                    | FSharpGlyph.Module -> [| SymbolKind.Module; SymbolKind.Class |]
                    | FSharpGlyph.NameSpace -> [| SymbolKind.Module |]
                    | FSharpGlyph.Property -> [| SymbolKind.Property |]
                    | FSharpGlyph.Struct -> [| SymbolKind.Struct; SymbolKind.Class |]
                    | FSharpGlyph.Typedef -> [| SymbolKind.Class |]
                    | FSharpGlyph.Type -> [| SymbolKind.Class |]
                    | FSharpGlyph.Union -> [| SymbolKind.Class |]
                    | FSharpGlyph.Variable -> [| SymbolKind.Variable |]
                    | FSharpGlyph.ExtensionMethod -> [| SymbolKind.Method |]
                    | FSharpGlyph.Error
                    | _ -> [||])

    type PlainNotification= { Content: string }

    type ProjectParms = {
        /// Project file to compile
        Project: TextDocumentIdentifier
    }

    type WorkspaceLoadParms = {
        /// Project files to load
        TextDocuments: TextDocumentIdentifier []
    }

    type WorkspacePeekRequest = {Directory : string; Deep: int; ExcludedDirs: string array}
    type DocumentationForSymbolReuqest = {XmlSig: string; Assembly: string}

    //type FakeTargetsRequest = {FileName : string; FakeContext : FakeSupport.FakeContext; }

    type HighlightingRequest = {FileName : string; }

    type LineLensConfig = {
        Enabled: string
        Prefix: string
    }

    type FsdnRequest = { Query: string }

    type DotnetNewListRequest = { Query: string }

    type DotnetNewRunRequest = { Template: string; Output: string option; Name: string option }

    type FSharpLiterateRequest = {FileName : string; }

    type FSharpPipelineHintRequest = {FileName : string; }

    type FSharpConfigDto = {
        AutomaticWorkspaceInit: bool option
        WorkspaceModePeekDeepLevel: int option
        ExcludeProjectDirectories: string [] option
        KeywordsAutocomplete: bool option
        ExternalAutocomplete: bool option
        Linter: bool option
        LinterConfig: string option
        UnionCaseStubGeneration: bool option
        UnionCaseStubGenerationBody: string option
        RecordStubGeneration: bool option
        RecordStubGenerationBody: string option
        InterfaceStubGeneration: bool option
        InterfaceStubGenerationObjectIdentifier: string option
        InterfaceStubGenerationMethodBody: string option
        UnusedOpensAnalyzer: bool option
        UnusedDeclarationsAnalyzer: bool option
        SimplifyNameAnalyzer: bool option
        ResolveNamespaces: bool option
        EnableReferenceCodeLens: bool option
        EnableAnalyzers: bool option
        AnalyzersPath: string [] option
        DisableInMemoryProjectReferences: bool option
        LineLens: LineLensConfig option
        UseSdkScripts: bool option
        DotNetRoot: string option
        FSIExtraParameters: string [] option
        FSICompilerToolLocations: string [] option
        TooltipMode : string option
        GenerateBinlog: bool option
        AbstractClassStubGeneration: bool option
        AbstractClassStubGenerationObjectIdentifier: string option
        AbstractClassStubGenerationMethodBody: string option

    }

    type FSharpConfigRequest = {
        FSharp: FSharpConfigDto
    }

    type FSharpConfig = {
        AutomaticWorkspaceInit: bool
        WorkspaceModePeekDeepLevel: int
        ExcludeProjectDirectories: string []
        KeywordsAutocomplete: bool
        ExternalAutocomplete: bool
        Linter: bool
        LinterConfig: string option
        UnionCaseStubGeneration: bool
        UnionCaseStubGenerationBody: string
        RecordStubGeneration: bool
        RecordStubGenerationBody: string
        AbstractClassStubGeneration: bool
        AbstractClassStubGenerationObjectIdentifier: string
        AbstractClassStubGenerationMethodBody: string
        InterfaceStubGeneration: bool
        InterfaceStubGenerationObjectIdentifier: string
        InterfaceStubGenerationMethodBody: string
        UnusedOpensAnalyzer: bool
        UnusedDeclarationsAnalyzer: bool
        SimplifyNameAnalyzer: bool
        ResolveNamespaces: bool
        EnableReferenceCodeLens: bool
        EnableAnalyzers: bool
        AnalyzersPath: string []
        DisableInMemoryProjectReferences: bool
        LineLens: LineLensConfig
        UseSdkScripts: bool
        DotNetRoot: string
        FSIExtraParameters: string []
        FSICompilerToolLocations: string []
        TooltipMode : string
        GenerateBinlog: bool
    }
    with
        static member Default : FSharpConfig =
            {
                AutomaticWorkspaceInit = false
                WorkspaceModePeekDeepLevel = 2
                ExcludeProjectDirectories = [||]
                KeywordsAutocomplete = false
                ExternalAutocomplete = false
                Linter = false
                LinterConfig = None
                UnionCaseStubGeneration = false
                UnionCaseStubGenerationBody = """failwith "Not Implemented" """
                RecordStubGeneration = false
                RecordStubGenerationBody = "failwith \"Not Implemented\""
                AbstractClassStubGeneration = true
                AbstractClassStubGenerationObjectIdentifier = "this"
                AbstractClassStubGenerationMethodBody = "failwith \"Not Implemented\""
                InterfaceStubGeneration = false
                InterfaceStubGenerationObjectIdentifier = "this"
                InterfaceStubGenerationMethodBody = "failwith \"Not Implemented\""
                UnusedOpensAnalyzer = false
                UnusedDeclarationsAnalyzer = false
                SimplifyNameAnalyzer = false
                ResolveNamespaces = false
                EnableReferenceCodeLens = false
                EnableAnalyzers = false
                AnalyzersPath = [||]
                DisableInMemoryProjectReferences = false
                LineLens = {
                    Enabled = "never"
                    Prefix =""
                }
                UseSdkScripts = false
                DotNetRoot = "Environment.dotnetSDKRoot.Value"  // todo ...
                FSIExtraParameters = [||]
                FSICompilerToolLocations = [||]
                TooltipMode = "full"
                GenerateBinlog = false
            }

        static member FromDto(dto: FSharpConfigDto): FSharpConfig =
            {
                AutomaticWorkspaceInit = defaultArg dto.AutomaticWorkspaceInit false
                WorkspaceModePeekDeepLevel = defaultArg dto.WorkspaceModePeekDeepLevel 2
                ExcludeProjectDirectories = defaultArg dto.ExcludeProjectDirectories [||]
                KeywordsAutocomplete = defaultArg dto.KeywordsAutocomplete false
                ExternalAutocomplete = defaultArg dto.ExternalAutocomplete false
                Linter = defaultArg dto.Linter false
                LinterConfig = dto.LinterConfig
                UnionCaseStubGeneration = defaultArg dto.UnionCaseStubGeneration false
                UnionCaseStubGenerationBody = defaultArg dto.UnionCaseStubGenerationBody "failwith \"Not Implemented\""
                RecordStubGeneration = defaultArg dto.RecordStubGeneration false
                RecordStubGenerationBody = defaultArg dto.RecordStubGenerationBody "failwith \"Not Implemented\""
                InterfaceStubGeneration = defaultArg dto.InterfaceStubGeneration false
                InterfaceStubGenerationObjectIdentifier = defaultArg dto.InterfaceStubGenerationObjectIdentifier "this"
                InterfaceStubGenerationMethodBody = defaultArg dto.InterfaceStubGenerationMethodBody "failwith \"Not Implemented\""
                UnusedOpensAnalyzer = defaultArg dto.UnusedOpensAnalyzer false
                UnusedDeclarationsAnalyzer = defaultArg dto.UnusedDeclarationsAnalyzer false
                SimplifyNameAnalyzer = defaultArg dto.SimplifyNameAnalyzer false
                ResolveNamespaces = defaultArg dto.ResolveNamespaces false
                EnableReferenceCodeLens = defaultArg dto.EnableReferenceCodeLens false
                EnableAnalyzers = defaultArg dto.EnableAnalyzers false
                AnalyzersPath = defaultArg dto.AnalyzersPath [||]
                DisableInMemoryProjectReferences = defaultArg dto.DisableInMemoryProjectReferences false
                LineLens = {
                    Enabled = defaultArg (dto.LineLens |> Option.map (fun n -> n.Enabled)) "never"
                    Prefix = defaultArg (dto.LineLens |> Option.map (fun n -> n.Prefix)) ""
                }
                UseSdkScripts = defaultArg dto.UseSdkScripts false
                DotNetRoot =
                    dto.DotNetRoot
                    |> Option.bind (fun s -> if String.IsNullOrEmpty s then None else Some s)
                    |> Option.defaultValue "Environment.dotnetSDKRoot.Value"    // todo
                FSIExtraParameters = defaultArg dto.FSIExtraParameters FSharpConfig.Default.FSIExtraParameters
                FSICompilerToolLocations = defaultArg dto.FSICompilerToolLocations FSharpConfig.Default.FSICompilerToolLocations
                TooltipMode = defaultArg dto.TooltipMode "full"
                GenerateBinlog = defaultArg dto.GenerateBinlog false
                AbstractClassStubGeneration = defaultArg dto.AbstractClassStubGeneration false
                AbstractClassStubGenerationObjectIdentifier = defaultArg dto.AbstractClassStubGenerationObjectIdentifier "this"
                AbstractClassStubGenerationMethodBody = defaultArg dto.AbstractClassStubGenerationMethodBody "failwith \Not Implemented\""
            }

        /// called when a configuration change takes effect, so None-valued members here should revert options
        /// back to their defaults
        member x.AddDto(dto: FSharpConfigDto) =
            {
                AutomaticWorkspaceInit = defaultArg dto.AutomaticWorkspaceInit x.AutomaticWorkspaceInit
                AbstractClassStubGeneration = defaultArg dto.AbstractClassStubGeneration x.AbstractClassStubGeneration
                AbstractClassStubGenerationObjectIdentifier = defaultArg dto.AbstractClassStubGenerationObjectIdentifier x.AbstractClassStubGenerationObjectIdentifier
                AbstractClassStubGenerationMethodBody = defaultArg dto.AbstractClassStubGenerationMethodBody x.AbstractClassStubGenerationMethodBody
                WorkspaceModePeekDeepLevel = defaultArg dto.WorkspaceModePeekDeepLevel x.WorkspaceModePeekDeepLevel
                ExcludeProjectDirectories = defaultArg dto.ExcludeProjectDirectories x.ExcludeProjectDirectories
                KeywordsAutocomplete = defaultArg dto.KeywordsAutocomplete x.KeywordsAutocomplete
                ExternalAutocomplete = defaultArg dto.ExternalAutocomplete x.ExternalAutocomplete
                Linter = defaultArg dto.Linter x.Linter
                LinterConfig = dto.LinterConfig
                UnionCaseStubGeneration = defaultArg dto.UnionCaseStubGeneration x.UnionCaseStubGeneration
                UnionCaseStubGenerationBody = defaultArg dto.UnionCaseStubGenerationBody x.UnionCaseStubGenerationBody
                RecordStubGeneration = defaultArg dto.RecordStubGeneration x.RecordStubGeneration
                RecordStubGenerationBody = defaultArg dto.RecordStubGenerationBody x.RecordStubGenerationBody
                InterfaceStubGeneration = defaultArg dto.InterfaceStubGeneration x.InterfaceStubGeneration
                InterfaceStubGenerationObjectIdentifier = defaultArg dto.InterfaceStubGenerationObjectIdentifier x.InterfaceStubGenerationObjectIdentifier
                InterfaceStubGenerationMethodBody = defaultArg dto.InterfaceStubGenerationMethodBody x.InterfaceStubGenerationMethodBody
                UnusedOpensAnalyzer = defaultArg dto.UnusedOpensAnalyzer x.UnusedOpensAnalyzer
                UnusedDeclarationsAnalyzer = defaultArg dto.UnusedDeclarationsAnalyzer x.UnusedDeclarationsAnalyzer
                SimplifyNameAnalyzer = defaultArg dto.SimplifyNameAnalyzer x.SimplifyNameAnalyzer
                ResolveNamespaces = defaultArg dto.ResolveNamespaces x.ResolveNamespaces
                EnableReferenceCodeLens = defaultArg dto.EnableReferenceCodeLens x.EnableReferenceCodeLens
                EnableAnalyzers = defaultArg dto.EnableAnalyzers x.EnableAnalyzers
                AnalyzersPath = defaultArg dto.AnalyzersPath x.AnalyzersPath
                DisableInMemoryProjectReferences = defaultArg dto.DisableInMemoryProjectReferences x.DisableInMemoryProjectReferences
                LineLens = {
                    Enabled = defaultArg (dto.LineLens |> Option.map (fun n -> n.Enabled)) x.LineLens.Enabled
                    Prefix = defaultArg (dto.LineLens |> Option.map (fun n -> n.Prefix)) x.LineLens.Prefix
                }
                UseSdkScripts = defaultArg dto.UseSdkScripts x.UseSdkScripts
                DotNetRoot =
                    dto.DotNetRoot
                    |> Option.bind (fun s -> if String.IsNullOrEmpty s then None else Some s)
                    |> Option.defaultValue FSharpConfig.Default.DotNetRoot
                FSIExtraParameters = defaultArg dto.FSIExtraParameters FSharpConfig.Default.FSIExtraParameters
                FSICompilerToolLocations = defaultArg dto.FSICompilerToolLocations FSharpConfig.Default.FSICompilerToolLocations
                TooltipMode = defaultArg dto.TooltipMode x.TooltipMode
                GenerateBinlog = defaultArg dto.GenerateBinlog x.GenerateBinlog
            }

        (* member x.ScriptTFM =
            match x.UseSdkScripts with
            | false -> FSIRefs.NetFx
            | true -> FSIRefs.NetCore
        *)