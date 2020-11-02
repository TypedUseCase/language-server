namespace Tuc.LanguageServer

open System
open System.IO
open FSharp.Compiler.SourceCodeServices
open System.Collections.Concurrent
open System.Threading
open FSharp.Compiler.Range
open FsLibLog

type DeclName = string
type CompletionNamespaceInsert = string * int * int * string

type State =
  {
    Logger: ILog

    Files : ConcurrentDictionary<SourceFilePath, VolatileFile>
    LastCheckedVersion: ConcurrentDictionary<SourceFilePath, int>

    HelpText : ConcurrentDictionary<DeclName, FSharpToolTipText>
    Declarations: ConcurrentDictionary<DeclName, FSharpDeclarationListItem * pos * SourceFilePath>
    CompletionNamespaceInsert : ConcurrentDictionary<DeclName, CompletionNamespaceInsert>
    mutable CurrentAST: FSharp.Compiler.SyntaxTree.ParsedInput option

    NavigationDeclarations : ConcurrentDictionary<SourceFilePath, FSharpNavigationTopLevelDeclaration[]>
    ParseResults: ConcurrentDictionary<SourceFilePath, FSharpParseFileResults>
    CancellationTokens: ConcurrentDictionary<SourceFilePath, CancellationTokenSource list>

    ScriptProjectOptions: ConcurrentDictionary<SourceFilePath, int * FSharpProjectOptions>

    mutable ColorizationOutput: bool
  }

  static member Initial (logger: ILog) = {
      Logger = logger
      Files = ConcurrentDictionary()
      LastCheckedVersion = ConcurrentDictionary()
      HelpText = ConcurrentDictionary()
      Declarations = ConcurrentDictionary()
      CurrentAST = None
      CompletionNamespaceInsert = ConcurrentDictionary()
      CancellationTokens = ConcurrentDictionary()
      NavigationDeclarations = ConcurrentDictionary()
      ParseResults = ConcurrentDictionary()
      ScriptProjectOptions = ConcurrentDictionary()
      ColorizationOutput = false }

  member x.TryGetFileVersion (file: SourceFilePath) : int option =
    let file = Path.normalize file

    x.Files.TryFind file
    |> Option.bind (fun f -> f.Version)

  member x.TryGetLastCheckedVersion (file: SourceFilePath) : int option =
    let file = Path.normalize file

    x.LastCheckedVersion.TryFind file

  member x.SetFileVersion (file: SourceFilePath) (version: int) =
    x.Files.TryFind file
    |> Option.iter (fun n ->
      let fileState = {n with Version = Some version}
      x.Files.[file] <- fileState
    )

  member x.SetLastCheckedVersion (file: SourceFilePath) (version: int) =
    x.LastCheckedVersion.[file] <- version

  member x.AddFileText(file: SourceFilePath, lines: LineStr[], version) =
    let file = Path.normalize file
    let fileState = { Lines = lines; Touched = DateTime.Now; Version = version }
    x.Files.[file] <- fileState

  member x.AddCancellationToken(file : SourceFilePath, token: CancellationTokenSource) =
    x.CancellationTokens.AddOrUpdate(file, [token], fun _ lst -> token::lst)
    |> ignore

  member x.GetCancellationTokens(file : SourceFilePath) =
    let lst = x.CancellationTokens.GetOrAdd(file, fun _ -> [])
    x.CancellationTokens.TryRemove(file) |> ignore
    lst

  member x.TryGetFileCheckerOptionsWithLines(file: SourceFilePath) : ResultOrString<LineStr[]> =
    let file = Path.normalize file
    match x.Files.TryFind(file) with
    | None when file |> File.Exists ->
      x.Logger.info (Log.setMessage "Read file lines: {file}" >> Log.addContextDestructured "file" file )

      file
      |> File.ReadAllLines
      |> tee (fun lines -> x.AddFileText(file, lines, None))
      |> ResultOrString.Ok
    | Some (volFile) -> ResultOrString.Ok (volFile.Lines)
    | _ -> ResultOrString.Error (sprintf "File '%s' is not found." file)

  member x.TryGetFileCheckerOptionsWithSource(file: SourceFilePath) : ResultOrString<string> =
    let file = Path.normalize file
    match x.TryGetFileCheckerOptionsWithLines(file) with
    | ResultOrString.Error x -> ResultOrString.Error x
    | Ok (lines) -> Ok (String.concat "\n" lines)

  member x.TryGetFileSource(file: SourceFilePath) : ResultOrString<string[]> =
    let file = Path.normalize file
    match x.Files.TryFind(file) with
    | None -> ResultOrString.Error (sprintf "File '%s' not parsed" file)
    | Some f -> Ok (f.Lines)

  member x.TryGetFileCheckerOptionsWithLinesAndLineStr(file: SourceFilePath, pos : pos) : ResultOrString<LineStr[] * LineStr> =
    let file = Path.normalize file
    match x.TryGetFileCheckerOptionsWithLines(file) with
    | ResultOrString.Error x -> ResultOrString.Error x
    | Ok (lines) ->
      let ok =
        pos.Line <= lines.Length && pos.Line >= 1 &&
        pos.Column <= lines.[pos.Line - 1].Length + 1 && pos.Column >= 1
      if not ok then ResultOrString.Error "Position is out of range"
      else Ok (lines, lines.[pos.Line - 1])
