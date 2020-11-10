namespace Tuc.LanguageServer

open FSharp.Compiler.AbstractIL.Internal.Library
open System
open FsLibLog
open TucHelpers

type VolatileFile = {
    Touched: DateTime
    Version: int option
    Lines: string []
    Segments: TucSegments
    ParsedTucs: Tuc.ParsedTuc list
}

open System.IO

type FileSystem(actualFs: IFileSystem, tryFindFile: SourceFilePath -> VolatileFile option) =
    let getContent (filename: string) =
        filename
        |> tryFindFile
        |> Option.map (fun file -> System.Text.Encoding.UTF8.GetBytes(String.Join("\n", file.Lines)))

    let fsLogger = LogProvider.getLoggerByName "FileSystem"
    /// translation of the BCL's Windows logic for Path.IsPathRooted.
    ///
    /// either the first char is '/', or the first char is a drive identifier followed by ':'
    let isWindowsStyleRootedPath (p: string) =
        let isAlpha (c: char) =
            (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

        (p.Length >= 1 && p.[0] = '/')
        || (p.Length >= 2 && isAlpha p.[0] && p.[1] = ':')

    /// translation of the BCL's Unix logic for Path.IsRooted.
    ///
    /// if the first character is '/' then the path is rooted
    let isUnixStyleRootedPath (p: string) = p.Length > 0 && p.[0] = '/'

    interface IFileSystem with (* for these two members we have to be incredibly careful to root/extend paths in an OS-agnostic way,
           as they handle paths for windows and unix file systems regardless of your host OS.
           Therefore, you cannot use the BCL's Path.IsPathRooted/Path.GetFullPath members *)

        member _.IsPathRootedShim(p: string) =
            let r =
                isWindowsStyleRootedPath p
                || isUnixStyleRootedPath p

            fsLogger.debug (Log.setMessage "Is {path} rooted? {result}" >> Log.addContext "path" p >> Log.addContext "result" r)
            r

        member _.GetFullPathShim(f: string) =
            let expanded =
                Path.filePathToUri f |> Path.fileUriToLocalPath

            fsLogger.debug (Log.setMessage "{path} expanded to {expanded}" >> Log.addContext "path" f >> Log.addContext "expanded" expanded)
            expanded

        (* These next members all make use of the VolatileFile concept, and so need to check that before delegating to the original FS implementation *)

        (* Note that in addition to this behavior, we _also_ do not normalize the file paths anymore for any other members of this interfact,
           because these members are always used by the compiler with paths returned from `GetFullPathShim`, which has done the normalization *)

        member _.ReadAllBytesShim(f) =
            getContent f
            |> Option.defaultWith (fun _ -> actualFs.ReadAllBytesShim f)

        member _.FileStreamReadShim(f) =
            getContent f
            |> Option.map (fun bytes -> new MemoryStream(bytes) :> Stream)
            |> Option.defaultWith (fun _ -> actualFs.FileStreamReadShim f)

        member _.GetLastWriteTimeShim(f) =
            tryFindFile f
            |> Option.map (fun f -> f.Touched)
            |> Option.defaultWith (fun _ -> actualFs.GetLastWriteTimeShim f)

        member _.FileStreamCreateShim(f) = actualFs.FileStreamCreateShim f
        member _.FileStreamWriteExistingShim(f) = actualFs.FileStreamWriteExistingShim f
        member _.IsInvalidPathShim(f) = actualFs.IsInvalidPathShim f
        member _.GetTempPathShim() = actualFs.GetTempPathShim()
        member _.SafeExists(f) = actualFs.SafeExists f
        member _.FileDelete(f) = actualFs.FileDelete f
        member _.AssemblyLoadFrom(f) = actualFs.AssemblyLoadFrom f
        member _.AssemblyLoad(f) = actualFs.AssemblyLoad f
        member _.IsStableFileHeuristic(f) = actualFs.IsStableFileHeuristic f

[<RequireQualifiedAccess>]
module FileSystem =
    let private writeContent (writer: StreamWriter) content = writer.WriteLine(sprintf "%s" content)

    let writeSeqToFile (filePath: string) (data: string seq) = File.WriteAllLines(filePath, data)

    let writeToFile (filePath: string) data = File.WriteAllText(filePath, data)

    let appendToFile (filePath: string) data = File.AppendAllText(filePath, data)

    let readLines (filePath: string) =
        File.ReadAllLines(filePath) |> Seq.toList

    let readContent (filePath: string) = File.ReadAllText(filePath)

    let tryReadContent (filePath: string) =
        if File.Exists filePath then File.ReadAllText(filePath) |> Some else None

    let getAllDirs =
        function
        | [] -> []
        | directories ->
            directories
            |> List.collect (Directory.EnumerateDirectories >> List.ofSeq)

    let rec getAllFiles =
        function
        | [] -> []
        | directories ->
            [
                yield! directories |> Seq.collect Directory.EnumerateFiles
                yield! directories |> Seq.collect Directory.EnumerateDirectories |> List.ofSeq |> getAllFiles
            ]
