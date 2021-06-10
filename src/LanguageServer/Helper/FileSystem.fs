namespace Tuc.LanguageServer

open System
open TucHelpers

type VolatileFile = {
    Touched: DateTime
    Version: int option
    Lines: string []
    Segments: TucSegments
    ParsedTucs: Tuc.ParsedTuc list
}

open System.IO

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
