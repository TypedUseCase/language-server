namespace Tuc.LanguageServer

[<RequireQualifiedAccess>]
module FileSystem =
    open System.IO

    let private writeContent (writer: StreamWriter) content =
        writer.WriteLine(sprintf "%s" content)

    let writeSeqToFile (filePath: string) (data: string seq) =
        File.WriteAllLines(filePath, data)

    let writeToFile (filePath: string) data =
        File.WriteAllText(filePath, data)

    let appendToFile (filePath: string) data =
        File.AppendAllText(filePath, data)

    let readLines (filePath: string) =
        File.ReadAllLines(filePath)
        |> Seq.toList

    let readContent (filePath: string) =
        File.ReadAllText(filePath)

    let tryReadContent (filePath: string) =
        if File.Exists filePath then File.ReadAllText(filePath) |> Some
        else None

    let getAllDirs = function
        | [] -> []
        | directories -> directories |> List.collect (Directory.EnumerateDirectories >> List.ofSeq)

    let rec getAllFiles = function
        | [] -> []
        | directories -> [
            yield! directories |> Seq.collect Directory.EnumerateFiles
            yield! directories |> Seq.collect Directory.EnumerateDirectories |> List.ofSeq |> getAllFiles
        ]

[<RequireQualifiedAccess>]
module Option =
    module Operators =
        let (=>) key value = (key, value)

[<RequireQualifiedAccess>]
module String =
    open System

    let toLower (value: string) =
        value.ToLower()

    let ucFirst (value: string) =
        match value |> Seq.toList with
        | [] -> ""
        | first :: rest -> (string first).ToUpper() :: (rest |> List.map string) |> String.concat ""

    let split (separator: string) (value: string) =
        value.Split(separator) |> Seq.toList

    let replaceAll (replace: string list) replacement (value: string) =
        replace
        |> List.fold (fun (value: string) toRemove ->
            value.Replace(toRemove, replacement)
        ) value

    let remove toRemove = replaceAll toRemove ""

    let append suffix string =
        sprintf "%s%s" string suffix

    let trimEnd (char: char) (string: string) =
        string.TrimEnd char

    let trimStart (char: char) (string: string) =
        string.TrimStart char

    let trim (char: char) (string: string) =
        string.Trim char

    let contains (subString: string) (string: string) =
        string.Contains(subString)

    let startsWith (prefix: string) (string: string) =
        string.StartsWith(prefix)

    let (|IsEmpty|_|): string -> _ = function
        | empty when empty |> String.IsNullOrEmpty -> Some ()
        | _ -> None

[<RequireQualifiedAccess>]
module Directory =
    open System.IO

    let ensure (path: string) =
        if path |> Directory.Exists |> not then Directory.CreateDirectory(path) |> ignore

[<RequireQualifiedAccess>]
module Path =
    open System
    open System.IO

    let fileName = String.split "/" >> List.rev >> List.head

    let fileNameWithoutExtension: string -> string = Path.GetFileNameWithoutExtension

    let dirName path =
        let file = path |> fileName
        path.Substring(0, path.Length - file.Length)

    module Operators =
        let (/) a b = Path.Combine(a, b)

    let getFullPathSafe (path: string) =
        try Path.GetFullPath path
        with _ -> path

    let getFileNameSafe (path: string) =
        try Path.GetFileName path
        with _ -> path

    /// Algorithm from https://stackoverflow.com/a/35734486/433393 for converting file paths to uris,
    /// modified slightly to not rely on the System.Path members because they vary per-platform
    let filePathToUri (filePath: string): string =
        let filePath, finished =
            if filePath.Contains "Untitled-" then
                let rg = System.Text.RegularExpressions.Regex.Match(filePath, @"(Untitled-\d+).fsx")
                if rg.Success then
                    rg.Groups.[1].Value, true
                else
                    filePath, false
            else
                filePath, false

        if not finished then
            let uri = System.Text.StringBuilder(filePath.Length)
            for c in filePath do
                if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') ||
                    c = '+' || c = '/' || c = '.' || c = '-' || c = '_' || c = '~' ||
                    c > '\xFF' then
                    uri.Append(c) |> ignore
                // handle windows path separator chars.
                // we _would_ use Path.DirectorySeparator/AltDirectorySeparator, but those vary per-platform and we want this
                // logic to work cross-platform (for tests)
                else if c = '\\' then
                    uri.Append('/') |> ignore
                else
                    uri.Append('%') |> ignore
                    uri.Append((int c).ToString("X2")) |> ignore

            if uri.Length >= 2 && uri.[0] = '/' && uri.[1] = '/' then // UNC path
                "file:" + uri.ToString()
            else
                "file:///" + (uri.ToString()).TrimStart('/')
        else
            "untitled:" + filePath

    /// handles unifying the local-path logic for windows and non-windows paths,
    /// without doing a check based on what the current system's OS is.
    let fileUriToLocalPath (uriString: string) =
        /// a test that checks if the start of the line is a windows-style drive string, for example
        /// /d:, /c:, /z:, etc.
        let isWindowsStyleDriveLetterMatch (s: string) =
            match s.[0..2].ToCharArray() with
            | [| |]
            | [| _ |]
            | [| _; _ |] -> false
            // 26 windows drive letters allowed, only
            | [| '/'; driveLetter; ':' |] when Char.IsLetter driveLetter -> true
            | _ -> false
        let initialLocalPath = Uri(uriString).LocalPath
        let fn =
            if isWindowsStyleDriveLetterMatch initialLocalPath
            then initialLocalPath.TrimStart('/')
            else initialLocalPath
        if uriString.StartsWith "untitled:" then (fn + ".fsx") else fn

[<AutoOpen>]
module Regexp =
    open System.Text.RegularExpressions

    // http://www.fssnip.net/29/title/Regular-expression-active-pattern
    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some (List.tail [ for g in m.Groups -> g.Value ])
        else None

[<RequireQualifiedAccess>]
module List =
    /// see https://stackoverflow.com/questions/32363848/fastest-way-to-reduce-a-list-based-on-another-list-using-f
    let filterNotIn excluding list =
        let toExclude = set excluding
        list |> List.filter (toExclude.Contains >> not)

    let filterNotInBy f excluding list =
        let toExclude = set excluding
        list |> List.filter (f >> toExclude.Contains >> not)

    let filterInBy f including list =
        let toInclude = set including
        list |> List.filter (f >> toInclude.Contains)

    let formatLines linePrefix f = function
        | [] -> ""
        | lines ->
            let newLineWithPrefix = "\n" + linePrefix

            lines
            |> List.map f
            |> String.concat newLineWithPrefix
            |> (+) newLineWithPrefix

    let formatAvailableItems onEmpty onItems wantedItem definedItems =
        let normalizeItem =
            String.toLower

        let similarDefinedItem =
            definedItems
            |> List.tryFind (normalizeItem >> (=) (wantedItem |> normalizeItem))

        let availableItems =
            definedItems
            |> List.map (function
                | similarItem when (Some similarItem) = similarDefinedItem -> sprintf "%s  <--- maybe this one here?" similarItem
                | item -> item
            )

        match availableItems with
        | [] -> onEmpty
        | items -> items |> onItems

    /// It splits a list by a true/false result of the given function, when the first false occures, it will left all other items in false branch
    /// Example: [ 2; 4; 6; 7; 8; 9; 10 ] |> List.splitBy isEven results in ( [ 2; 4; 6 ], [ 7; 8; 9; 10 ] )
    let splitBy f list =
        let rec splitter trueBranch falseBranch f = function
            | [] -> trueBranch |> List.rev, falseBranch
            | i :: rest ->
                let trueBranch, falseBranch, rest =
                    if i |> f
                        then i :: trueBranch, falseBranch, rest
                        else trueBranch, falseBranch @ i :: rest, []

                rest |> splitter trueBranch falseBranch f

        list |> splitter [] [] f

[<RequireQualifiedAccess>]
module Map =
    let keys map =
        map
        |> Map.toList
        |> List.map fst

[<AutoOpen>]
module Utils =
    let tee f a =
        f a
        a
