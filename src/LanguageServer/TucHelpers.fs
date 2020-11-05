namespace Tuc.LanguageServer

module LspHelpers =
    open Tuc
    open Tuc.Domain
    open Tuc.Parser
    open LanguageServerProtocol

    let locations (parsedTucs: ParsedTuc list) =
        []
