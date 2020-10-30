namespace Tuc.LanguageServer

open Tuc.Domain

type Commands = {
    ResolveDomainTypes: string option -> DomainType list    // todo - return Result<DomainType list, DomainParseError>
}
