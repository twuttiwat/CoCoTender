namespace CoCoTender.Storage

open System

open CoCoTender.Domain
open CoCoTender.Domain.Project

type IStorageApi =
    abstract member getBoQItems : unit -> Result<BoQItem list,string>
    abstract member addBoQItem : boqItem:BoQItem -> Result<unit, string>
    abstract member updateBoQItem : boqItem:BoQItem -> Result<unit, string>
    abstract member deleteBoQItem : itemId:Guid -> Result<Unit, string>
    abstract member loadFactorFTable : unit -> FactorFTable

