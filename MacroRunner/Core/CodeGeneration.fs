namespace BCore.CodeGeneration
// types should only go here that multiple libraries and the public interface is concerned with
open System.Collections.Generic
open System.Text

// TODO: review this for unneccesary core types
module DteWrapCore =
    type IProject =
            abstract member Name:string option
            abstract member FullName:string option
    // interface for EnvDte.ProjectItems
    [<AllowNullLiteral>]
    type IProjectItemsCollection =
            inherit IEnumerable<IProjectItem>
            abstract member AddFromFile:string -> IProjectItem
    and [<AllowNullLiteral>] IProjectItem =
        abstract member Name:string with get
        // indexer property, don't think F# supports the direct syntax
        abstract member get_FileNames:int16 -> string
        abstract member ProjectItems:IProjectItemsCollection with get

    // leaking: GetProjectItems
    [<NoComparison;NoEquality>]
    type ProjectWrapper = {
        GetProjectItems: unit -> IProjectItemsCollection // EnvDTE.ProjectItems
        GetFullName: unit -> string
        GetName: unit -> string
        GetKind: unit -> string
    }

    [<NoComparison;NoEquality>]
    type SourceControlWrapper = {
        GetIsItemUnderSC: string -> bool
        GetIsItemCheckedOut: string -> bool
        CheckOutItem:string -> bool
    }

    // abstract away any specific needs the manager has to interact with DTE
    // leaking: GetProjects method
    [<NoComparison;NoEquality>]
    type DteWrapper = {
        FindProjectItemPropertyValue: string -> string -> string
        GetSourceControl: unit -> SourceControlWrapper option
        GetProjects: unit -> ProjectWrapper list
        Log: string -> unit
        }

    type CreateProcessResult =
        |Unchanged
        |Changed

    type IManager =
        abstract member StartNewFile : Filename:string -> unit
        abstract member TemplateFile: string with get
        abstract member DteWrapperOpt : DteWrapper option with get
        abstract member EndBlock: unit -> unit
        // return the input (startNewFile or null for main file) mapped to the full file path created
        abstract member Process: doMultiFile:bool -> IDictionary<string,CreateProcessResult*string>
        abstract member DefaultProjectNamespace: string with get
        abstract member GeneratedFileNames : string seq with get
        abstract member GetTextSize: unit -> int


// TODO: review this for unneccesary core types
module SqlWrapCore =
    type TableIdentifier = {Schema:string; Name:string;}
    type FKeyIdentifier = {Table: TableIdentifier; Column:string}
    type ReferenceData = {FKeyId:FKeyIdentifier; GenerateReferenceTable:bool; ValuesWithComment: IDictionary<string,string>}
    type InsertsGenerationConfig =
        {
            InsertTitling: string
            // @"Scripts\Post-Deployment\TableInserts\Accounting1.5\AccountingInserts.sql";
            // or @"Scripts\Post-Deployment\TableInserts\Accounting1.5\AccountingGeneratorInserts.sql";
            TargetInsertRelativePath: string
            AdditionalReferenceData: ReferenceData seq
        }
    type FKey =
        |FKeyIdentifier of FKeyIdentifier
        |FKeyWithReference of ReferenceData

    module ColumnTyping =
        type Precision private(p) =
            member __.P = p
            static member Default = Precision(18uy)
            static member Create p =
                if 1uy <= p && p <= 38uy then
                    Precision(p)
                    |> Some
                else None
        type Scale private (s) =
            member __.S = s
            static member Default = Scale(0uy)
            static member Create p =
                if 0uy <= p && p <= 38uy then
                    Scale(p)
                    |> Some
                else None
        type ColumnPS = {Precision:Precision; Scale: Scale}
        // only accounting for varchar text fields currently
        type ColumnType =
            | Bit
            ///// 0 - 255
            //| TinyIntColumn
            ///// up to 32,767
            //| SmallIntColumn
            /// up to 2,147,483,647
            | IntColumn
            | IdentityColumn
            | StringColumn of length:int
            | StringMax
            | NStringMax
            | NStringColumn of length:int
            | Floater of ColumnPS option
            | DecimalColumn of ColumnPS option
            | DateTimeColumn
            | UniqueIdentifier
            // in case you want small datetime for a datetime
            | Custom of string
            with
                member x.NeedsQuoted =
                    match x with
                    | StringColumn _
                    | StringMax
                    | NStringMax
                    | NStringColumn _
                    | DateTimeColumn
                    | Custom _
                    | UniqueIdentifier -> true
                    | _ -> false

    open ColumnTyping
    // differences: UseMax, Scale, Precision added (base would be ColumnGenerationInfo<Type>)
    type Nullability =
        | AllowNull
        | NotNull
        | Computed of isNullable:bool
        | PrimaryKey
        with member x.IsNullable = match x with |AllowNull -> true |Computed a -> a | _ -> false
    type Uniqueness =
        | Unique
        | NotUnique
    type ColumnInput = {
            Name:string
            ColumnType: ColumnTyping.ColumnType
            Nullability: Nullability
            //Attributes:string list
            FKey:FKey option
            DefaultValue: string
            Comments: string list
    //        GenerateReferenceTable: bool
    //        ReferenceValuesWithComment: IDictionary<string,string>
            IsUnique: Uniqueness
        } with
            member x.IsPrimaryKey =
                match x.Nullability with |PrimaryKey -> true | _ -> false
            member x.IsIdentity =
                match x.ColumnType with
                | IdentityColumn -> true
                | _ -> false
            member x.IsComputed =
                match x.Nullability with
                | Computed a -> a
                | _ -> false
    type TableGenerationInfo = {Id:TableIdentifier; Columns: ColumnInput list}
