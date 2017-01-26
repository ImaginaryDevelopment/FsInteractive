module CodeGeneration.GenerateFromTypes
open System

// some parts of this may be from a translation of BMore.linq
[<AutoOpen>]
module TypeMapper = 
    open BReusable.Reflection
    let mapTypeToF = 
        function
        | TypeOf(isType:int) -> "int"
        | TypeOf(isType:string) -> "string"
        | TypeOf(isType:Nullable<DateTime>) -> "DateTime Nullable"
        | TypeOf(isType:Nullable<int>) -> "int Nullable"
        | TypeOf(isType:Nullable<bool>) -> "bool Nullable"
        | x -> x.Name

[<AutoOpen>]
module StringAssembler = //Abstract Data Type
    open System.Text

    type Sb = 
        private 
            { SB:StringBuilder}
        with
            override x.ToString() = x.SB.ToString()
            
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Sb =
        let startAssembler() = {SB = StringBuilder()}
        let appendLine i (s:string) (sb:Sb) =
            let addlIndent = List.replicate i "  " |> Seq.fold (+) String.Empty
            sb.SB.AppendLine(sprintf "%s%s" addlIndent s) |> ignore
            sb

module SimplifiedInput =
    type StringLength= |Length of int | UseMax
    type NumberDetails = { Scale:int;Precision:int}
    type DbType =
        | String of StringLength
        | Money of NumberDetails

    type ColumnInfo = {Name:string; Type:Type; Length: int option; Precision: int option; Scale: int option; UseMax:bool}
    type TableInfo = {Name:string; Schema: string; Columns: ColumnInfo seq}

//let dc = new TypedDataContext()
    // 't is a container type of some sort, in LinqPad it would be Table<_>
    [<Obsolete("this doesn't work, typedefof<'t> isn't referring to a real arg")>]
    let extractTypeInfo (t:Type) =
        t
        |> fun t -> t.GetProperties()
        |> Seq.filter (fun p -> p.Name.StartsWith("_") |> not)
        |> Seq.filter (fun p -> p.PropertyType.IsGenericType && p.PropertyType.GetGenericTypeDefinition() = typedefof<'t> )
        |> Seq.map (fun p -> p.Name, p.PropertyType.GenericTypeArguments.[0])
        |> Seq.map (fun (name,t) -> name, t.Name, t.GetFields() |> Seq.map(fun f -> f.Name, f.FieldType))

    [<Obsolete("this doesn't work")>]
    let extractInfo<'t> () = extractTypeInfo typeof<'t>

//let generateFromType<'t>() = 

module ModelGenerator = 
    type GenerationStyle = 
        |Readonly
        |Writable
        
    type GenerationType =
        |Value
        |Entity
    
    let generateInterface gs sb (indent:string) (name:string) (props:seq<string*Type>) =
        
        let addProps i sb = 
            let suffix = match gs with |Readonly -> String.Empty |Writable -> " with get,set"
            props 
            |> Seq.map (fun (name,t) -> 
                sprintf "/// %s" t.FullName, sprintf "abstract member %s:%s%s" name (mapTypeToF t) suffix
                )
            |> Seq.iter (fun (comment,text) -> 
                sb
                |> Sb.appendLine i comment
                |> Sb.appendLine i text 
                |> ignore
                )
            sb
            
        let iname = match gs with |Readonly -> "" |Writable -> "RW"
        
        sb 
        |> Sb.appendLine 0 String.Empty
        |> Sb.appendLine 1 (sprintf "type I%s%s =" iname name)
        |> addProps 2
    #if LINQPAD
    let getTypeToGen() = Util.Cache(fun () -> dc.GetType()) 
    let dumpt (t:string) x = x.Dump(t); x
    #else
    type private User () = 
        member val UserId = 0 with get,set
    and private Table<'t>() = 
        member val TableId = 0 with get,set
    and private SampleType() = 
        member val Users:Table<User> = Unchecked.defaultof<_>
    let getTypeToGen() = typeof<SampleType>
    let dumpt (t:string) x = printfn "%s:%A" t x; x
    #endif

//let generateRecord typeName 
    let sampleGeneratorCall() = 
        let limit = 5
        let sb = Sb.startAssembler()
        let types = 
            getTypeToGen()
            |> SimplifiedInput.extractTypeInfo
            |> Seq.take 5
            |> Array.ofSeq
            
    #if LINQPAD
        Newtonsoft.Json.JsonConvert.SerializeObject(types).Dump("shape input")
    #endif

        try
            types
            |> Seq.map (fun (_,name,t) -> name, generateInterface Readonly sb String.Empty name t)
            |> Seq.iter (fun (name,s)-> (s |> string |> dumpt name) |> ignore<string>)
        finally
            types
            |> dumpt "typeInfo"
            |> ignore

let sampleShapeInput = 
    """[
  {
    "Item1": "Accidents",
    "Item2": "Accidents",
    "Item3": [
      {
        "Item1": "AccidentID",
        "Item2": "System.Int32, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
      },
      {
        "Item1": "DisplayText",
        "Item2": "System.String, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
      },
      {
        "Item1": "Code",
        "Item2": "System.String, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
      }
    ]
  },
  {
    "Item1": "Accounts",
    "Item2": "Account",
    "Item3": [
      {
        "Item1": "AccountID",
        "Item2": "System.Int32, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
      },
      {
        "Item1": "AccountTypeId",
        "Item2": "System.String, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
      },
      {
        "Item1": "Name",
        "Item2": "System.String, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
      },
      {
        "Item1": "PayerID",
        "Item2": "System.Nullable`1[[System.Int32, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089]], mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
      }
    ]
  },
  {
    "Item1": "AccountPatients",
    "Item2": "AccountPatient",
    "Item3": [
      {
        "Item1": "AccountID",
        "Item2": "System.Int32, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
      },
      {
        "Item1": "PatientID",
        "Item2": "System.Int32, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
      },
      {
        "Item1": "Name",
        "Item2": "System.Int32, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
      }
    ]
  },
  {
    "Item1": "AccountTypes",
    "Item2": "AccountType",
    "Item3": [
      {
        "Item1": "AccountTypeId",
        "Item2": "System.String, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
      }
    ]
  },
  {
    "Item1": "AdjustmentReasons",
    "Item2": "AdjustmentReasons",
    "Item3": [
      {
        "Item1": "Code",
        "Item2": "System.String, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
      },
      {
        "Item1": "Description",
        "Item2": "System.String, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
      },
      {
        "Item1": "EffectiveDate",
        "Item2": "System.String, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
      },
      {
        "Item1": "DeactivationDate",
        "Item2": "System.String, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
      },
      {
        "Item1": "LastModifiedDate",
        "Item2": "System.String, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
      },
      {
        "Item1": "Notes",
        "Item2": "System.String, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
      },
      {
        "Item1": "CodeType",
        "Item2": "System.String, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
      }
    ]
  }
]"""