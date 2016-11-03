// fparsec practice
#r 
MacroRunner.NuGetAlternative.
#if NUGET
#endif
#if INTERACTIVE

#r "D:\Users\Dbee\AppData\Local\LINQPad\NuGet.FW46\FParsec\FParsec.1.0.2\lib\portable-net45+netcore45+wpa81+wp8\FParsecCS.dll"
#r "D:\Users\Dbee\AppData\Local\LINQPad\NuGet.FW46\FParsec\FParsec.1.0.2\lib\portable-net45+netcore45+wpa81+wp8\FParsec.dll"
open FParsec
#endif

let test p str = 
    match run p str with
    |Success(result, _, _)   -> printfn "Success: %A" result
    |Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
    
// following http://trelford.com/blog/post/parsecsharp.aspx (http://fssnip.net/lf)
module Ast =
    // Base type abbreviations
    type Name = string
    type VarName = Name
    type TypeName = Name
    type MemberName = Name
    type LabelName = Name
    type Value = obj
    type Literal = Literal of Value
    // Expressions
    type ArgType = ValueArg | RefArg | OutArg
    
    type Expr = 
        | Value of Literal
        | Variable of VarName
        | MethodInvoke of MemberName * Arg list
        | PropertyGet of MemberName
        | Cast of TypeName * Expr
        | InfixOp of Expr * string * Expr
        | PrefixOp of string * Expr
        | PostfixOp of Expr * string
        | TernaryOp of Expr * Expr * Expr
        | ObjectConstructor of TypeName * Arg list * objectInitializers: ObjectInitializer option (* { items in the case of an array or dict, or property intializers } *)
    and ObjectInitializer = | ArrayInitialization of Expr list | PropertyInitialization of (MemberName * Expr) list
    and Arg = Arg of ArgType * Expr
    // Statements
    type Define = Define of TypeName * VarName
    type Init = 
        | Assign of Name * (* =,+=, etc. *) Expr
        | Construct of TypeName * Name * Expr
    type Condition = Expr
    type Iterator = Expr
    type Statement =
        | Definition of Define
        | Assignment of Init
        | PropertySet of MemberName * Expr
        | Action of Expr
        //| Block/Scope of Statement list
        | If of Expr * Block
        | IfElse of Expr * Block * Block
        | Switch of Expr * Case list
        | For of Init list * Condition * Iterator list * Block
        | ForEach of Define * Expr * Block
        | While of Expr * Block
        | DoWhile of Block * Expr
        | Throw of Expr
        | Try of Block
        | Catch of TypeName * Block
        | Finally of Block
        | Lock of Expr * Block
        | Using of Expr * Block
        | Label of LabelName
        | Goto of LabelName
        | Break
        | Continue
        | Return of Expr
        //| Directive of Name
    and Case = 
        | Case of Literal * Block
        | Default of Block
    and Block = Statement list
    // Modifiers
    type Access = Public | Private | Protected | Internal
    type Modifier = Static | Sealed | Override | Virtual | Abstract
    // Members
    type ReturnType = TypeName
    type MemberInfo = MemberInfo of Access * Modifier option * ReturnType * Name
    type IsReadOnly = bool
    type ParamType = ByValue | ByRef | Out | Params
    type Param = Param of ParamType * TypeName * VarName
    type PreConstruct = PreConstruct of Name * Param list
    type Member =
        | Field of Access * Modifier option * IsReadOnly * 
                   ReturnType * Name * Expr option
        | Property of MemberInfo * Block option * Block option
        | Method of MemberInfo * Param list * Block
        | Constructor of Access * Modifier option * Name * Param list * 
                         PreConstruct option * Block
    // Types
    type Members = Member list
    type Implements = Name list
    type EnumValue = EnumValue of Name * Value
    type CSharpType = 
        | Class of Access * Modifier option * Name * Implements * Members
        | Struct of Access * Name * Member list
        | Interface of Access * Name * Implements * Member list
        | Enum of Access * TypeName * EnumValue list
        | Delegate of Access * Name * ReturnType * Param list
    // Namespace scopes
    type Import = 
        | Import of Name list
        | Alias of Name * Name list
    type NamespaceScope =
        | Namespace of Import list * Name list * NamespaceScope list
        | Types of Import list * CSharpType list
        
[<AutoOpen>]
module Parser =  
    open Ast
    let maxCount = System.Int32.MaxValue
    let pcomment = pstring "//" >>. many1Satisfy ((<>) '\n') 
    let pspaces = spaces >>. many (spaces >>. pcomment >>. spaces)
    let pmlcomment = pstring "/*" >>. skipCharsTillString "*/" true (maxCount)
    let ws = pspaces >>. many (pspaces >>. pmlcomment .>> pspaces) |>> (function | [] -> () | x -> printfn "found comments %A" x)
    let ws1 = spaces1
    let str_ws s = pstring s .>> ws
    let str_ws1 s = pstring s .>> ws1

    // Literals
    
    type Lit = NumberLiteralOptions
    let numberFormat = Lit.AllowMinusSign ||| Lit.AllowFraction ||| Lit.AllowExponent
    let pnumber : Parser<Literal, unit> =
        numberLiteral numberFormat "number"
        |>> fun nl ->
                if nl.IsInteger then Literal(int nl.String)
                else Literal(float nl.String)
    let ptrue = str_ws "true" |>> fun _ -> Literal(true)
    let pfalse = str_ws "false" |>> fun _ -> Literal(false)
    let pbool = ptrue <|> pfalse
    let pstringliteral =
        let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
        let unescape c = match c with
                         | 'n' -> '\n'
                         | 'r' -> '\r'
                         | 't' -> '\t'
                         | c   -> c
        let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
        between (pstring "\"") (pstring "\"")
                (manyChars (normalChar <|> escapedChar))
        |>> fun s -> Literal(s)

    let pliteral = pnumber <|> pbool <|> pstringliteral

    // Expressions

    let reserved = ["for";"do"; "while";"if";"switch";"case";"default";"break";"new" (*;...*)]
    let pidentifierraw =
        let isIdentifierFirstChar c = isLetter c || c = '_' || c = '@'
        let isIdentifierChar c = isLetter c || isDigit c || c = '_'
        many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
    let pidentifier =
        pidentifierraw 
        <??> "pidentifier"
        >>= fun s -> 
            if s.StartsWith("@") || reserved |> List.exists ((=) s) then fail "keyword" 
            else preturn s

    let pidentifier_ws = pidentifier .>> ws
    let pvar = 
        pidentifier 
        <??> "pvar"
        >>=  fun s ->
            preturn s
        |>> fun x -> Variable(x)

    let pargref = str_ws1 "ref" |>> fun _ -> RefArg
    let pargout = str_ws1 "out" |>> fun _ -> OutArg
    let pargtype = (opt pargref <|> opt pargout) 
                   |>> function Some x -> x | None -> ValueArg

    let pexpr_, pexprimpl = createParserForwardedToRef ()
    let pexpr = 
        pexpr_ <??> "pexpr"

    let parg = pargtype .>>. pexpr .>> ws <??> "parg" |>> fun (by,e) -> Arg(by,e)

    let pinvoke =
        pidentifier_ws .>>.
        // this appears it would have failed on a multi arg method call
        between (str_ws "(") (str_ws ")") (sepBy parg (str_ws ",")) 
        <??> "pinvoke"
        |>> fun (name,args) -> MethodInvoke(name,args)


    // (could contain more pconstructors inside but they should also be exprs right?)
    // let pexpr, pexprimpl = createParserForwardedToRef ()
    // object initializer or array initializer 
    // X = Expr, ... ; 
    // or 
    // Expr,Expr,Expr
//    let pobject = passign

    // should match {} { ws } or { Some p }
    let commaSepOptTrail p =
        let trailingComma = (str_ws "," |>> fun _ -> None )
        ((opt (attempt (sepBy p (str_ws ",")))) <|> (attempt trailingComma))
    let exprBraceSeqOpt p =
        
        between (str_ws "{") (str_ws "}") (commaSepOptTrail p)
    // `new` expressions
    let pconstructorArgs = // not accounting for named params yet
        between (str_ws "(") (str_ws ")") (sepBy parg (str_ws ","))  
        <??> "pconstructorArgs expected (comma delimited args)"
        |>> fun args -> args
    let ptypedarray = 
        pidentifier_ws .>>. opt (str_ws "[]")
        <??> "ptypedarray expected identifier with optional trailing []"
        |>> fun (x,y) -> defaultArg y System.String.Empty |> (+) x
    let ptype = 
        attempt ptypedarray <|> str_ws "[]"
//        |>> id
        <??> "ptype expected identifer or '[]'"

    let parrayinitializer =
        exprBraceSeqOpt pexpr .>> optional (str_ws ",") <??> "parrayinitializer"
//        between (str_ws "{") (str_ws "}") (many pexpr)
        |>> fun args -> 
            printfn "array initializer returning %A" args
            args
            |> Option.map ObjectInitializer.ArrayInitialization
//    let pobjinitializer,pbojinitializerImpl = createParserForwardedToRef ()

    let pobjinitializer =
        exprBraceSeqOpt (
            pidentifier 
            .>> ws 
            .>> str_ws "=" <??> "pbojinitializer.'='" 
            .>>. (pexpr <??> "pbojinitializer.pexpr") <??> "pexpr.'='.pexpr" 
            //.>> attempt (str_ws "," .>> (lookAhead (str_ws "}")))
            )
        <??> "pobjinitializer"
        |>> fun args -> 
            printfn "pobjinit %A" args
            args
            |> Option.map ObjectInitializer.PropertyInitialization

    // used in new X where x can be identifier or int[] for example

    let pconstructorcall =  // not necessarily pinvoke may not have ()
        str_ws "new " 
        >>. ptype 
        .>> ws 
        .>>. ((opt (attempt pconstructorArgs)) <??> "opt attempt pconstructorArgs") 
        .>> ws 
        .>>. (opt (attempt parrayinitializer <|> attempt pobjinitializer) <??> "addl constructor stuff")
        <??> "pconstructorcall"
        //(name,args),objectInitializers
        |>> fun ((name,args),oi) -> 
            let oi = oi |> Option.bind id
            let args = defaultArg args List.empty
            printfn "constructor name=%s, args=%A, oi=%A" name args oi
            ObjectConstructor(name, args, oi)

    let pcast =
        let ptypecast = between (str_ws "(") (str_ws ")") pidentifier_ws
        ptypecast .>>. pexpr |>> fun (name,e) -> Cast(name,e)
    
    let pvalue = (pliteral <??> "pliteral" |>> fun x -> Value(x)) <|> 
                 attempt pconstructorcall <??> "pconstructorcall" <|> attempt pinvoke <??> "pinvoke" <|> attempt pvar <??> "pvar" <|> attempt pcast 

    pexprimpl:= pvalue

let text = """new TableInfo[] 
        {
            new TableInfo
            {
                Name="Payment",
                Schema="dbo",
                Columns = new []
                {
                    new ColumnInfo{Name="PaymentID", Type = typeof(int), 
                        Attributes = new []{"identity","primary key"},
                        },
                    CreateFKeyedColumn<int>("AppointmentId", new FKeyInfo{Schema="dbo",Table="Appointments",Column="AppointmentId"}, /* allowNull= */ true),
                    new ColumnInfo
                    {
                        Name="PaymentTypeId", Type= typeof(string), Length=50,
                        FKey=new FKeyInfo{ Schema="Accounts", Table="PaymentType",Column="PaymentTypeId" },
                        GenerateReferenceTable = true,
                        ReferenceValuesWithComment= new Dictionary<string,string>{
                            {"Patient",null},{"ThirdParty",null},
                            {"Era",null}
                            },
                        Comments= new[]{
                            "|Patient of PatientIdentifier * PatientPayment |ThirdParty of PayerIdentifier * ThirdPartyPayment |Era of PayerIdentifier * EraPaymentMethod"
                            }
                        },
                    new ColumnInfo{
                        Name="PaymentMethodId", Type = typeof(string),
                        Length=50,
                        GenerateReferenceTable = true,
                        FKey=new FKeyInfo{ Schema="Accounts", Table="PaymentMethod"},
                        ReferenceValuesWithComment = new Dictionary<string,string>{
                            {"Cash",null},{"CC",null},{"Check",null},{"Ach",null},{"Fsa",null},{"Other","for when Era amount is 0 or a catch-all"}
                            },
                        },
                    new ColumnInfo{
                        Name="PaymentStatusId", Type = typeof(string),
                        Length=50,
                        GenerateReferenceTable = true,
                        FKey=new FKeyInfo{ Schema="Accounts", Table="PaymentStatus"},
                        ReferenceValuesWithComment = new []{"New", "Partial", "Complete"}.ToDictionary(f=>f,f=> (string)null),
                        },
                    new ColumnInfo{
                        Name="TotalAmount", Type = typeof(decimal),
                        Precision=12,Scale=2, // see: http://stackoverflow.com/questions/2377174/how-do-i-interpret-precision-and-scale-of-a-number-in-a-database
                        Comments = new[]{ "was Amount (18,2)"}
                        },
                    CreateUserIdColumn(null, true, "null to allow system inserts/adjustments that aren't done by a user"),
                    CreateFKeyedColumn<int>("PayerID", new FKeyInfo{ Schema="dbo", Table="Payers" }, /* allowNull= */ true),
                    CreatePatientIdColumn(null, true,null),
                    new ColumnInfo{
                        Name="Created", Type = typeof(DateTime),
                        AllowNull=true,
                        Comments = new[]{ "was timestamp"}
                        },
                    new ColumnInfo{
                        Name="TransactionNumber", Type = typeof(string),
                        Length=30,
                        AllowNull=true,
                        Comments = new[]{ "was checkNumber now will store check number or ACH number (when applicable)"}
                        },
                    new ColumnInfo{
                        Name="Rcd", Type = typeof(DateTime),
                        Comments = new []{"Payment Recvd"},
                        AllowNull=true,
                        },
                    new ColumnInfo{
                        Name="IsElectronic", Type = typeof(bool),
                        },
                    CreateFKeyedColumn<int>("CCItemID", new FKeyInfo{ Schema="Accounts", Table="CCItem"},true),
                    new ColumnInfo{
                        Name="Comments", Type = typeof(string),
                        UseMax=true,
                        AllowNull=true,
                        },
                    }
                },
            new TableInfo{
                Schema="Accounts",
                Name="CCItem",
                Columns = new []
                {
                    new ColumnInfo{
                        Name="CCItemID", Type = typeof(int), Attributes = new []{"identity","primary key"},
                        },
                    MakeNullable50("ResponseCode"),
                    MakeNullable50("ResponseDescription"),
                    MakeNullable50("TransactionID"),
                    MakeNullable50("TransactionType"),
                    MakeNullable50("CardType"),
                    MakeNullable50("MaskedAcctNum"),
                    MakeNullable50("ExpDate"),
                    MakeNullable50("AcctNumSource"),
                    MakeNullable50("CardholderName"),
                    MakeNullable50("Alias"),
                    MakeNullable50("ProcessorResponse"),
                    MakeNullable50("BatchNum"),
                    MakeNullable50("BatchAmount"),
                    MakeNullable50("ApprovalCode"),
                    }
                },
            new TableInfo{
                Schema="Accounts",
                Name="PaymentItem",
                Columns = new []{
                    new ColumnInfo{ Name = "PaymentItemID", Type = typeof(int), Attributes = new []{"identity","primary key"}},
                    new ColumnInfo{ Name = "PaymentID", Type = typeof(int), FKey= new FKeyInfo{Schema="dbo",Table="Payment"}},
                    new ColumnInfo{ Name = "PaymentItemTypeId", Type = typeof(string), Length=50,
                        AllowNull=true,
                        GenerateReferenceTable=true, FKey= new FKeyInfo{Schema="Accounts", Table="PaymentItemType", Column="PaymentItemTypeId"},
                        ReferenceValuesWithComment = new []{"EraPayment", "EraAdjustment", "PtRespDeductible", "PtRespCoPay","PtRespCoIns","Other"}.ToDictionary(f => f, f => (string)null),
                    },
                    new ColumnInfo{
                        Name="PaymentTierId", Type = typeof(string),
                        Length=50,
                        GenerateReferenceTable = true,
                        AllowNull=true,
                        FKey=new FKeyInfo{ Schema="Accounts", Table="PaymentTier",Column="PaymentTierId" },
                        ReferenceValuesWithComment = new []{"Primary", "Secondary", "Tertiary", "Worker'sComp"}.ToDictionary(f=>f,f=> (string)null),
                        },
                    new ColumnInfo{
                        Name="PtRespTypeId", Type = typeof(string),
                        Length=50,
                        GenerateReferenceTable = true,
                        AllowNull=true,
                        FKey=new FKeyInfo{ Schema="Accounts", Table="PtRespType",Column="PtRespTypeId" },
                        ReferenceValuesWithComment = new []{"Deductible", "CoIns", "CoPay"}.ToDictionary(f=>f,f=> (string)null),
                        },
                    new ColumnInfo{ Name = "Created", Type= typeof(DateTime)},
                    new ColumnInfo{ Name = "Amount", Type= typeof(decimal), Precision=8, Scale=2},
                    new ColumnInfo{ Name = "PatientResponsiblityAmt", Type = typeof(decimal), Precision=8, Scale=2},
                    CreateFKeyedColumn<int>("ChargeID", new FKeyInfo{Schema="dbo",Table="Charge"},true),
                    MakeNullable50("RemarkCode"),
                    MakeNullable50("AdjustmentCode"),
                    new ColumnInfo{ Name = "PaymentItemStatusId", Type = typeof(string), Length=50,
                        GenerateReferenceTable=true, FKey= new FKeyInfo{Schema="Accounts", Table="PaymentItemStatus"},
                        ReferenceValuesWithComment = new []{"Posted", "Unposted"}.ToDictionary(f=>f,f=> (string)null),
                    },
                    new ColumnInfo{
                        Name="Comments", Type = typeof(string),
                        UseMax=true,
                        AllowNull=true,
                    },
                }
            }
        }
"""

let text1 = """new ColumnInfo{Name="PaymentID", Type = typeof(int), 
                        Attributes = new []{"identity","primary key"},
                        }"""
let text2 = """new ColumnInfo{Name="PaymentID", Type = typeof(int), 
                        Attributes = new []{"identity","primary key"}
                        }"""
let text3 = """new ColumnInfo{Name="PaymentID", Type = typeof(int), 
                        Attributes = jim
                        }"""

module Tests = 
    let tests = 
        [
            pobjinitializer, """{Name="PaymentID"}"""
            pobjinitializer, """{Name="PaymentID", Type = typeof(int),}"""
            pobjinitializer, """new ColumnInfo{Name="PaymentID", Type = typeof(int)}"""
        ]
    let runTests() =
        tests
        |> Seq.mapi (fun i (x,y) -> i,x,y)
        |> Seq.tryFind (fun (_,p,s) -> run p s |> function | Failure _ -> true | _ -> false)
        |> Option.iter (fun (i,p,s) -> printfn "Failing: %i" i; test p s )
    
open Ast
//match run pconstructorcall text with | Success (a,b,c) -> sprintf "%A" (a,b,c);;
//Expr.ObjectConstructor(x,y,z)
let rec makeF = 
    function
    | ObjectConstructor(x,y,z) as e ->
        printfn "%A" (x,y)
        if false then
            makeF e
match run pexpr text with 
| Success (ObjectConstructor(x,y,z) as e,b,c) -> Some e 
| Success (Variable "new" as e,b,c) -> None
| x -> 
    printfn "unexpected %A" x
    None
|> Option.map (fun e -> printfn "clearing"; printfn "--------"; makeF e |> printfn "result:%A")