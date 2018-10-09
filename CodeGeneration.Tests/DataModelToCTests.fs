module DataModelToCTests

open System

open Expecto

open BReusable
open BReusable.StringHelpers
open DataModelToC


let simpleProp name t =
    {Name=name;Type=t;Comment=null;IsWritable=true}
[<Tests>]
let typeGeneration =
    testList "TypeGeneration" [
        let simpleProps =
            [
                simpleProp "Approved" "bool"
                simpleProp "ResponseCode" "int"
                simpleProp "ResponseCodeDescription" "string"
                simpleProp "ResponseMessage" "string"
                simpleProp "TransactionId" "string"
            ] 
        yield testCase "canGenerateAnInterface" <|
            fun () ->
                let expected =
                    [
                        "public interface IAuthorizationResponse"
                        "{"
                        "  bool Approved { get; set; }"
                        "  int ResponseCode { get; set; }"
                        "  string ResponseCodeDescription { get; set; }"
                        "  string ResponseMessage { get; set; }"
                        "  string TransactionId { get; set; }"
                        "}"
                    ] |> delimit Environment.NewLine
                let actual =
                    generateInterface "IAuthorizationResponse" Seq.empty false
                        simpleProps (DataModelToC.Impl.defaultIndenter "  ")
                Expect.equal actual expected "bad interface generation"
        yield testCase"canGenerateAClass" <|
            fun () ->
                let expected =
                    [
                        "public class CardAuthorizationResponse : IAuthorizationResponse"
                        "{"
                        "  public bool Approved { get; set; }"
                        "  public int ResponseCode { get; set; }"
                        "  public string ResponseCodeDescription { get; set; }"
                        "  public string ResponseMessage { get; set; }"
                        "  public string TransactionId { get; set; }"
                        "}"
                    ] |> delimit Environment.NewLine
                let actual =
                    generateClass "CardAuthorizationResponse" ["IAuthorizationResponse"] false simpleProps (DataModelToC.Impl.defaultIndenter "  ")
                Expect.equal actual expected "bad class generation"

    ]

