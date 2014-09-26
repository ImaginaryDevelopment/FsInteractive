
namespace Macros

open System
open System.DirectoryServices

module Ldap = 
    let private after (value:string) (delimiter:string) = 
        value.Substring(value.IndexOf(delimiter))
    let private afterOrSelf (value:string) delimiter = 
        if value.Contains(delimiter) then value |> after delimiter else value
    let LookupDomainUserById (ids:string seq) = 
        let customPath = sprintf "LDAP://DC=%s,DC=local" Environment.UserDomainName
        use de= new DirectoryEntry(Path=customPath,AuthenticationType = AuthenticationTypes.Secure)
        use deSearch = new DirectorySearcher(SearchRoot=de)
        let tryGetUser () = 
           try
               let user = deSearch.FindOne()
               Some (user.Properties.["name"].[0].ToString())
               with ex -> None
        [
        for id in ids do
            deSearch.Filter <- sprintf "(&(objectClass=user)(SAMAccountname=%s))" (afterOrSelf id "\\")
            let user = tryGetUser()
            if user.IsSome then yield id,user.Value
        ] |> Map.ofSeq
    let LookupComputersInDomain () = 
        let customPath = sprintf "LDAP://DC=%s,DC=local" Environment.UserDomainName
        use de= new DirectoryEntry(Path=customPath,AuthenticationType = AuthenticationTypes.Secure)
        use deSearch = new DirectorySearcher(SearchRoot=de)
        let path = sprintf "(&(ObjectClass=computer)(objectCategory=Computer))"
        deSearch.Filter <- path
        deSearch.FindAll() 
        |> Seq.cast<SearchResult>
        |> Seq.map (fun searchResult -> searchResult.Properties.["cn"].[0] )
        |> Array.ofSeq

    let LookupUsersByComputerName machineName = 
        let path = sprintf "WinNT://%s,computer" machineName
        use computerEntry = new DirectoryEntry(path)
        computerEntry.Children
        |> Seq.cast<DirectoryEntry> 
        //|> Seq.filter (fun child -> child.SchemaClassName = "User") 
        //|> Seq.filter (fun child -> child.Name.Contains("randon"))
        //|> Seq.map (fun child -> child.Name)
        |> Array.ofSeq