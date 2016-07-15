namespace Macros

open System

open Microsoft.TeamFoundation.Client
open Microsoft.TeamFoundation.VersionControl.Client

// usage (assuming your serverItemPath is $/Development/): 
//  #r "tfsmacros";open tfsmacros;let tfs=getTfs();;
//  getTfsChangesByUserAndFile tfs None "$/Development/";;


type ChangeHistory = {ChangesetId:int;Owner:string;CreationDate:DateTime;Changes:string[]}
type ChangeHistoryWithWorkItems = {ChangesetId:int;Owner:string;CreationDate:DateTime;AssociatedWorkItems:(int*string*string)[];Changes:string[]}
type UserChangeHistory = {ChangesetId:int;CreationDate:DateTime;AssociatedWorkItems:(int*string*string)[];Changes:string[]}

// F# module for F# consumption
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] // append module to the name if a type with the same name is defined
module Tfs =
    // assumes that you have a tfs server listed in an environment variable called servers (semicolon delimited list of work servers)
    let GetTfsServerFromEnvironment () = 
      System.Environment.GetEnvironmentVariable("servers").Split([|";"|],StringSplitOptions.None)
      |> Seq.filter (fun s -> s.Contains("tfs"))
      |> Seq.head

    let protocol = "https"
    let GetTfsUri protocol serverName port path =
      sprintf "%s://%s%s%s" protocol serverName 
      <| match port with |Some p -> sprintf ":%i" p |_ -> String.Empty
      <| match path with |Some p -> sprintf "/%s" p | _ -> String.Empty
    let webPort = 8080
    let teamProject = "Development"
    let GetWebLinkBase protocol serverName webPort teamProject = sprintf "%s://%s:%i/DefaultCollection/%s" protocol serverName webPort teamProject
      //match tfsVersion with
      //| "tfs2010" -> sprintf "%s://%s:%i/tfs/web/%s" protocol serverName webPort teamProject
      //| _ -> sprintf "%s://%s:%i/DefaultCollection/%s" protocol serverName webPort teamProject

    let GetChangesetLink webLinkBase changeset = sprintf "%s/_versionControl/changeset/%i" webLinkBase changeset
    let GetItemLink changesetLink path = sprintf "%s#path=%s&_a=compare" changesetLink path
    let GetWorkItemLink webLinkBase workItem = sprintf "%s/_workitems/edit/%i" webLinkBase workItem

    let GetTfs (tfsUri:Uri) = new TfsTeamProjectCollection(tfsUri)
    let GetVcs (tfs:TfsTeamProjectCollection) = tfs.GetService<VersionControlServer>()
    let GetTfsChangesByPath (tfs:TfsTeamProjectCollection) queryPath (user:string option) queryLimit includeDetails = 
      let vcs = GetVcs tfs
      let limitArg = match queryLimit with |None -> Int32.MaxValue | Some x -> x
      let userArg = match user with |None -> null |Some x -> x
      vcs.QueryHistory (queryPath, VersionSpec.Latest, 0, RecursionType.Full, userArg, null, null, limitArg, includeDetails,false)
      |> Seq.cast<Changeset>
      |> Seq.filter (fun cs -> cs.Changes |> Seq.exists (fun change -> change.Item.ServerItem.Contains("/Playground/"))=false )

    /// For example: "$/Development/**.user"
    let GetVcsItemsByWildcard (vcs:VersionControlServer) wildcard = 
        vcs.GetItems(wildcard,RecursionType.Full)

    let GetTfsChangesByUserAndFile (tfs:TfsTeamProjectCollection) (user:string option) querypath (resultLimit:int option) =
      let userArg = match user with |None -> Some Environment.UserName | f -> f
      GetTfsChangesByPath tfs querypath userArg None true
      |> fun items-> if resultLimit.IsSome then Seq.take(resultLimit.Value) items else items
      |> Seq.map (fun cs -> {
                                UserChangeHistory.ChangesetId=cs.ChangesetId;
                                CreationDate=cs.CreationDate;
                                AssociatedWorkItems=(cs.AssociatedWorkItems |> Seq.map (fun wi -> wi.Id,wi.Title,wi.WorkItemType) |> Array.ofSeq);
                                Changes=(cs.Changes |> Seq.map( fun change-> change.Item.ServerItem) |> Array.ofSeq)}
      )
      |> fun i -> userArg,i

    let GetTfsChangesWithoutWorkItems (tfs:TfsTeamProjectCollection) (user:string option) querypath (resultLimit:int option) = 
      GetTfsChangesByPath tfs querypath user None true
      |> Seq.filter (fun cs -> cs.AssociatedWorkItems.Length = 0)
      |> fun items -> if resultLimit.IsSome then Seq.take(resultLimit.Value) items else items
      |> Seq.map (fun cs -> {ChangeHistory.ChangesetId=cs.ChangesetId;
                          Owner=cs.Owner;
                          CreationDate=cs.CreationDate;
                          Changes=(cs.Changes |> Seq.map( fun change-> change.Item.ServerItem) |> Array.ofSeq)})

open System.Runtime.InteropServices

type TFS (serverName, teamProjectName:string, tfs:TfsTeamProjectCollection, [<Optional;DefaultParameterValue(8080)>] ?webPort,[<Optional;DefaultParameterValue("https")>] ?protocol) = 
    let webPort = defaultArg webPort 8080
    let protocol = defaultArg protocol "https"
    let webLinkBase = Tfs.GetWebLinkBase protocol serverName webPort teamProjectName

    static let nullToNone t = if t<>null then Some t else None
    static let noneToNull (t:string option) = if t.IsSome then t.Value else null
    static let defaultToNone t = if t = 0 then None else Some t
    static let optionOfNullable (a : System.Nullable<'T>) = if a.HasValue then Some a.Value else None

    static member GetTfs() = Tfs.GetTfs <| Uri(Tfs.GetTfsUri Tfs.protocol (Tfs.GetTfsServerFromEnvironment()) <| Some Tfs.webPort <| None)

    member x.WebPort = webPort
    member x.Protocol = protocol
    member x.WebLinkBase = webLinkBase
    member x.TfsPC = tfs

    // C# compat optional parameter imitation
    new(serverName,teamProjectName, webport:Nullable<int>, protocol:string, path) = 
        let uri = Tfs.GetTfsUri protocol serverName <| optionOfNullable webport <| Some path
        System.Diagnostics.Trace.WriteLine(sprintf "tfsUri:%A" uri)
        System.Diagnostics.Debug.WriteLine(sprintf "tfsUri:%A" uri)
        new TFS(serverName, teamProjectName, Tfs.GetTfs(Uri(uri)), ?webPort=optionOfNullable webport, ?protocol = nullToNone protocol)

    new() = new TFS(Tfs.GetTfsServerFromEnvironment(),Tfs.teamProject, Nullable<int>(), null,null)

    member x.GetWorkItemLink(workItem) = Tfs.GetWorkItemLink webLinkBase workItem
    member x.GetChangesetLink(changeset) = Tfs.GetChangesetLink webLinkBase changeset
    member x.GetItemLink(changeset,path) = Tfs.GetItemLink <| x.GetChangesetLink(changeset) <| path
    member x.GetChangesByUserAndFile (user:string) querypath (resultLimit:int) =
        // if user<>null then Some user else None
        let user,items = Tfs.GetTfsChangesByUserAndFile tfs (nullToNone user) querypath (defaultToNone resultLimit)
        noneToNull user,items

    member x.GetChangesWithoutWorkItems (user:string) querypath (resultLimit:int) = 
        Tfs.GetTfsChangesWithoutWorkItems tfs (nullToNone user) querypath (defaultToNone resultLimit)
    member x.GetItemsByWildcard(wildcard) = Tfs.GetVcsItemsByWildcard (tfs|> Tfs.GetVcs) wildcard

    interface IDisposable with
        member x.Dispose() = tfs.Dispose()

module Build = 

  open Microsoft.TeamFoundation.Build.Client

  let GetBuildServer (tfs:TfsTeamProjectCollection) = tfs.GetService<IBuildServer>()
  let GetBuildDefinitions (buildServer:IBuildServer) (teamProjectName:string) =
    buildServer.QueryBuildDefinitions(teamProjectName)

  let GetBuildDefinition (buildServer:IBuildServer) (teamProjectName:string) buildName =
    buildServer.GetBuildDefinition(teamProjectName,buildName)
  // let asSerializable (build:IBuildDefinition) = easier in C#? or difficult to determine what code would produce consistent results

type TfsBuild(buildServer) =
    new(tfs) = TfsBuild(Build.GetBuildServer tfs)
    member x.GetBuildDefinitions(teamProjectName) = Build.GetBuildDefinitions buildServer teamProjectName
    member x.GetBuildDefinition(teamProjectName, buildName) = Build.GetBuildDefinition buildServer teamProjectName buildName

module tf_exe = 
    // for example: tf undo "$/Development" /WORKSPACE:FOOComputerName;domainname\user.name /recursive /server:https://tfs.foo.com
    let baseUndo serverItemPath workspaceName user tfsUri : string = sprintf "tf undo \"%s\" /WORKSPACE:%s;%s /recursive /server:%s" serverItemPath workspaceName user tfsUri
