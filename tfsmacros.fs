module tfsmacros
open System
open Microsoft.TeamFoundation.Client
open Microsoft.TeamFoundation.VersionControl.Client
// assumes that you have a tfs server listed in an environment variable called servers (semicolon delimited list of work servers)
// usage (assuming your serverItemPath is $/Development/): 
//  #r "tfsmacros";open tfsmacros;let tfs=getTfs();;
//  getTfsChangesByUserAndFile tfs None "$/Development/";;


type ChangeHistory = {ChangesetId:int;Owner:string;CreationDate:DateTime;AssociatedWorkItems:(int*string*string)[];Changes:string[]}

let tfsServer = 
  System.Environment.GetEnvironmentVariable("servers").Split([|";"|],StringSplitOptions.None)
  |> Seq.filter (fun s -> s.Contains("tfs"))
  |> Seq.head


type UserChangeHistory = {ChangesetId:int;CreationDate:DateTime;AssociatedWorkItems:(int*string*string)[];Changes:string[]}

let tfsUri = 
  sprintf "https://%s" tfsServer
let webPort = 8080
let teamProject = "Development"

let private webLinkBase  = sprintf "http://%s:%i/DefaultCollection/%s" tfsServer webPort teamProject
let getChangesetLink changeset = sprintf "%s/_versionControl/changeset/%i" webLinkBase changeset
let getItemLink changeset path = sprintf "%s#path=%s&_a=compare" <| getChangesetLink changeset |> path
let getWorkItemLink = sprintf "%s/_workitems/edit/%i" webLinkBase

let getTfs() = new TfsTeamProjectCollection(new Uri(tfsUri))
let getVcs (tfs:TfsTeamProjectCollection) = tfs.GetService<VersionControlServer>()
let getTfsChangesByPath (tfs:TfsTeamProjectCollection) queryPath (user:string option) queryLimit includeDetails = 
  let vcs = getVcs tfs
  let limitArg = match queryLimit with |None -> Int32.MaxValue | Some x -> x
  let userArg = match user with |None -> null |Some x -> x
  vcs.QueryHistory (queryPath, VersionSpec.Latest, 0, RecursionType.Full, userArg, null, null, limitArg, includeDetails,false)
  |> Seq.cast<Changeset>
  |> Seq.filter (fun cs -> cs.Changes |> Seq.exists (fun change -> change.Item.ServerItem.Contains("/Playground/"))=false )

let getTfsChangesByUserAndFile (tfs:TfsTeamProjectCollection) (user:string option) querypath (resultLimit:int option) =
  let userArg = match user with |None -> Some Environment.UserName | f -> f
  getTfsChangesByPath tfs querypath userArg None true
  |> fun items-> if resultLimit.IsSome then Seq.take(resultLimit.Value) items else items
  |> Seq.map (fun cs -> {
                            UserChangeHistory.ChangesetId=cs.ChangesetId;
                            CreationDate=cs.CreationDate;
                            AssociatedWorkItems=(cs.AssociatedWorkItems |> Seq.map (fun wi -> wi.Id,wi.Title,wi.WorkItemType) |> Array.ofSeq);
                            Changes=(cs.Changes |> Seq.map( fun change-> change.Item.ServerItem) |> Array.ofSeq)}
  )
  |> fun i -> userArg,i

let getTfsChangesWithoutWorkItems (tfs:TfsTeamProjectCollection) (user:string option) querypath (resultLimit:int option) = 
  getTfsChangesByPath tfs querypath user None true
  |> Seq.filter (fun cs->cs.AssociatedWorkItems.Length = 0)
  |> fun items-> if resultLimit.IsSome then Seq.take(resultLimit.Value) items else items
  |> Seq.map (fun cs -> {ChangesetId=cs.ChangesetId;
                      Owner=cs.Owner;
                      CreationDate=cs.CreationDate;
                      AssociatedWorkItems=(cs.AssociatedWorkItems |> Seq.map (fun wi -> wi.Id,wi.Title,wi.WorkItemType) |> Array.ofSeq);
                      Changes=(cs.Changes |> Seq.map( fun change-> change.Item.ServerItem) |> Array.ofSeq)})