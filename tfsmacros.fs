module tfsmacros
open System
open Microsoft.TeamFoundation.Client
open Microsoft.TeamFoundation.VersionControl.Client
// assumes that you have a tfs server listed in an environment variable called servers (semicolon delimited list of work servers)
// usage (assuming your serverItemPath is $/Development/): 
//  #r "tfsmacros";open tfsmacros;let tfs=getTfs();;
//  getTfsChangesByUserAndFile tfs None "$/Development/";;

type UserChangeHistory = {ChangesetId:int;CreationDate:DateTime;AssociatedWorkItems:(int*string*string)[];Changes:string[]}
type ChangeHistory = {ChangesetId:int;Owner:string;CreationDate:DateTime;AssociatedWorkItems:(int*string*string)[];Changes:string[]}
let tfsServer = 
  System.Environment.GetEnvironmentVariable("servers").Split([|";"|],StringSplitOptions.None)
  |> Seq.filter (fun s -> s.Contains("tfs"))
  |> Seq.head
let tfsUri = 
  sprintf "https://%s" tfsServer
let webPort = 8080
let teamProject = "Development"
let getTfs() = new TfsTeamProjectCollection(new Uri(tfsUri))
let getVcs (tfs:TfsTeamProjectCollection) = tfs.GetService<VersionControlServer>()
let getTfsChangesByPath (tfs:TfsTeamProjectCollection) queryPath (user:string option) includeDetails = 
  let vcs = getVcs tfs
  let userArg = match user with |None -> null |Some x -> x
  vcs.QueryHistory (queryPath, VersionSpec.Latest, 0, RecursionType.Full, userArg, null, null, Int32.MaxValue, includeDetails,false)
  |> Seq.cast<Changeset>
  |> Seq.filter (fun cs -> cs.Changes |> Seq.exists (fun change -> change.Item.ServerItem.Contains("/Playground/"))=false )

let getTfsChangesByUserAndFile (tfs:TfsTeamProjectCollection) (user:string option) querypath =
  let userArg = match user with |None -> Some Environment.UserName | f -> f
  getTfsChangesByPath tfs querypath userArg true
  |> Seq.map (fun cs -> {
                            UserChangeHistory.ChangesetId=cs.ChangesetId;
                            CreationDate=cs.CreationDate;
                            AssociatedWorkItems=(cs.AssociatedWorkItems |> Seq.map (fun wi -> wi.Id,wi.Title,wi.WorkItemType) |> Array.ofSeq);
                            Changes=(cs.Changes |> Seq.map( fun change-> change.Item.ServerItem) |> Array.ofSeq)}
  )
  |> fun i -> userArg,i

let getTfsChangesWithoutWorkItems (tfs:TfsTeamProjectCollection) (user:string option) querypath = 
  getTfsChangesByPath tfs querypath user true
  |> Seq.filter (fun cs->cs.AssociatedWorkItems.Length = 0)
  |> Seq.map (fun cs -> {ChangesetId=cs.ChangesetId;
                      Owner=cs.Owner;
                      CreationDate=cs.CreationDate;
                      AssociatedWorkItems=(cs.AssociatedWorkItems |> Seq.map (fun wi -> wi.Id,wi.Title,wi.WorkItemType) |> Array.ofSeq);
                      Changes=(cs.Changes |> Seq.map( fun change-> change.Item.ServerItem) |> Array.ofSeq)})