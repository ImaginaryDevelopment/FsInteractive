module tfsmacros
open System
open Microsoft.TeamFoundation.Client
open Microsoft.TeamFoundation.VersionControl.Client
// assumes that you have a tfs server listed in an environment variable called servers (semicolon delimited list of work servers)
// usage (assuming your serverItemPath is $/Development/): 
//  #r "tfsmacros";open tfsmacros;let tfs=getTfs();;
//  getTfsChangesByUserAndFile tfs None "$/Development/";;

type ChangeHistory = {ChangesetId:int;CreationDate:string;AssociatedWorkItems:(int*string*string)[];Changes:string[]}

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
let getTfsChangesByUserAndFile (tfs:TfsTeamProjectCollection) (user:string option) querypath = 
  let vcs = getVcs tfs
  vcs.QueryHistory (querypath, VersionSpec.Latest, 0, RecursionType.Full, (match user with | None -> Environment.UserName | Some x -> x), null, null, Int32.MaxValue, true, false)
  |> Seq.cast<Changeset>
  |> Seq.filter (fun cs -> cs.Changes |> Seq.exists (fun change -> change.Item.ServerItem.Contains("/Playground/"))=false )
  |> Seq.map (fun cs -> {ChangesetId=cs.ChangesetId;
                      CreationDate=(cs.CreationDate.ToString());
                      AssociatedWorkItems=(cs.AssociatedWorkItems |> Seq.map (fun wi -> wi.Id,wi.Title,wi.WorkItemType) |> Array.ofSeq);
                      Changes=(cs.Changes |> Seq.map( fun change-> change.Item.ServerItem) |> Array.ofSeq)}

  )