open Printf

module G = struct
  module G = Graph.Imperative.Digraph.Abstract(struct type t = string end)
  module Display = struct
    include G
    let vertex_name = V.label
    let graph_attributes _ = []
    let default_vertex_attributes _ = []
    let vertex_attributes _ = []
    let default_edge_attributes _ = []
    let edge_attributes _ = []
    let get_subgraph _ = None
  end
  module Dot = Graph.Graphviz.Dot(Display)
  include G
end

let fold_process_lines cmd ~f ~init =
  let in_channel = Unix.open_process_in cmd in
  let rec loop acc =
    try loop (f acc (input_line in_channel))
    with End_of_file ->
      (ignore (Unix.close_process_in in_channel); acc)
  in loop init

let find_cmis dir =
  let rec loop cmis = function
    | [] -> cmis
    | dir::rest ->
      let contents =
        dir
        |> Sys.readdir
        |> Array.to_list
        |> List.rev_map (Filename.concat dir) in
      let (dirs, files) =
        List.fold_left (fun (dirs, files) f ->
          match (Unix.stat f).Unix.st_kind with
          | Unix.S_REG -> (dirs, f::files)
          | Unix.S_DIR -> (f::dirs, files)
          | _ ->
            eprintf "Skipping '%s'\n" f;
            (dirs, files)
        ) ([], []) contents in
      let fs = List.filter (fun f -> Filename.check_suffix f ".cmi") files in
      loop (fs @ cmis) (dirs @ rest) in
  loop [] [dir]

type module_ =
  { name: string
  ; digest: string }

type cmi =
  { module_name: string
  ; refs: module_ list }

let of_group s =
  { digest=Str.matched_group 1 s
  ; name=Str.matched_group 2 s }

let re =
  let mod_name = {|[A-Z][a-z0-9_]*|} in
  let digest = {|[0-9a-f]+|} in
  let space = "\t" in
  let group = sprintf {|\(%s\)|} in
  sprintf {|%s%s%s%s|} space (group digest) space (group mod_name)
  |> Str.regexp

let read_module_line l =
  if Str.string_match re l 0
  then Some (of_group l)
  else None

let read_cmi path =
  let refs =
    fold_process_lines (sprintf "ocamlobjinfo %s" path) ~init:[]
      ~f:(fun acc line ->
        match read_module_line line with
        | Some m -> m :: acc
        | None -> acc) in
  { module_name =
      path
      |> Filename.basename
      |> Filename.chop_extension
      |> String.capitalize
  ; refs }

let add_cmi graph is_available cmi =
  let v = G.V.create cmi.module_name in
  G.add_vertex graph v;
  cmi.refs
  |> List.filter (fun { name ; _ } -> is_available name)
  |> List.iter (fun { name ; _ } ->
    let v' = G.V.create name in
    G.add_vertex graph v';
    G.add_edge graph v v'
  )

let read_files_stdin () =
  let rec loop lines =
    try loop ((input_line stdin)::lines)
    with End_of_file -> lines in
  match loop [] with
  | [] -> failwith "no input files"
  | ls -> ls

let run_files cmis =
  let g = G.create () in
  let (register, is_available) =
    let h = Hashtbl.create 64 in
    (fun { module_name ; _ } -> Hashtbl.replace h module_name ()),
    (Hashtbl.mem h) in
  let cmis = List.map read_cmi cmis in
  cmis |> List.iter register;
  cmis |> List.iter (add_cmi g is_available);
  G.Dot.output_graph stdout g

let run dir =
  let files =
    match dir with
    | None -> read_files_stdin ()
    | Some d -> find_cmis d in
  run_files files

open Cmdliner

let dir =
  let doc = "recursively find cmi's in this directory" in
  Arg.(value & pos 0 (some dir) None & info [] ~doc)

let term = Term.(pure run $ dir)
let info = Term.info "cmigraph"

let () =
  match Term.eval (term, info) with
  | `Error _ -> exit 1
  | _ -> exit 0
