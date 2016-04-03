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

module StringSet = Set.Make(String)

let fold_process_lines cmd ~f ~init =
  let in_channel = Unix.open_process_in cmd in
  let rec loop acc =
    try loop (f acc (input_line in_channel))
    with End_of_file ->
      (ignore (Unix.close_process_in in_channel); acc)
  in loop init

let find_matching dir ~f =
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
      let fs = List.filter f files in
      loop (fs @ cmis) (dirs @ rest) in
  loop [] [dir]

let find_cmis = find_matching ~f:(fun f -> Filename.check_suffix f ".cmi")

let find_sources = find_matching ~f:(fun f ->
  Filename.check_suffix f ".mli" || Filename.check_suffix f ".ml")

type module_ =
  { name: string
  ; deps: StringSet.t }

let re =
  let mod_name = {|[A-Z][a-z0-9_]*|} in
  let digest = {|[0-9a-f]+|} in
  let space = "\t" in
  let group = sprintf {|\(%s\)|} in
  sprintf {|%s%s%s%s|} space digest space (group mod_name)
  |> Str.regexp

let read_module_line l =
  if Str.string_match re l 0
  then Some (Str.matched_group 1 l)
  else None

let name_of_path path =
  path
  |> Filename.basename
  |> Filename.chop_extension
  |> String.capitalize

let read_cmi path =
  let name = name_of_path path in
  let deps =
    fold_process_lines (sprintf "ocamlobjinfo %s" path) ~init:StringSet.empty
      ~f:(fun acc line ->
        match read_module_line line with
        | Some m when m <> name -> StringSet.add m acc
        | Some _
        | None -> acc) in
  { name ; deps }

let read_ocamldep_line =
  let colon = Str.regexp ": ?" in
  let space = Str.regexp "[ \t]+" in
  fun line ->
    match Str.split colon line with
    | [path ; deps] ->
      let res =
        { name=name_of_path path
        ; deps=StringSet.of_list (Str.split space deps) } in
      res
    | [path] -> { name=name_of_path path ; deps=StringSet.empty }
    | _ -> failwith (sprintf "Inavlid ocamldep line: '%s'" line)

let ocamldep files =
  let cmd = "ocamldep -modules " ^ (String.concat " " files) in
  let deps = Hashtbl.create 64 in
  fold_process_lines cmd ~init:() ~f:(fun () l ->
    let d = read_ocamldep_line l in
    begin match Hashtbl.find deps d.name with
    | exception Not_found -> Hashtbl.add deps d.name d
    | d' ->
      Hashtbl.replace deps d.name
        { d with deps=StringSet.union d'.deps d.deps }
    end;
  );
  let elems = ref [] in
  deps |> Hashtbl.iter (fun _ v -> elems := v :: !elems);
  !elems

let add_module graph is_available mod_ =
  let v = G.V.create mod_.name in
  G.add_vertex graph v;
  mod_.deps
  |> StringSet.filter is_available
  |> StringSet.iter (fun name ->
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
    (fun { name ; _ } -> Hashtbl.replace h name ()),
    (Hashtbl.mem h) in
  let cmis = List.map read_cmi cmis in
  cmis |> List.iter register;
  cmis |> List.iter (add_module g is_available);
  G.Dot.output_graph stdout g

let run_ocamldep sources =
  let g = G.create () in
  let (register, is_available) =
    let h = Hashtbl.create 64 in
    (fun { name ; _ } -> Hashtbl.replace h name ()),
    (Hashtbl.mem h) in
  let deps = ocamldep sources in
  deps |> List.iter register;
  deps |> List.iter (add_module g is_available);
  G.Dot.output_graph stdout g

let run dir =
  let files =
    match dir with
    | None -> read_files_stdin ()
    | Some d -> find_sources d in
  run_ocamldep files

open Cmdliner

let dir =
  let doc = "recursively find {cmis,ml + mli}'s in this directory" in
  Arg.(value & pos 0 (some dir) None & info [] ~doc)

let term = Term.(pure run $ dir)
let info = Term.info "cmigraph"

let () =
  match Term.eval (term, info) with
  | `Error _ -> exit 1
  | _ -> exit 0
