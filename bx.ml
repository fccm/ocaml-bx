#use "topfind"
#require "xml-light"

module SSet = Set.Make(String)
module SMap = Map.Make(String)
open Xml

class out oc =
  object
    method o_int n d =
      match n with
      | 1 -> output_byte oc d
      | 4 -> output_binary_int oc d
      | _ -> assert false
    method o_str s =
      output_string oc s
  end

class read ic =
  object
    val len = (in_channel_length ic)
    method i_int n =
      match n with
      | 1 -> input_byte ic
      | 4 -> input_binary_int ic
      | _ -> assert false
    method i_str n =
      let s = String.create n in
      really_input ic s 0 n;
      (s)
    method eof =
      pos_in ic = len
  end

let get_strings_of_attrs acc attrs =
  List.fold_left (fun acc (att_name, att_val) ->
    SSet.add att_name (SSet.add att_val acc)
  ) acc attrs

let get_strings xml =
  let rec aux acc = function
  | Xml.Element (tag, attrs, children) :: next ->
      let acc = SSet.add tag acc in
      let acc = get_strings_of_attrs acc attrs in
      let acc = aux acc children in
      aux acc next
  | Xml.PCData d :: next ->
      let acc = SSet.add d acc in
      aux acc next
  | [] -> acc
  in
  aux SSet.empty [xml]

let print_strings o ss =
  o#o_int 4 (SSet.cardinal ss);
  SSet.fold (fun s () ->
    o#o_int 4 (String.length s);
    o#o_str s
  ) ss ()

let read_strings i =
  let n = i#i_int 4 in
  Array.init n (fun _ ->
    let len = i#i_int 4 in
    i#i_str len
  )

let opening_tag = 0x01
let closing_tag = 0x02
let pcdata = 0x03

let print_attrs o attrs str =
  o#o_int 1 (List.length attrs);
  List.iter (fun (att_name, att_val) ->
    let att_name_index = SMap.find att_name str in
    let att_val_index = SMap.find att_val str in
    o#o_int 1 att_name_index;
    o#o_int 1 att_val_index;
  ) attrs

let read_attrs i sa =
  let n = i#i_int 1 in
  for j = 1 to n do
    let att_name_index = i#i_int 1 in
    let att_val_index = i#i_int 1 in
    Printf.printf " %s='%s'"
      sa.(att_name_index)
      sa.(att_val_index)
  done

let read_opening_tag i sa path =
  let tag_index = i#i_int 1 in
  let tag = sa.(tag_index) in
  Printf.printf "<%s" tag;
  read_attrs i sa;
  print_string ">\n";
  (tag :: path)

let read_closing_tag i = function
  | tag :: path ->
      Printf.printf "</%s>\n" tag;
      (path)
  | _ -> assert false

let read_pcdata i sa path =
  let dat_index = i#i_int 1 in
  Printf.printf "%s\n" sa.(dat_index);
  (path)

let rec read_bxml i sa path =
  let path =
    let tag = i#i_int 1 in
    if tag = opening_tag then read_opening_tag i sa path else
    if tag = closing_tag then read_closing_tag i path else
    if tag = pcdata then read_pcdata i sa path else
      assert false
  in
  if i#eof then assert(path = [])
  else read_bxml i sa path

let print_xml o xml str =
  let rec aux = function
  | Xml.Element (tag, attrs, children) :: next ->
      let tag_index = SMap.find tag str in
      o#o_int 1 opening_tag;
      o#o_int 1 tag_index;
      print_attrs o attrs str;
      aux children;
      o#o_int 1 closing_tag;
      aux next
  | Xml.PCData d :: next ->
      let dat_index = SMap.find d str in
      o#o_int 1 pcdata;
      o#o_int 1 dat_index;
      aux next
  | [] -> ()
  in
  aux [xml]

let write in_file out_file =
  let xml = Xml.parse_file in_file in
  let ss = get_strings xml in
  let str, _ =
    SSet.fold (fun s (str, i) ->
      (SMap.add s i str, succ i)
    ) ss (SMap.empty, 0)
  in
  let oc = open_out out_file in
  let o = new out oc in
  print_strings o ss;
  print_xml o xml str;
  close_out oc;
;;

let read in_file =
  let ic = open_in in_file in
  let i = new read ic in
  let sa = read_strings i in
  read_bxml i sa [];
;;

let usage () =
  let c = Sys.argv.(0) in
  Printf.printf "Usage:\n\
    %s -in <xml-in-file> -out <binary-out-file>\n\
    %s -recover <binary-in-file>\n%!" c c;
  exit 1

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  match args with
  | ["-in"; in_file; "-out"; out_file] -> write in_file out_file
  | ["-recover"; in_file] -> read in_file
  | _ -> usage ()
