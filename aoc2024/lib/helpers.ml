let read_lines_from_file filename =
  let channel = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line channel in
      read_lines (line :: acc)
    with End_of_file -> List.rev acc
  in
  let lines = read_lines [] in
  close_in channel;
  lines

let print_int int = Format.printf "%a \n" Format.pp_print_int int

let print_listof_ints ints =
  Format.printf "%a \n"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
       Format.pp_print_int)
    ints

let print_listof_strs ints =
  Format.printf "%a \n"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
       Format.pp_print_string)
    ints

let print_listof_chars chars =
  Format.printf "%a \n"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
       Format.pp_print_char)
    chars
