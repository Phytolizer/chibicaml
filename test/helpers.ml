open Unix
open BatBuffer

exception CommandFailed of string array
exception CommandCrashed of string array

let check_exit args status =
  match status with
  | WEXITED code -> if code != 0 then raise (CommandFailed args)
  | _ -> raise (CommandCrashed args)

let check_process name args =
  create_process name args stdin stdout stderr
  |> waitpid [] |> snd |> check_exit args

let () =
  let contents =
    {|
      int ret3() { return 3; }
      int ret5() { return 5; }
      int add(int x, int y) { return x+y; }
      int sub(int x, int y) { return x-y; }

      int add6(int a, int b, int c, int d, int e, int f) {
        return a+b+c+d+e+f;
      }
    |}
  in
  let stdin =
    contents |> BatIO.input_string |> BatIO.to_input_channel
    |> descr_of_in_channel
  in
  let args = [| "gcc"; "-xc"; "-c"; "-"; "-o"; "tmp2.o" |] in
  create_process "gcc" args stdin stdout stderr
  |> waitpid [] |> snd |> check_exit args

let run input =
  let strbuf = BatBuffer.create 80 |> output_buffer in
  Chibic.Compiler.compile input strbuf;
  let asm = BatIO.close_out strbuf in
  Out_channel.with_open_text "tmp.asm" (fun out -> output_string out asm);
  try
    check_process "nasm" [| "nasm"; "-felf64"; "tmp.asm"; "-o"; "tmp.o" |];
    check_process "gcc"
      [|
        "gcc"; "-static"; "-z"; "noexecstack"; "tmp.o"; "tmp2.o"; "-o"; "tmp";
      |];
    let result =
      create_process "./tmp" [||] stdin stdout stderr |> waitpid [] |> snd
    in
    match result with
    | WEXITED code -> print_int code
    | _ -> raise (CommandCrashed [| "./tmp" |])
  with (CommandFailed args | CommandCrashed args) as x ->
    Printf.eprintf "command '%s' failed to execute\n"
      (String.concat " " (Array.to_list args));
    prerr_endline "generated asm:\n";
    prerr_endline asm;
    raise x
