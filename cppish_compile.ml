open Cppish_ast
open Cish_ast

exception Implement_Me

(* generate fresh labels *)
let label_counter = ref 0
let new_int() = (label_counter := (!label_counter) + 1; !label_counter)
let new_label() = "FUNC_" ^ (string_of_int (new_int()))
let new_variable() = "t_" ^ (string_of_int (new_int()))

let global_func_list = ref []

module StringSet = Set.Make(String)

(* Mapping from Class names to Fields list *)
let fieldTbl : (string, string list) Hashtbl.t = Hashtbl.create 50 

(* Mapping from Class names to Function list *)
let funcTbl : (string, string list) Hashtbl.t = Hashtbl.create 50 

let rec compile_main_body (pg :Cppish_ast.program) : Cish_ast.stmt = 
  match pg with 
  | hd :: tl ->
    (match hd with 
    | Cppish_ast.Clas {name; body; extend; pos} -> raise Implement_Me
    | Cppish_ast.Fn {name; args; body; pos}  -> raise Implement_Me
    )
  | [] -> (Cish_ast.skip, 0)

let rec compile_program_body (pg :Cppish_ast.program) : Cish_ast.program =
  let main_body = compile_main_body pg in
  let main_sig = Cish_ast.Fn {name = "main"; args = [""]; body = (Cish_ast.Let("result", ( Cish_ast.Int 0 ,0), (Seq(main_body, (Return((Var "result", 0)),0)),0) ),0); pos = 0} in
  global_func_list := [ main_sig ] @ !global_func_list;
  !global_func_list