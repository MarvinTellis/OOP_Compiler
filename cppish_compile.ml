open Cppish_ast
open Cish_ast

exception Implement_Me

(* generate fresh labels *)
let label_counter = ref 0
let new_int() = (label_counter := (!label_counter) + 1; !label_counter)
let new_label() = "FUNC_" ^ (string_of_int (new_int()))
let new_variable() = "t_" ^ (string_of_int (new_int()))

let object_lst = ref []

let global_func_list = ref []

module StringSet = Set.Make(String)

(* Mapping from Object names to Class names *)
let objClassTbl : (string, string) Hashtbl.t = Hashtbl.create 50 

(* Mapping from Class names to Fields list *)
let fieldTbl : (string, string list) Hashtbl.t = Hashtbl.create 50 

(* Mapping from Class names to Function list *)
let funcTbl : (string, (string * string) list) Hashtbl.t = Hashtbl.create 50 

let update_fieldTable (key : string) (value : string) : unit =
  let og_list = try (Hashtbl.find fieldTbl key) with Not_found -> [] in
  let updated_list = og_list @ [value] in
  Hashtbl.replace fieldTbl key updated_list

let update_fieldTbl_with_lst (key : string) (value : string list) : unit =
  let og_list = try (Hashtbl.find fieldTbl key) with Not_found -> [] in
  let updated_list = og_list @ value in
  Hashtbl.replace fieldTbl key updated_list

let update_funcTbl_with_lst (key : string) (value : (string * string) list) : unit =
  let og_list = try (Hashtbl.find funcTbl key) with Not_found -> [] in
  let updated_list = og_list @ value in
  Hashtbl.replace funcTbl key updated_list
let update_funcTable (key1 : string) (key2 : string) (value : string): unit =
  let og_list = try (Hashtbl.find funcTbl key1) with Not_found -> [] in
  let updated_list = og_list @ [(key2, value)] in
  Hashtbl.replace funcTbl key1 updated_list

let rec get_index_of_field v lst =
  match lst with
  | [] -> 
      raise Not_found
  | hd :: tl -> 
      if v = hd then 1 else 1 + get_index_of_field v tl

let rec get_index_of_func v lst =
  match lst with
  | [] -> 
      raise Not_found
  | (v1, v2) :: tl -> 
      if v = v1 then 0 else 1 + get_index_of_func v tl


(* Function to generate a single assignment statement *)
let generate_assign_stmt base_offset temp_name =
  let offset = base_offset * 4 in
  let exp = Cish_ast.Binop(temp_name, Plus, (Cish_ast.Int(offset), 0)), 0 in
  let store_stmt = Cish_ast.Exp(Cish_ast.Store(exp, (Cish_ast.Int(0), 0)), 0) in
  (store_stmt, 0)

(* Function to generate a sequence of assignment statements *)
let rec generate_stmts base_offset temp_name (lst : string list) : (Cish_ast.stmt) = 
  match lst with 
  | [] -> (Exp (Int 0, 0),0)  (* Base case: return a skip statement *)
  | var :: rest ->
      let assign_stmt = generate_assign_stmt base_offset temp_name in
      let next_offset = base_offset + 1 in
      let rest_stmts = generate_stmts next_offset temp_name rest in
      (Seq (assign_stmt, rest_stmts), 0)

let gen_assign_func_stmt base_offset temp_name func_name=
  let offset = base_offset * 4 in
  let exp = Cish_ast.Binop (temp_name, Plus, (Cish_ast.Int(offset), 0)), 0 in
  let store_stmt = Cish_ast.Exp (Cish_ast.Store(exp, (Cish_ast.Var(func_name), 0)), 0) in
  (store_stmt, 0)
      
let rec generate_stmts_func base_offset temp_name (lst : (string*string) list) : (Cish_ast.stmt) = 
  match lst with 
  | [] -> (Exp (Int 0, 0),0)  (* Base case: return a skip statement *)
  | (var1, var2) :: rest ->
      let assign_stmt = gen_assign_func_stmt base_offset temp_name var2 in
      let next_offset = base_offset + 1 in
      let rest_stmts = generate_stmts_func next_offset temp_name rest in
      (Seq (assign_stmt, rest_stmts), 0)

let rec compile_expression (class_name : string) (expr : Cppish_ast.exp) : (Cish_ast.exp) = 
  match expr with
  | (Cppish_ast.Int i, _) -> (Cish_ast.Int i, 0)
  | (Cppish_ast.Var v, _) -> 
    if List.mem v (Hashtbl.find fieldTbl class_name) then 
      let depth = get_index_of_field v (Hashtbl.find fieldTbl class_name) in
      (Cish_ast.Load( (Cish_ast.Binop((Cish_ast.Var("this"),0) , Cish_ast.Plus, (Cish_ast.Int(depth*4), 0) ), 0) ), 0)
    else 
      (Cish_ast.Var v, 0)
  | (Cppish_ast.Binop (e1, op, e2), _) -> 
    let new_e1 = compile_expression class_name e1 in
    let new_e2 = compile_expression class_name e2 in
    (match op with 
    | Cppish_ast.Plus -> (Cish_ast.Binop(new_e1, Cish_ast.Plus, new_e2), 0)
    | Cppish_ast.Minus -> (Cish_ast.Binop(new_e1, Cish_ast.Minus, new_e2), 0)
    | Cppish_ast.Times -> (Cish_ast.Binop(new_e1, Cish_ast.Times, new_e2), 0)
    | Cppish_ast.Div -> (Cish_ast.Binop(new_e1, Cish_ast.Div, new_e2), 0)
    | Cppish_ast.Eq -> (Cish_ast.Binop(new_e1, Cish_ast.Eq, new_e2), 0)
    | Cppish_ast.Neq -> (Cish_ast.Binop(new_e1, Cish_ast.Neq, new_e2), 0)
    | Cppish_ast.Lt -> (Cish_ast.Binop(new_e1, Cish_ast.Lt, new_e2), 0)
    | Cppish_ast.Lte -> (Cish_ast.Binop(new_e1, Cish_ast.Lte, new_e2), 0)
    | Cppish_ast.Gt -> (Cish_ast.Binop(new_e1, Cish_ast.Gt, new_e2), 0)
    | Cppish_ast.Gte -> (Cish_ast.Binop(new_e1, Cish_ast.Gte, new_e2), 0)
    )
  | (Cppish_ast.Not(e), _) -> 
    let new_e = compile_expression class_name e in
    (Cish_ast.Not(new_e), 0)
  | (Cppish_ast.And(e1, e2), _) ->
    let new_e1 = compile_expression class_name e1 in
    let new_e2 = compile_expression class_name e2 in
    (Cish_ast.And(new_e1, new_e2), 0)
  | (Cppish_ast.Or(e1, e2), _) ->
    let new_e1 = compile_expression class_name e1 in
    let new_e2 = compile_expression class_name e2 in
    (Cish_ast.Or(new_e1, new_e2), 0)
  | (Cppish_ast.Assign(v, e), _) -> 
    let new_e = compile_expression class_name e in
    (Cish_ast.Assign(v, new_e), 0)
  | (Cppish_ast.Field(obj, v, e), _) ->
    let new_e = compile_expression class_name e in
    let depth = get_index_of_field v (Hashtbl.find fieldTbl (Hashtbl.find objClassTbl obj)) in
    (Cish_ast.Store( (Cish_ast.Binop((Cish_ast.Var(obj),0) , Cish_ast.Plus, (Cish_ast.Int(depth*4), 0) ), 0) , new_e), 0) 
  | (Cppish_ast.Call(e, lst), _) ->
    let new_e = compile_expression class_name e in
    let new_lst = List.map (compile_expression class_name) lst in
    (Cish_ast.Call(new_e, new_lst), 0)
  | (Cppish_ast.Load(e), _) ->
    let new_e = compile_expression class_name e in
    (Cish_ast.Load(new_e), 0)
  | (Cppish_ast.Store(e1, e2), _) ->
    let new_e1 = compile_expression class_name e1 in
    let new_e2 = compile_expression class_name e2 in
    (Cish_ast.Store(new_e1, new_e2), 0)
  | (Cppish_ast.Malloc(e), _) ->
    let new_e = compile_expression class_name e in
    (Cish_ast.Malloc(new_e), 0)

let rec compile_cpp_stmt (class_name : string) (cpp_stmt : Cppish_ast.stmt) : Cish_ast.stmt =
  match cpp_stmt with 
  | (Cppish_ast.Exp(e), _) -> (Cish_ast.Exp(compile_expression class_name e), 0)
  | (Cppish_ast.Let(v, e, s), _) -> (Cish_ast.Let(v, compile_expression class_name e, compile_cpp_stmt class_name s), 0)
  | (Cppish_ast.Seq(s1, s2), _) -> (Cish_ast.Seq(compile_cpp_stmt class_name s1, compile_cpp_stmt class_name s2), 0)
  | (Cppish_ast.If(e, s1, s2), _) -> (Cish_ast.If(compile_expression class_name e, compile_cpp_stmt class_name s1, compile_cpp_stmt class_name s2), 0)
  | (Cppish_ast.While(e, s), _) -> (Cish_ast.While(compile_expression class_name e, compile_cpp_stmt class_name s), 0)
  | (Cppish_ast.For(init, cond, incr, s), _) -> (Cish_ast.For(compile_expression class_name init, compile_expression class_name cond,compile_expression class_name incr,compile_cpp_stmt class_name s), 0)
  | (Cppish_ast.Return e, _) -> (Cish_ast.Return(compile_expression class_name e), 0)
  | (Cppish_ast.New (obj_name, class_name), _) -> 
    object_lst := !object_lst @ [obj_name];
    Hashtbl.add objClassTbl obj_name class_name;
    let field_lst = try (Hashtbl.find fieldTbl class_name) with Not_found -> [] in
    let field_size = List.length field_lst in
    let t1 = new_variable() in
    let func_lst = try (Hashtbl.find funcTbl class_name) with Not_found -> [] in
    let func_size = List.length func_lst in
    let obj_alloc = (Cish_ast.Assign(t1, (Cish_ast.Malloc(Cish_ast.Int(field_size*4 + 4), 0), 0)), 0) in
    let field_init_stmt = generate_stmts 1 (Cish_ast.Var(t1), 0) field_lst  in
    let block_1 = Cish_ast.Seq( (Cish_ast.Exp(obj_alloc), 0) , field_init_stmt) in
    let v_table = Cish_ast.Load(Cish_ast.Var(t1), 0) in
    let v_table_init = 
      if func_size > 0 then (Cish_ast.Exp(Cish_ast.Store((Cish_ast.Var(t1),0), (Cish_ast.Malloc(Cish_ast.Int(func_size*4), 0), 0)), 0), 0)
      else (Cish_ast.skip, 0)
    in
    let func_init_stmt = generate_stmts_func 0 (v_table, 0) func_lst in
    let block_2 = Cish_ast.Seq( v_table_init , func_init_stmt) in
    let final_block = Cish_ast.Seq((block_1, 0), (block_2, 0)) in
    let stm1 = (Cish_ast.Exp(Cish_ast.Assign(obj_name, (Cish_ast.Var(t1), 0)), 0), 0) in
    (Cish_ast.Let(t1,(Cish_ast.Int(0), 0), (Cish_ast.Seq((final_block, 0), stm1), 0) ), 0)
  | (Cppish_ast.Method(obj, fname, lst), _) ->
    let new_lst = List.map (compile_expression class_name) lst in
    let t1 = new_variable() in
    let t2 = new_variable() in
    let v_table = (Cish_ast.Exp(Cish_ast.Assign(t1, (Cish_ast.Load(Cish_ast.Var(obj), 0), 0)), 0), 0) in
    let func_index = get_index_of_func fname (Hashtbl.find funcTbl (Hashtbl.find objClassTbl obj)) in
    let func_ptr = (Cish_ast.Load(Cish_ast.Binop((Cish_ast.Var(t1), 0), Plus, (Cish_ast.Int(func_index*4), 0)), 0), 0) in
    let func_to_call = (Cish_ast.Exp((Cish_ast.Assign(t2, func_ptr), 0)), 0) in
    let block = (Cish_ast.Seq(v_table, func_to_call), 0) in
    let func_call = (Cish_ast.Exp(Cish_ast.Call( (Cish_ast.Var(t2), 0), (Cish_ast.Var(obj), 0) :: new_lst), 0), 0) in
    let bind1 = (Cish_ast.Let(t2, (Cish_ast.Int(0),0), (Cish_ast.Seq(block, func_call), 0)), 0) in
    (Cish_ast.Let(t1, (Cish_ast.Int(0),0), bind1), 0) 

let rec compile_class_stmt (class_name : string) (class_stmt : Cppish_ast.classtmt) : unit = 
  (match class_stmt with 
    | (Let (v, exp) , _) -> 
      update_fieldTable class_name v
    | (Fn {name; args; body; _} , _) -> 
      let new_func_name = new_label() in
      let _ = update_funcTable class_name name new_func_name in
      (* TODO : Compile class method body to replace class fields with offsets*)
      let new_body = compile_cpp_stmt class_name body in
      let class_Fun_sig = Cish_ast.Fn {name = new_label(); args = "this" :: args; body = new_body; pos = 0} in
      global_func_list := !global_func_list @ [class_Fun_sig];
    | (Seq (cstmt1, cstmt2) , _) -> 
      compile_class_stmt class_name cstmt1;
      compile_class_stmt class_name cstmt2;
  )

let rec compile_main_body (pg :Cppish_ast.program) : unit = 
  match pg with 
  | hd :: tl ->
    (match hd with 
    | Cppish_ast.Clas {name; body; extend; pos} -> 
      let _ = Hashtbl.add fieldTbl name [] in
      (* Parent class handling *)
      let parent_field_lst =try (Hashtbl.find fieldTbl extend) with Not_found -> [] in
      let _ = update_fieldTbl_with_lst name parent_field_lst in
      let parent_func_lst = try (Hashtbl.find funcTbl extend) with Not_found -> [] in
      let _ = update_funcTbl_with_lst name parent_func_lst in
      (* Parent class function handling *)
      compile_class_stmt name body
    | Cppish_ast.Fn {name; args; body; pos}  -> 
      let new_body = compile_cpp_stmt "" body in
      let new_name = if name = "main" then "main" else new_label() in
      let c_Fun_sig = Cish_ast.Fn {name = new_name; args = [""]; body = new_body; pos = 0} in
      global_func_list := !global_func_list @ [c_Fun_sig];
    )
  | [] -> ()

let rec compile_program_body (pg :Cppish_ast.program) : Cish_ast.program =
  let _ = compile_main_body pg in
  !global_func_list