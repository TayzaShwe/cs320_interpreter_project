(* parsing util functions *)

let is_lower_case c = 'a' <= c && c <= 'z'

let is_upper_case c = 'A' <= c && c <= 'Z'

let is_alpha c = is_lower_case c || is_upper_case c

let is_digit c = '0' <= c && c <= '9'

let is_alphanum c = is_lower_case c || is_upper_case c || is_digit c

let is_blank c = String.contains " \012\n\r\t" c

let explode s = List.of_seq (String.to_seq s)

let implode ls = String.of_seq (List.to_seq ls)

let readlines (file : string) : string =
  let fp = open_in file in
  let rec loop () =
    match input_line fp with
    | s -> s ^ "\n" ^ loop ()
    | exception End_of_file -> ""
  in
  let res = loop () in
  let () = close_in fp in
  res

(* end of util functions *)

(* parser combinators *)

type 'a parser = char list -> ('a * char list) option

let parse (p : 'a parser) (s : string) : ('a * char list) option = p (explode s)

let pure (x : 'a) : 'a parser = fun ls -> Some (x, ls)

let fail : 'a parser = fun ls -> None

let bind (p : 'a parser) (q : 'a -> 'b parser) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> q a ls
  | None -> None

let ( >>= ) = bind

let ( let* ) = bind

let read : char parser =
  fun ls ->
  match ls with
  | x :: ls -> Some (x, ls)
  | _ -> None

let satisfy (f : char -> bool) : char parser =
  fun ls ->
  match ls with
  | x :: ls ->
    if f x then
      Some (x, ls)
    else
      None
  | _ -> None

let char (c : char) : char parser = satisfy (fun x -> x = c)

let seq (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
  fun ls ->
  match p1 ls with
  | Some (_, ls) -> p2 ls
  | None -> None

let ( >> ) = seq

let seq' (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) -> (
      match p2 ls with
      | Some (_, ls) -> Some (x, ls)
      | None -> None)
  | None -> None

let ( << ) = seq'

let alt (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) -> Some (x, ls)
  | None -> p2 ls

let ( <|> ) = alt

let map (p : 'a parser) (f : 'a -> 'b) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> Some (f a, ls)
  | None -> None

let ( >|= ) = map

let ( >| ) p c = map p (fun _ -> c)

let rec many (p : 'a parser) : 'a list parser =
  fun ls ->
  match p ls with
  | Some (x, ls) -> (
      match many p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> Some ([], ls)

let rec many1 (p : 'a parser) : 'a list parser =
  fun ls ->
  match p ls with
  | Some (x, ls) -> (
      match many p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> None

let rec many' (p : unit -> 'a parser) : 'a list parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) -> (
      match many' p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> Some ([], ls)

let rec many1' (p : unit -> 'a parser) : 'a list parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) -> (
      match many' p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> None

let whitespace : unit parser =
  fun ls ->
  match ls with
  | c :: ls ->
    if String.contains " \012\n\r\t" c then
      Some ((), ls)
    else
      None
  | _ -> None

let ws : unit parser = many whitespace >| ()

let ws1 : unit parser = many1 whitespace >| ()

let digit : char parser = satisfy is_digit

let natural : int parser =
  fun ls ->
  match many1 digit ls with
  | Some (xs, ls) -> Some (int_of_string (implode xs), ls)
  | _ -> None

let literal (s : string) : unit parser =
  fun ls ->
  let cs = explode s in
  let rec loop cs ls =
    match (cs, ls) with
    | [], _ -> Some ((), ls)
    | c :: cs, x :: xs ->
      if x = c then
        loop cs xs
      else
        None
    | _ -> None
  in
  loop cs ls

let keyword (s : string) : unit parser = literal s >> ws >| ()

(* end of parser combinators *)

(* TODO *)
type const = 
  | Number of int
  | Boolean of bool
  | Null of unit
  | Name of string

type com = 
  | Push of const | Pop of const | Trace of const
  | Add of const | Sub of const | Mul of const | Div of const
  | And | Or | Not
  | Equal | Lte 
  | Local | Global | Lookup
  | Begin of com list | IfElse of (com list * com list)
  | Fun of (const * const * com list ) | Call | Try of com list | Switch of (const * com list) list


let natural'=
  fun ls ->
  match many1 digit ls with
  | Some (xs, ls) -> Some (Number (int_of_string (implode xs)), ls)
  | _ -> None



let int_parser : const parser = 
  natural' <|>
  (
    satisfy (fun x -> x='-') >>= fun _ ->
    natural >>= fun n ->
    pure (Number (-1*n))
  )

let bool_parser =
  ((literal "True") >>= fun _ ->
   pure (Boolean true)) 
  <|> 
  ((literal "False") >>= fun _ ->
   pure (Boolean false))



let empty_parser = 
  (literal "()") >>= fun _ ->
  pure (Null ())

let name_parser = 
  let* letter = satisfy (is_alpha) in
  let* rest = many (satisfy (is_alphanum) <|> satisfy (fun x->x='_' || x='\'')) in
  pure (Name (implode (letter::rest))) << ws

let const_parser = 
  int_parser <|> bool_parser <|> empty_parser <|> name_parser

let push_parser = 
  (literal "Push") >>= fun _ ->
  whitespace >>= fun _ ->
  const_parser >>= fun c -> 
  ws >>= fun _ ->
  pure (Push c)

let pop_parser = 
  (literal "Pop") >>= fun _ ->
  whitespace >>= fun _ ->
  int_parser >>= fun i -> 
  ws >>= fun _ ->
  pure (Pop i)

let trace_parser = 
  (literal "Trace") >>= fun _ ->
  whitespace >>= fun _ ->
  int_parser >>= fun i -> 
  ws >>= fun _ ->
  pure (Trace i)

let add_parser = 
  (literal "Add") >>= fun _ ->
  whitespace >>= fun _ ->
  int_parser >>= fun i -> 
  ws >>= fun _ ->
  pure (Add i)

let sub_parser = 
  (literal "Sub") >>= fun _ ->
  whitespace >>= fun _ ->
  int_parser >>= fun i -> 
  ws >>= fun _ ->
  pure (Sub i)

let mul_parser = 
  (literal "Mul") >>= fun _ ->
  whitespace >>= fun _ ->
  int_parser >>= fun i -> 
  ws >>= fun _ ->
  pure (Mul i)

let div_parser = 
  (literal "Div") >>= fun _ ->
  whitespace >>= fun _ ->
  int_parser >>= fun i -> 
  ws >>= fun _ ->
  pure (Div i)

let and_parser = 
  (literal "And") >>= fun _ -> 
  ws >>= fun _ ->
  pure (And)

let or_parser = 
  (literal "Or") >>= fun _ -> 
  ws >>= fun _ ->
  pure (Or)

let not_parser = 
  (literal "Not") >>= fun _ -> 
  ws >>= fun _ ->
  pure (Not)

let equal_parser = 
  (literal "Equal") >>= fun _ -> 
  ws >>= fun _ ->
  pure (Equal)

let lte_parser = 
  (literal "Lte") >>= fun _ -> 
  ws >>= fun _ ->
  pure (Lte)

let local_parser = 
  (literal "Local") >>= fun _ -> 
  ws >>= fun _ ->
  pure (Local)

let global_parser = 
  (literal "Global") >>= fun _ -> 
  ws >>= fun _ ->
  pure (Global)

let lookup_parser = 
  (literal "Lookup") >>= fun _ -> 
  ws >>= fun _ ->
  pure (Lookup)

let call_parser = 
  let* c = keyword "Call" in
  pure (Call) << ws

let rec basic_com_parser () =
  ws >> push_parser <|> pop_parser <|> trace_parser <|> 
        add_parser <|> sub_parser <|> mul_parser <|> div_parser <|>
        and_parser <|> or_parser <|> not_parser <|>
        equal_parser <|> lte_parser <|>
        local_parser <|> global_parser <|> lookup_parser <|>
        begin_parser () <|> ifelse_parser () <|>
        fun_parser () <|> call_parser <|> try_parser () <|> switch_parser ()

and begin_parser () =
  ws >>
  let* first = keyword "Begin" in
  let* c_list = many (basic_com_parser ()) in 
  let* last = keyword "End" in
  pure (Begin c_list) << ws

and ifelse_parser () =
  ws >>
  let* first = keyword "If" in
  let* c_list1 = many (basic_com_parser ()) in 
  let* second = keyword "Else" in
  let* c_list2 = many (basic_com_parser ()) in 
  let* last = keyword "End" in
  pure (IfElse (c_list1, c_list2)) << ws

and fun_parser () =
  ws >>
  let* first = keyword "Fun" in
  let* name1 = name_parser in 
  let* name2 = name_parser in 
  let* c_list = many (basic_com_parser ()) in
  let* last = keyword "End" in
  pure (Fun (name1, name2, c_list)) << ws

and try_parser () =
  ws >>
  let* first = keyword "Try" in
  let* c_list = many (basic_com_parser ()) in 
  let* last = keyword "End" in
  pure (Try c_list) << ws

and switch_parser () =
  ws >>
  let* first = keyword "Switch" in
  let* case_list = many (
      let* case = keyword "Case" in
      let* i = int_parser in
      let* c_list = many (basic_com_parser ()) in
      pure (i, c_list)) in
  let* last = keyword "End" in
  pure (Switch case_list) << ws

let coms_parser = fun ls -> (many1 (basic_com_parser ())) ls

let whitespace_blank_remover ls = 
  List.filter (fun x -> x!=' ' && (is_blank x)=false) ls 
let list_of_coms string_coms = 
  match (parse coms_parser string_coms) with
  | Some (h::t, []) -> Some (h::t)
  | Some (h::t, h2::t2) -> (
      match whitespace_blank_remover (h2::t2) with
      | [] -> Some (h::t)
      | _ -> None)
  | Some ([], []) -> Some ([])
  | Some ([], h2::t2) -> None
  | None -> None

type value = 
  | Number of int
  | Boolean of bool
  | Null of unit
  | Name of string
  | Clo of (((string * value) list * (string * value) list * (string * value) list) * string * string * com list )

let rec eval com_list =
  let rec aux (stack: value list) (log: string list) (com_list: com list) (env: ((string * value) list * (string * value) list * (string * value) list))= (
    match com_list with 
    | [] -> Some(stack, log, env, "success")
    | com::rest_com_list -> (
        match com with 
        | Push c -> (
            match c with 
            | Number i -> aux (Number i::stack) log rest_com_list env
            | Boolean b -> aux (Boolean b::stack) log rest_com_list env
            | Null x -> aux (Null x::stack) log rest_com_list env
            | Name name -> aux (Name name::stack) log rest_com_list env)
        | Pop i -> (
            match i with
            | Number i -> (
                if i<0 then
                  Some(stack, log, env, "fail")
                else if i=0 then (
                  aux stack log rest_com_list env
                )
                else (
                  match stack with
                  | [] -> Some(stack, log, env, "fail")
                  | top::rest_stack -> aux rest_stack log ((Pop (Number (i-1))::rest_com_list)) env
                ))
            | _ -> aux stack log rest_com_list env)
        | Trace i -> (
            match i with
            | Number i -> (
                if i<0 then
                  Some(stack, log, env, "fail")
                else if i=0 then (
                  aux stack log rest_com_list env
                )
                else (
                  match stack, log with
                  | [], _ -> Some(stack, log, env, "fail")
                  | top::rest_stack, log_ls -> (
                      match top with
                      | Number n -> aux rest_stack ((string_of_int n)::log_ls) ((Trace (Number (i-1))::rest_com_list)) env
                      | Boolean true -> aux rest_stack ("True"::log_ls) ((Trace (Number (i-1))::rest_com_list)) env
                      | Boolean false -> aux rest_stack ("False"::log_ls) ((Trace (Number (i-1))::rest_com_list)) env
                      | Null x -> aux rest_stack ("()"::log_ls) ((Trace (Number (i-1))::rest_com_list)) env
                      | Name name -> aux rest_stack (name::log_ls) ((Trace (Number (i-1))::rest_com_list)) env
                      | Clo _ -> aux rest_stack ("<fun>"::log_ls) ((Trace (Number (i-1))::rest_com_list)) env)))
            | _ -> Some(stack, log, env, "fail"))
        | Add i -> (
            match i with
            | Number i -> (
                if i<0 then 
                  Some(stack, log, env, "fail")
                else if i=0 then
                  aux (Number 0::stack) log rest_com_list env
                else if i=1 then (
                  match stack with 
                  | [] -> Some(stack, log, env, "fail")
                  | (Number n1)::stack_ls -> aux ((Number n1)::stack_ls) log rest_com_list env
                  | _ -> Some(stack, log, env, "fail"))
                else (
                  match stack with 
                  | [] -> Some(stack, log, env, "fail")
                  | top::[] -> Some(stack, log, env, "fail")
                  | top1::top2::rest_stack -> (
                      match top1,top2 with
                      | Number n1, Number n2 -> aux ((Number (n1+n2))::rest_stack) log ((Add (Number (i-1)))::rest_com_list) env 
                      | _, _ -> Some(stack, log, env, "fail"))))
            | _ -> aux stack log rest_com_list env)
        | Sub i -> (
            match i with
            | Number i -> (
                if i<0 then 
                  Some(stack, log, env, "fail")
                else if i=0 then
                  aux (Number 0::stack) log rest_com_list env
                else if i=1 then(
                  match stack with 
                  | [] -> Some(stack, log, env, "fail")
                  | (Number n1)::stack_ls -> aux ((Number n1)::stack_ls) log rest_com_list env
                  | _ -> Some(stack, log, env, "fail"))
                else (
                  match stack with 
                  | [] -> Some(stack, log, env, "fail")
                  | top::[] -> Some(stack, log, env, "fail")
                  | top1::top2::rest_stack -> (
                      match top1,top2 with
                      | Number n1, Number n2 -> aux ((Number (n1-n2))::rest_stack) log ((Sub (Number (i-1)))::rest_com_list) env
                      | _, _ -> Some(stack, log, env, "fail"))))
            | _ -> aux stack log rest_com_list env)
        | Mul i -> (
            match i with
            | Number i -> (
                if i<0 then 
                  Some(stack, log, env, "fail")
                else if i=0 then
                  aux (Number 1::stack) log rest_com_list env
                else if i=1 then(
                  match stack with 
                  | [] -> Some(stack, log, env, "fail")
                  | (Number n1)::stack_ls -> aux ((Number n1)::stack_ls) log rest_com_list env
                  | _ -> Some(stack, log, env, "fail"))
                else (
                  match stack with 
                  | [] -> Some(stack, log, env, "fail")
                  | top::[] -> Some(stack, log, env, "fail")
                  | top1::top2::rest_stack -> (
                      match top1,top2 with
                      | Number n1, Number n2 -> aux ((Number (n1*n2))::rest_stack) log ((Mul (Number (i-1)))::rest_com_list) env
                      | _, _ -> Some(stack, log, env, "fail"))))
            | _ -> aux stack log rest_com_list env)
        | Div i -> (
            match i with
            | Number i -> (
                if i<0 then 
                  Some(stack, log, env, "fail")
                else if i=0 then
                  aux (Number 1::stack) log rest_com_list env
                else if i=1 then(
                  match stack with 
                  | [] -> Some(stack, log, env, "fail")
                  | (Number n1)::stack_ls -> aux ((Number n1)::stack_ls) log rest_com_list env
                  | _ -> Some(stack, log, env, "fail"))
                else (
                  match stack with 
                  | [] -> Some(stack, log, env, "fail")
                  | top::[] -> Some(stack, log, env, "fail")
                  | top1::top2::rest_stack -> (
                      match top1,top2 with
                      | Number dividend, Number n2 -> (
                          match aux ((Number n2)::rest_stack) log ([Mul (Number (i-1))]) env with
                          | None -> None
                          | Some([], log_ls, env, "fail") -> Some([], log_ls, env, "fail")
                          | Some((Number divisor)::rest_stack2, log_ls, env, "success") -> (
                              if divisor=0 then
                                Some(rest_stack2, log_ls, env, "fail")
                              else 
                                aux ((Number (dividend/divisor))::rest_stack2) log_ls rest_com_list env
                            )
                          | Some(value::stack, log_ls, env, "fail") -> Some(value::stack, log_ls, env, "fail")
                          | _ -> None
                        )
                      | _, _ -> Some(stack, log, env, "fail"))))
            | _ -> aux stack log rest_com_list env)
        | And -> (
            match stack with 
            | (Boolean top1)::(Boolean top2)::rest_stack -> aux ((Boolean (top1 && top2))::rest_stack) log rest_com_list env
            | _ -> Some(stack, log, env, "fail"))
        | Or -> (
            match stack with 
            | (Boolean top1)::(Boolean top2)::rest_stack -> aux ((Boolean (top1 || top2))::rest_stack) log rest_com_list env
            | _ -> Some(stack, log, env, "fail"))
        | Not -> (
            match stack with 
            | (Boolean top)::rest_stack -> aux ((Boolean (not top))::rest_stack) log rest_com_list env
            | _ -> Some(stack, log, env, "fail"))
        | Equal -> (
            match stack with 
            | (Number top1)::(Number top2)::rest_stack -> aux ((Boolean (top1=top2))::rest_stack) log rest_com_list env
            | _ -> Some(stack, log, env, "fail"))
        | Lte -> (
            match stack with 
            | (Number top1)::(Number top2)::rest_stack -> aux ((Boolean (top1<=top2))::rest_stack) log rest_com_list env
            | _ -> Some(stack, log, env, "fail"))
        | Local -> (
            match stack, env with 
            | (Name top1)::value::rest_stack, (local_env_temp, local_env_inherited, global_env) -> 
              aux ((Null ())::rest_stack) log rest_com_list ((top1, value)::local_env_temp, local_env_inherited, global_env)
            | _ -> Some(stack, log, env, "fail"))
        | Global -> (
            match stack, env with 
            | (Name top1)::value::rest_stack, (local_env_temp, local_env_inherited, global_env) -> 
              aux ((Null ())::rest_stack) log rest_com_list (local_env_temp, local_env_inherited, (top1, value)::global_env)
            | _ -> Some(stack, log, env, "fail"))
        | Lookup -> (
            match stack, env with 
            | (Name top)::rest_stack, (local_env_temp, local_env_inherited, global_env) -> (
                match List.assoc_opt top local_env_temp with
                | Some value -> aux (value::rest_stack) log rest_com_list env
                | None -> (
                    match List.assoc_opt top local_env_inherited with
                    | Some value -> aux (value::rest_stack) log rest_com_list env
                    | None -> (
                        match List.assoc_opt top global_env with
                        | Some value -> aux (value::rest_stack) log rest_com_list env
                        | None -> Some(stack, log, env, "fail"))))
            | _ -> Some(stack, log, env, "fail")
          )
        | Begin c_list -> (
            match env with
            | (local_env_temp, local_env_inherited, global_env) -> (
                match aux [] log c_list ([], local_env_temp@local_env_inherited, global_env) with
                | Some(top1::rest_stack, log1, (local_env_temp1, local_env_inherited1, global_env1), "success") -> 
                  aux (top1::stack) log1 rest_com_list (local_env_temp, local_env_inherited, global_env1)
                | Some([], log1, (local_env_temp1, local_env_inherited1, global_env1), "success") -> Some([], log, (local_env_temp, local_env_inherited, global_env1), "fail")
                | Some(top1::rest_stack, log1, (local_env_temp1, local_env_inherited1, global_env1), "fail") ->  Some(stack, log, (local_env_temp, local_env_inherited, global_env1), "fail")
                | Some([], log1, (local_env_temp1, local_env_inherited1, global_env1), "fail") ->  Some([], log, (local_env_temp, local_env_inherited, global_env1), "fail")  
                | _ -> Some(stack, log, env, "fail")))
        | IfElse (if_c_list, else_c_list) -> (
            match stack with 
            | (Boolean true)::rest_stack -> aux rest_stack log (if_c_list@rest_com_list) env
            | (Boolean false)::rest_stack -> aux rest_stack log (else_c_list@rest_com_list) env
            | _ -> Some(stack, log, env, "fail"))
        | Fun (name1, name2, c_list) -> (
            match name1, name2, env with
            | Name n1, Name n2, (local_env_temp, local_env_inherited, global_env) -> (
                aux stack log rest_com_list ((n1, Clo (env, n1, n2, c_list))::local_env_temp, local_env_inherited, global_env))
            | _ -> Some(stack, log, env, "fail")
          )
        | Call -> (
            match stack, env with
            | (Clo ((local_env_temp_c, local_env_inherited_c, global_env_c), n1, n2, c_list_clo)::value::rest_stack), (local_env_temp, local_env_inherited, global_env) -> (
                let old_clo = Clo ((local_env_temp_c, local_env_inherited_c, global_env_c), n1, n2, c_list_clo) in
                let new_env = ((n1, old_clo)::(n2, value)::local_env_temp_c, local_env_inherited_c, global_env@global_env_c) in
                match aux [] log c_list_clo new_env with 
                | Some (top_c::rest_stack_c, log_c, (local_env_temp_res, local_env_inherited_res, global_env_res), "success") -> 
                  aux (top_c::rest_stack) log_c rest_com_list (local_env_temp, local_env_inherited, global_env_res)
                | Some ([], _, _, _) -> Some(stack, log, env, "fail")
                | Some (h::t, _, _, _) -> Some(stack, log, env, "fail")
                | None -> None)
            | _ -> None)
        | Try c_list -> (
            match (aux [] log c_list env), env with
            | Some (top::rest_stack_c, log_res, (local_env_temp_res, local_env_inherited_res, global_env_res), "success"), (local_env_temp, local_env_inherited, global_env) ->
              aux (top::stack) log_res rest_com_list (local_env_temp, local_env_inherited, global_env_res)
            | Some ([], _, _, "success"), _ -> Some(stack, log, env, "fail")
            | Some (_, _, (local_env_temp_res, local_env_inherited_res, global_env_res), "fail"), (local_env_temp, local_env_inherited, global_env) -> 
              aux stack log rest_com_list (local_env_temp, local_env_inherited, global_env_res)
            | _ -> Some(stack, log, env, "fail")
          )
        | Switch case_list -> (
            match stack, case_list with
            | Number top_i::rest_stack, [] -> Some(stack, log, env, "fail")
            | Number top_i::rest_stack, (Number case_i, c_list)::rest_case_list -> (
                if top_i=case_i then
                  aux rest_stack log (c_list@rest_com_list) env 
                else
                  aux (Number top_i::rest_stack) log ((Switch rest_case_list)::rest_com_list) env 
              )
            | _ -> Some(stack, log, env, "fail")
          )
      )
  )
  in aux [] [] com_list ([], [], [])


let interp (src : string) : string list = 
  let com_list = list_of_coms src in 
  match com_list with
  | None -> ["Error"]
  | Some([]) -> []
  | Some(h::t) -> 
    match (eval (h::t)) with
    | None -> ["Error"]
    | Some(_, [], _, "fail") -> []
    | Some(_, h::t, _, "success") -> h::t
    | _ -> []


(* Calling (main "test.txt") will read the file test.txt and run interp on it.
   This is only used for debugging and will not be used by the gradescope autograder. *)
let main fname =
  let src = readlines fname in
  interp src
