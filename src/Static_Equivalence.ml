(* Static equivalence in the theory of XOR with inverses *)

open Core_kernel
open Abbrevs
open Util
open Expressions
open Deductibility
open Unification

(* ** XOR static equivalence *)


let pp_bool _fmt b = if b then F.printf "1" else F.printf "0"

let equal_bool_list_opt list1 list2 =
  let b =
  match (list1,list2) with
  | None, None -> true
  | Some bl1, Some bl2 ->
    L.fold_left (L.zip_exn bl1 bl2) ~init:true
      ~f:(fun b (b1,b2) -> if not b then false else (b1 && b2) || (not b1 && not b2))
  | _ -> false
  in
  b

let find_all_solutions matrix =
  (* This function computes all the solutions of a linear system, given the matrix (A | b) in echelon form *)
  let n_vars = (L.length (L.hd_exn matrix)) - 1 in
  (* A column is said to be non-free iff *)
  let non_free_cols =
    L.fold_left (range 0 n_vars)
       ~init:([],[])
       ~f:(fun (cols,rows) c ->
           let count, row =
             L.fold_left matrix
               ~init:(0,None)
               ~f:(fun (count,r) row -> if Z2.is_zero (L.nth_exn row c) then (count,r) else (count+1, Some row) )
           in
           if count = 1 && not(L.mem ~equal:equal_bool_list_opt rows row) then (cols @ [c], rows @ [row])
           else (cols,rows)
         )
    |> (fun (cols,_) -> cols)
  in
  let free_cols = L.filter (range 0 n_vars) ~f:(fun c -> not(L.mem ~equal:(=) non_free_cols c)) in

  L.map (all_bit_strings (L.length free_cols))
    ~f:(fun free ->
        let solution_list  = L.zip_exn free_cols free in
        let solution = Int.Map.of_alist_exn solution_list in
        L.map matrix
          ~f:(fun row ->
            match L.filter (range 0 n_vars) ~f:(fun c -> (Z2.is_one (L.nth_exn row c)) && not(L.mem ~equal:(=) free_cols c)) with
              | [c] ->
                let value =
                  L.fold_left (range 0 n_vars)
                    ~init:Z2.zero
                    ~f:(fun v c' -> if c = c' || (Z2.is_zero (L.nth_exn row c')) then v else Z2.add v (Map.find_exn solution c'))
                in
                [(c, Z2.add value (L.nth_exn row n_vars))]
              | _ -> assert (Z2.is_zero (L.nth_exn row n_vars)); [] (* Otherwise, there is no solution and this should not be executed *)
            )
        |> (fun l ->
            L.sort ((L.concat l) @ solution_list) ~compare:(fun (a,_) (b,_) -> Int.compare a b)
            |> L.map ~f:(fun (_,v) -> v)
          )
      )

let linalg_nth_solution ~nth matrix =
  (* This function computes all the solutions of a linear system, given the matrix (A | b) in echelon form *)
  let n_vars = (L.length (L.hd_exn matrix)) - 1 in
  let non_free_cols =
    L.fold_left (range 0 n_vars)
       ~init:([],[])
       ~f:(fun (cols,rows) c ->
         let count, row =
           L.fold_left matrix
               ~init:(0,None)
               ~f:(fun (count,r) row -> if Z2.is_zero (L.nth_exn row c) then (count,r) else (count+1, Some row) ) in
         if count = 1 && not(L.mem ~equal:equal_bool_list_opt rows row) then (cols @ [c], rows @ [row])
         else (cols,rows)
       )
    |> (fun (cols,_) -> cols)
  in
  let free_cols = L.filter (range 0 n_vars) ~f:(fun c -> not(L.mem ~equal:(=) non_free_cols c)) in
  match nth_bit_string ~nth (L.length free_cols) with
  | None -> None
  | Some free ->
     let solution_list  = L.zip_exn free_cols free in
     let solution = Int.Map.of_alist_exn solution_list in
     Some (L.map matrix
        ~f:(fun row ->
          match L.filter (range 0 n_vars) ~f:(fun c -> (Z2.is_one (L.nth_exn row c)) && not(L.mem ~equal:(=) free_cols c)) with
          | [c] ->
             let value =
               L.fold_left (range 0 n_vars)
                   ~init:Z2.zero
                   ~f:(fun v c' -> if c = c' || (Z2.is_zero (L.nth_exn row c')) then v else Z2.add v (Map.find_exn solution c'))
             in
             [(c, Z2.add value (L.nth_exn row n_vars))]
          | _ -> assert (Z2.is_zero (L.nth_exn row n_vars)); [] (* Otherwise, there is no solution and this should not be executed *)
        )
     |> (fun l ->
       L.sort ((L.concat l) @ solution_list) ~compare:(fun (a,_) (b,_) -> Int.compare a b)
       |> L.map ~f:(fun (_,v) -> v)
        ))

let xor_lin_span frame expr =
  let knowledge = L.map frame.frame_sigma ~f:ground_xor_to_list in
  let e = ground_xor_to_list expr in
  let constants =
    L.map (expr :: frame.frame_sigma) ~f:atoms |> L.concat
    |> L.map ~f:(function | Const a -> a | _ -> assert false)
    |> L.dedup_and_sort ~compare:String.compare
  in
  let forbidden = L.filter constants ~f:(L.mem frame.frame_names ~equal:S.equal) in
  if L.length forbidden = 0 then []
  else
    let contains_const c l = L.mem l c ~equal:S.equal in
    let matrix = L.map forbidden ~f:(fun c -> L.map knowledge ~f:(contains_const c)) in
    let vector = L.map forbidden ~f:(fun c -> contains_const c e) in
    Lin.reduced_row_echelon_form matrix vector

let xor_static_equivalence frame1 frame2 =

  let names_str = "Please, use the same list of forbidden names for both frames" in
  if compare_lists ~compare:String.compare frame1.frame_names frame2.frame_names <> 0 then
    failwith names_str
  else if (L.length frame1.frame_sigma) <> (L.length frame2.frame_sigma) then failwith names_str
  else
    let span1 = xor_lin_span frame1 Zero in
    let span2 = xor_lin_span frame2 Zero in

    let different =
      if (L.length span1) <> (L.length span2) then true
      else
        L.exists (L.zip_exn span1 span2)
          ~f:(fun (row1, row2) ->
              L.exists (L.zip_exn row1 row2) ~f:(fun (t1,t2) -> compare_bool t1 t2 <> 0))
    in
    not (different)

(* ** Static equivalence for functions with inverses *)

let compute_cE funs =
  (* This function returns the bound cE for the size of contexts that is enough to analyze.
     That is, it is sufficient to limit the size of contexts to cE for analyzing static equivalence
     The bound is described in M. Abadi, V. Cortier Theoretical Computer Science 367 (2006) 2-32
   *)
  L.fold_left funs ~init:2 ~f:(fun d (_,inv,d1,d2) -> let m = if inv then 1+d1+2*d2 else 1+d1+d2 in if m > d then m else d)

let build_all_contexts_of_at_most_size_n n atoms funs =
  let all_contexts = ref (Int.Map.of_alist_exn [(1, atoms)]) in

  let rec compute_contexts k =
    if k = 1 then atoms
    else
      let calculated =
        L.map funs ~f:(fun (name,inv,d1,d2) ->
           L.map (all_k_lists_sum_n (d1+d2) (k-1))
             ~f:(fun l ->
               let contexts = L.map l ~f:(fun d -> compute_contexts d) in
               let combinations = combine_lists_all_combinations contexts in
               L.map combinations
                  ~f:(fun comb ->
                    let left,right = split_list_by_idx d1 comb in
                    if inv then
                      (L.map (range 0 d2) ~f:(fun i -> F(name,i,POS,(left,right),inv,false,None))) @
                        (L.map (range 0 d2) ~f:(fun i -> F(name,i,NEG,(left,right),inv,false,None)))
                    else
                      L.map (range 0 d2) ~f:(fun i -> F(name,i,POS,(left,right),inv,false,None))
                  )
               |> L.concat
             )
           |> L.concat
          )
        |> L.concat
      in
      let calculated = L.map calculated ~f:simplify_expr |> dedup_preserving_order ~equal:equal_expr in
      all_contexts := Map.add_exn !all_contexts ~key:k ~data:calculated;
      calculated
  in

  let rec asc_iter k =
    if k > n then ()
    else
      let _ = compute_contexts k in
      asc_iter (k+1)
  in
  asc_iter 2;
  Map.find_exn !all_contexts n

let rec subterms expr =
  match simplify_expr expr with
  | Zero | Const _ | Var _ -> [expr]
  | XOR(list) -> expr :: (L.map list ~f:subterms |> L.concat) |> dedup_preserving_order ~equal:equal_expr
  | F(_,_,_,(left,right),_,_,_) ->
     expr :: (L.map (left@right) ~f:subterms |> L.concat) |> dedup_preserving_order ~equal:equal_expr

let saturate_frame frame =
  let frame_subterms = L.map frame.frame_sigma ~f:subterms |> L.concat |> dedup_preserving_order ~equal:equal_expr in
  L.map frame_subterms
      ~f:(fun s -> match fun_deduce frame s with
                   | None -> []
                   | Some sol -> [(s, sol)]
         )
  |> L.concat

let one_dir_fun_static_equivalence frame1 frame2 =
  (* We put them inside a fake F to analyze them at the same time *)
  let sat = saturate_frame frame1 in
  let sat =
    L.fold_left (range 1 ((L.length frame1.frame_sigma)+1))
       ~init:sat
       ~f:(fun list i ->
         L.map list ~f:(fun (e,formula) ->
             e, replace_expr formula ~old:(Var ("%"^(string_of_int i))) ~by:(Var ("%%"^(string_of_int i))) )
       )
    |> safe_sort ~compare:(fun (e1,_) (e2,_) -> Int.compare (L.length (subterms e1)) (L.length (subterms e2)))
  in
  let (_,b) =
    L.fold_left sat
       ~init:([],true)
       ~f:(fun (accum_sat,b) (e,f) ->
         if not b then ([],b)
         else
           begin match fun_deduce { frame_names = frame1.frame_names; frame_sigma = accum_sat; } e with
           | None -> (accum_sat @ [e]), b
           | Some formula ->
              let formula' =
                L.fold_left (range 0 (L.length accum_sat))
                   ~init:formula
                   ~f:(fun formula' i ->
                     let (_,by) = L.nth_exn sat i in
                     replace_expr formula' ~old:(Var ("%"^(string_of_int (i+1)))) ~by)
              in
              let f2, formula2 =
                L.fold_left (range 0 (L.length frame2.frame_sigma))
                    ~init:(f,formula')
                    ~f:(fun (f2,formula2) i ->
                      let old = Var ("%%"^(string_of_int (i+1))) in
                      let by = L.nth_exn frame2.frame_sigma i in
                      (replace_expr f2 ~old ~by |> simplify_expr,
                       replace_expr formula2 ~old ~by |> simplify_expr)
                    )
              in
              if equal_expr f2 formula2 then (accum_sat @ [e]), b
              else
                ([], false)
           end
       )
  in
  b

let fun_static_equivalence frame1 frame2 =
  if compare_lists ~compare:String.compare frame1.frame_names frame2.frame_names <> 0 then false
  else if (L.length frame1.frame_sigma) <> (L.length frame2.frame_sigma) then false
  else
    if one_dir_fun_static_equivalence frame1 frame2 then one_dir_fun_static_equivalence frame2 frame1
    else false

(* ** Static equivalence for the combined theory *)

let extend_frame frame1 frame2 =

  let subterms2 =
    L.map frame2.frame_sigma ~f:subterms |> L.concat
    |> L.filter ~f:(fun e -> not (L.mem frame2.frame_sigma e ~equal:(fun e1 e2 -> equal_expr (simplify_expr e1) (simplify_expr e2))) )
  in
  let recipes =
    L.map subterms2 ~f:(fun s -> match deduce frame2 s with | None -> [] | Some recipe -> [recipe])
    |> L.concat
  in
  let new_terms_for_sigma1 =
    L.map recipes
       ~f:(fun r ->
         L.fold_left (range 0 (L.length frame1.frame_sigma))
               ~init:r
               ~f:(fun r' i ->
                 let old = Var ("%"^(string_of_int (i+1))) in
                 let by = L.nth_exn frame1.frame_sigma i in
                 replace_expr r' ~old ~by |> simplify_expr
               )
       )
    |> L.map ~f:simplify_expr
  in
  { frame1 with frame_sigma = (L.map (frame1.frame_sigma @ new_terms_for_sigma1) ~f:simplify_expr ) }

let static_equivalence frame1 frame2 =

  if compare_lists ~compare:String.compare frame1.frame_names frame2.frame_names <> 0 then false
  else if (L.length frame1.frame_sigma) <> (L.length frame2.frame_sigma) then false
  else

    let extension1 = extend_frame frame1 frame2 in
    let frame1' = extend_frame extension1 extension1 in
    let frame2' = extend_frame (extend_frame frame2 frame2) extension1 in

    let sub = L.map (frame1'.frame_sigma @ frame2'.frame_sigma) ~f:(fun e -> factor_subterms (simplify_expr e))
              |> L.concat
              |> dedup_preserving_order ~equal:equal_expr
    in
    let fun_subterms = L.filter sub ~f:(fun t -> equal_sig (sign t) F_sig) in
    let fun_subterms =
      fun_subterms
      |> safe_sort
           ~compare:(fun t1 t2 ->
             if equal_expr t1 t2 then 0
             else if expr_is_subterm_of_expr t1 t2 then +1
             else if expr_is_subterm_of_expr t2 t1 then -1
             else 0)
    in
    let xor_subterms = L.filter sub ~f:(fun t -> equal_sig (sign t) XOR_sig) in
    let xor_subterms =
      xor_subterms
      |> safe_sort
           ~compare:(fun t1 t2 ->
             if equal_expr t1 t2 then 0
             else if expr_is_subterm_of_expr t1 t2 then +1
             else if expr_is_subterm_of_expr t2 t1 then -1
             else 0
           )
    in
    let fun_names = fresh_name_vector (L.length fun_subterms) in
    let xor_names = fresh_name_vector (L.length xor_subterms) in
    let replace list e = L.fold_left list ~init:e ~f:(fun e' (t,n) -> replace_expr e' ~old:t ~by:(Const n)) in

    let fun_replace e = replace (L.zip_exn fun_subterms fun_names) (simplify_expr e) in
    let xor_replace e = replace (L.zip_exn xor_subterms xor_names) (simplify_expr e) in
    let frame1'_rho_xor =
      { frame_names = xor_names @ frame1.frame_names;
        frame_sigma = L.map frame1'.frame_sigma ~f:xor_replace;
      }
    in
    let frame2'_rho_xor =
      { frame_names = xor_names @ frame2.frame_names;
        frame_sigma = L.map frame2'.frame_sigma ~f:xor_replace;
      }
    in
    let b = fun_static_equivalence frame1'_rho_xor frame2'_rho_xor in
    if b then
      let frame1'_rho_fun =
        { frame_names = fun_names @ frame1.frame_names;
          frame_sigma = L.map frame1'.frame_sigma ~f:fun_replace;
        }
      in
      let frame2'_rho_fun =
        { frame_names = fun_names @ frame2.frame_names;
          frame_sigma = L.map frame2'.frame_sigma ~f:fun_replace;
        }
      in
      xor_static_equivalence frame1'_rho_fun frame2'_rho_fun
    else
      false
