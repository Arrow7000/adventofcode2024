(* open Ppxlib
   open Ast_builder.Default

   (** Helper to create printf expressions for results *)
   let make_printf ~loc fmt value =
     pexp_apply ~loc
       (evar ~loc "Printf.printf")
       [ (Nolabel, estring ~loc fmt); (Nolabel, value) ]

   (** Generate expression to run a single AoC day's solutions *)
   let generate_day_runner ~loc day_number =
     let module_name = Printf.sprintf "Day%d" day_number in

     let run_part ~loc part_num part_fn =
       pexp_try ~loc
         (make_printf ~loc
            (Printf.sprintf "Part %d: %%s\n" part_num)
            (pexp_apply ~loc (evar ~loc part_fn) [ (Nolabel, eunit ~loc) ]))
         [
           case
             ~lhs:(ppat_var ~loc (Located.mk ~loc "e"))
             ~guard:None
             ~rhs:
               (make_printf ~loc "Part %d not implemented yet: %s\n"
                  (pexp_tuple ~loc
                     [
                       eint ~loc part_num;
                       pexp_apply ~loc
                         (evar ~loc "Printexc.to_string")
                         [ (Nolabel, evar ~loc "e") ];
                     ]));
         ]
     in

     let open_expr =
       pexp_open ~loc
         {
           popen_expr =
             pmod_ident ~loc (Located.mk ~loc (Longident.Lident module_name));
           popen_override = Fresh;
           popen_loc = loc;
           popen_attributes = [];
         }
         (pexp_sequence ~loc
            (make_printf ~loc "\nDay %d:\n" (eint ~loc day_number))
            (pexp_sequence ~loc
               (run_part ~loc 1 "solve_part1")
               (run_part ~loc 2 "solve_part2")))
     in
     open_expr

   (** Extension point implementation for running days *)
   let days_runner ~ctxt arg =
     let loc = Expansion_context.Extension.extension_point_loc ctxt in

     (* Parse the argument to determine which days to run *)
     match arg with
     | None ->
         (* No arg: Run all days 1-25 *)
         let days = List.init 25 (fun i -> i + 1) in
         let day_runners = List.map (generate_day_runner ~loc) days in
         pexp_sequence ~loc
           (make_printf ~loc "Running all days...\n" (eunit ~loc))
           (List.fold_left
              (fun acc expr -> pexp_sequence ~loc acc expr)
              (List.hd day_runners) (List.tl day_runners))
     | Some (Pexp_constant (Pconst_integer (day, _))) ->
         (* Specific day number provided *)
         let day = int_of_string day in
         if day < 1 || day > 25 then
           Location.raise_errorf ~loc "Day must be between 1 and 25"
         else generate_day_runner ~loc day
     | Some _ ->
         Location.raise_errorf ~loc "Expected integer day number or no argument"

   (** Register the PPX extension *)
   let extension =
     Extension.V3.declare "run_days" Extension.Context.expression
       Ast_pattern.(pstr nil)
       (fun ~ctxt ->
         let loc = Expansion_context.Extension.extension_point_loc ctxt in
         let env = Expansion_context.Extension.environment ctxt in

         let has_value modname fname =
           try
             ignore (Env.lookup_value (Longident.Lident fname) env);
             true
           with Not_found -> false
         in

         let open_expr =
           pexp_open ~loc
             {
               popen_expr =
                 pmod_ident ~loc (Located.mk ~loc (Longident.Lident "Day1"));
               popen_override = Fresh;
               popen_loc = loc;
               popen_attributes = [];
             }
             (pexp_sequence ~loc
                (pexp_apply ~loc
                   (evar ~loc "Printf.printf")
                   [
                     (Nolabel, estring ~loc "Part 1: %s\n");
                     ( Nolabel,
                       pexp_apply ~loc (evar ~loc "solve_part1")
                         [ (Nolabel, eunit ~loc) ] );
                   ])
                (if has_value "Day1" "solve_part2" then
                   pexp_apply ~loc
                     (evar ~loc "Printf.printf")
                     [
                       (Nolabel, estring ~loc "Part 2: %s\n");
                       ( Nolabel,
                         pexp_apply ~loc (evar ~loc "solve_part2")
                           [ (Nolabel, eunit ~loc) ] );
                     ]
                 else
                   pexp_apply ~loc
                     (evar ~loc "Printf.printf")
                     [ (Nolabel, estring ~loc "Part 2 not implemented yet\n") ]))
         in
         open_expr)

   let rule = Context_free.Rule.extension extension
   let () = Driver.register_transformation ~rules:[ rule ] "aoc_runner" *)
