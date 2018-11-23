(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Nandor Licker, University of Cambridge                 *)
(*                                                                        *)
(*   Copyright 2018--     Nandor Licker                                   *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Specific operations for the GenM processor *)

open Cmm
open Reg

type reg = Arg | Param | Result | Generic

let num_regs = 1000

let num_register_classes = 1
let num_available_registers = [| num_regs |]
let first_available_register = [| 0 |]
let rotate_registers = false

let register i t =
  let key = match t with
    | Arg     -> num_regs + ((i lsl 3) lor 0)
    | Param   -> num_regs + ((i lsl 3) lor 1)
    | Result  -> num_regs + ((i lsl 3) lor 2)
    | Generic -> i
  in
  Reg.at_location Int (Reg key)

let phys_reg i =
  register i Generic

let op_is_pure _op =
  false

let regs_are_volatile _regs =
  false

let destroyed_at_raise = [| |]
let destroyed_at_oper _ = [| |]

let safe_register_pressure _arg = max_int
let max_register_pressure _arg = [| 13; 13 |]

let max_arguments_for_tailcalls  = 10
let loc_spacetime_node_hole = Reg.dummy
let loc_exn_bucket = phys_reg 2000


let loc_arguments arg =
  (* Outgoing parameters to a call. *)
  (Array.mapi (fun i _arg -> register i Arg) arg, 0)

let loc_parameters arg =
  (* Incoming parameters to a function. *)
  Array.mapi (fun i _arg -> register i Param) arg

let loc_results arg =
  (* Outgoing results from a call. *)
  Array.mapi (fun i _arg -> register i Result) arg


let loc_external_results res =
  Array.mapi (fun i _arg -> register i Result) res

let loc_external_arguments arg =
  let loc =  arg |> Array.mapi (fun i _arg -> [| register i Arg |])
  in (loc, 0)


let register_name reg = "$" ^ string_of_int reg

let register_class _reg = 0

let word_addressed = false

let num_stack_slots = [| 0; 0 |]
let contains_calls = ref false

let assemble_file infile outfile =
  let infile = Filename.quote infile in
  let outfile = Filename.quote outfile in
  Ccomp.command (Format.sprintf "cp %s %s\n" infile outfile)

let init () =
    ()
