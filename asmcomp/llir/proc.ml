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

let num_register_classes = 2
let num_available_registers = [| 0; 0 |]
let first_available_register = [| 0; 0 |]
let rotate_registers = false

let register i t typ =
  let key = match t with
    | Arg     -> (i lsl 3) lor 0
    | Param   -> (i lsl 3) lor 1
    | Result  -> (i lsl 3) lor 2
    | Generic -> (i lsl 3) lor 3
  in
  Reg.at_location typ (Reg key)

let phys_reg _ = assert false

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
let loc_exn_bucket = register 0 Generic Val


let loc_arguments arg =
  (* Outgoing parameters to a call. *)
  (Array.mapi (fun i arg -> register i Arg arg.typ) arg, 0)

let loc_parameters arg =
  (* Incoming parameters to a function. *)
  Array.mapi (fun i arg -> register i Param arg.typ) arg

let loc_results arg =
  (* Outgoing results from a call. *)
  Array.mapi (fun i arg -> register i Result arg.typ) arg


let loc_external_results res =
  Array.mapi (fun i arg -> register i Result arg.typ) res

let loc_external_arguments arg =
  let loc = arg |> Array.mapi (fun i arg -> [| register i Arg arg.(0).typ |])
  in (loc, 0)


let register_name reg = "$" ^ string_of_int reg

let register_class reg = match reg.typ with
  | Float -> 1
  | _ -> 0

let word_addressed = false

let num_stack_slots = [| 0; 0 |]
let contains_calls = ref false

let assemble_file infile outfile =
  let infile = Filename.quote infile in
  let outfile = Filename.quote outfile in
  Ccomp.command (Format.sprintf "cp %s %s\n" infile outfile)

let init () =
    ()
