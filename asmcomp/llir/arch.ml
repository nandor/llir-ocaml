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

type subarch =
  | POWER
  | AMD64
  | ARM64
  | RISCV

let subarch =
  match Config.architecture with
  | "llir_amd64" -> AMD64
  | "llir_arm64" -> ARM64
  | "llir_riscv" -> RISCV
  | "llir_power" -> POWER
  | _ -> failwith "invalid architecture"

type addressing_mode = Iindexed of int

type specific_operation =
  | Isqrt
  | Ibswap16
  | Ibswap32
  | Ibswap64

let big_endian = false
let size_addr = 8
let size_int = 8
let size_float = 8
let allow_unaligned_access = true
let division_crashes_on_overflow = true

let spacetime_node_hole_pointer_is_live_before _ = false

let print_addressing printreg addr ppf arg =
  match addr with
  | Iindexed(n) ->
    printreg ppf arg.(0);
    if n <> 0 then Format.fprintf ppf ", %i" n

let print_specific_operation printreg op ppf arg =
  match op with
  | Isqrt ->
      Format.fprintf ppf "sqrt %a" printreg arg.(0)
  | Ibswap16 ->
      Format.fprintf ppf "bswap16 %a" printreg arg.(0)
  | Ibswap32 ->
      Format.fprintf ppf "bswap32 %a" printreg arg.(0)
  | Ibswap64 ->
      Format.fprintf ppf "bswap64 %a" printreg arg.(0)

let offset_addressing addr delta =
  match addr with
  | Iindexed n -> Iindexed (n + delta)

let identity_addressing = Iindexed 0

let command_line_options = []
