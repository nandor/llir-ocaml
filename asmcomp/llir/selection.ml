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

open Arch
open Cmm



class selector = object
  inherit Selectgen.selector_generic as super

  method is_immediate _ = true

  method select_addressing _chunk = function
    | Cop((Cadda | Caddv), [arg; Cconst_int(n, _)], _) ->
      (Iindexed n, arg)
    | arg ->
      (Iindexed 0, arg)

  method! select_operation op args dbg =
    match op, subarch with
    | Cextcall("sqrt", _, _, _), (ARM64|AMD64) ->
      (Ispecific Isqrt, args)
    | Cextcall("caml_bswap16_direct", _, _, _), _ ->
      (Ispecific Ibswap16, args)
    | Cextcall("caml_int32_direct_bswap", _, _, _), _ ->
      (Ispecific Ibswap32, args)
    | Cextcall("caml_int64_direct_bswap", _, _, _), _ ->
      (Ispecific Ibswap64, args)
    | Cextcall("caml_nativeint_direct_bswap", _, _, _), _ ->
      (Ispecific Ibswap64, args)
    | _ ->
      super#select_operation op args dbg
end

let fundecl f =
  (new selector)#emit_fundecl f
