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
  inherit Selectgen.selector_generic

  method is_immediate _ = true

  method select_addressing _chunk = function
    | Cop((Cadda | Caddv), [arg; Cconst_int n], _) ->
      (Iindexed n, arg)
    | arg ->
      (Iindexed 0, arg)
end

let fundecl f =
  (new selector)#emit_fundecl f
