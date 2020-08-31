#*******************************************************************************
#*                                                                             *
#*                                 OCaml                                       *
#*                                                                             *
#*                 Nandor Licker, University of Cambridge                      *
#*                                                                             *
#*   Copyright 2018--     Nandor Licker                                        *
#*                                                                             *
#*   All rights reserved.  This file is distributed under the terms of         *
#*   the GNU Lesser General Public License version 2.1, with the               *
#*   special exception on linking described in the file LICENSE.               *
#*                                                                             *
#*******************************************************************************


################################################################################
# caml_callback_asm
################################################################################
  .text
  .globl caml_callback_asm
caml_callback_asm:
  .call             c
  .args             i64, i64
  trap

################################################################################
# caml_callback2_asm
################################################################################
  .text
  .globl caml_callback2_asm
caml_callback2_asm:
  .call             c
  .args             i64, i64, i64
  trap

################################################################################
# caml_callback3_asm
################################################################################
  .text
  .globl caml_callback3_asm
caml_callback3_asm:
  .call             c
  .args             i64, i64, i64, i64
  trap

################################################################################
# caml_raise_exn
################################################################################
  .text
  .globl caml_raise_exn
caml_raise_exn:
  .call             caml_raise
  .args             i64
  trap



################################################################################
# caml_raise_exception
################################################################################
  .text
  .globl caml_raise_exception
caml_raise_exception:
  .call       c
  .args       i64
  trap

################################################################################
# caml_exn_val
################################################################################
  .data
  .globl caml_exn_val
caml_exn_val:
  .quad 0

################################################################################
# caml_start_program
################################################################################
  .text
  .globl caml_start_program
caml_start_program:
  .call             c
  .stack_object     0, 24, 8
  .stack_object     1, 32, 8
  trap

################################################################################
# caml_start_program_helper
################################################################################
caml_start_program_helper:
  .call             c
  .stack_object     0, 24, 8
  .stack_object     1, 32, 8
  .args             i64, i64, i64, i64, i64
  trap


################################################################################
# caml_stack_overflow
################################################################################
  .text
  .globl caml_stack_overflow
caml_stack_overflow:
  trap
