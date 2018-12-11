################################################################################
#                                                                              #
#                                  OCaml                                       #
#                                                                              #
#                  Nandor Licker, University of Cambridge                      #
#                                                                              #
#    Copyright 2018--     Nandor Licker                                        #
#                                                                              #
#    All rights reserved.  This file is distributed under the terms of         #
#    the GNU Lesser General Public License version 2.1, with the               #
#    special exception on linking described in the file LICENSE.               #
#                                                                              #
################################################################################


################################################################################
# caml_alloc1
################################################################################
  .text
  .globl caml_alloc1
caml_alloc1:
  trap

################################################################################
# caml_alloc2
################################################################################
  .text
  .globl caml_alloc2
caml_alloc2:
  trap

################################################################################
# caml_alloc3
################################################################################
  .text
  .globl caml_alloc3
caml_alloc3:
  trap

################################################################################
# caml_allocN
################################################################################
  .text
  .globl caml_allocN
caml_allocN:
  trap

################################################################################
# caml_c_call
################################################################################
  .text
  .globl caml_c_call
caml_c_call:
  trap

################################################################################
# caml_call_gc
################################################################################
  .text
  .globl caml_call_gc
caml_call_gc:
  trap

################################################################################
# caml_ml_array_bound_error
################################################################################
  .text
  .globl caml_ml_array_bound_error
caml_ml_array_bound_error:
  trap

################################################################################
# caml_raise_exn
################################################################################
  .text
  .globl caml_raise_exn
caml_raise_exn:
  trap

  .data
  .globl caml_exn_val
caml_exn_val:
  .quad 0

################################################################################
# caml_callback2_exn
################################################################################
  .text
  .globl caml_callback2_exn
caml_callback2_exn:
  trap

################################################################################
# caml_callback3_exn
################################################################################
  .text
  .globl caml_callback3_exn
caml_callback3_exn:
  trap

################################################################################
# caml_callback_exn
################################################################################
  .text
  .globl caml_callback_exn
caml_callback_exn:
  trap

################################################################################
# caml_stack_overflow
################################################################################
  .text
  .globl caml_raise_exception
caml_raise_exception:
  trap

################################################################################
# caml_start_program
################################################################################
  .text
  .globl caml_start_program
caml_start_program:
  .stack        24

.Lentry:
  # Save global variables.
  mov.i64           $0, caml_gc_regs
  ld.8.i64          $1, [$0]
  mov.i64           $2, caml_last_return_address
  ld.8.i64          $3, [$2]
  mov.i64           $4, caml_bottom_of_stack
  ld.8.i64          $5, [$4]
  mov.i64           $6, caml_exception_pointer
  ld.8.i64          $7, [$6]

  # Create an exception frame.
  frame.i64         $51, 0            # Pointer to handler.
  mov.i64           $54, .Lhandler
  st.8              [$51], $54
  frame.i64         $52, 8            # Pointer to stack frame.
  mov.i64           $55, $sp
  st.8              [$52], $55
  frame.i64         $53, 6            # Pointer to exn chain.
  ld.8.u64          $56, [$6]
  st.8              [$53], $56
  st.8              [$6], $51         # Pointer to this structure.

  # Invoke the OCaml program.
  mov.i64           $10, caml_program
  invoke.i64.ocaml  $11, $10, .Lhandler       @caml_root_frame

.Lreturn:
  phi.i64           $12, .Lentry, $11, .Lhandler, $23

  # Restore global variables, popping the trap.
  st.8              [$6], $7
  st.8              [$4], $5
  st.8              [$2], $3
  st.8              [$0], $1

  ret.i64           $12

.Lhandler:
  # Exception handler, marks value as exception.
  mov.i64           $20, caml_exn_val
  ld.8.i64          $21, [$20]
  mov.i64           $22, 2
  or.i64            $23, $21, $22
  jmp               .Lreturn


################################################################################
# caml_stack_overflow
################################################################################
  .text
  .globl caml_stack_overflow
caml_stack_overflow:
  trap
