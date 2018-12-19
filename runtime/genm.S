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
  .call       c
  .args       0
.Lalloc1:
  mov.i64     $0, caml_young_ptr
  ld.8.i64    $1, [$0]
  mov.i64     $2, 16
  sub.i64     $3, $1, $2

  mov.i64     $4, caml_young_limit
  ld.8.i64    $5, [$4]

  cmp.ge.i32  $6, $3, $5
  jf          $6, .Lcollect1

  st.8        [$0], $3
  mov.i64     $7, 8
  add.i64     $8, $3, $7
  ret.i64     $8

.Lcollect1:
  mov.i64     $99, caml_call_gc
  call.c      $99
  jmp         .Lalloc1

################################################################################
# caml_alloc2
################################################################################
  .text
  .globl caml_alloc2
caml_alloc2:
  .call       c
  .args       0
.Lalloc2:
  mov.i64     $0, caml_young_ptr
  ld.8.i64    $1, [$0]
  mov.i64     $2, 24
  sub.i64     $3, $1, $2

  mov.i64     $4, caml_young_limit
  ld.8.i64    $5, [$4]

  cmp.ge.i32  $6, $3, $5
  jf          $6, .Lcollect2

  st.8        [$0], $3
  mov.i64     $7, 8
  add.i64     $8, $3, $7
  ret.i64     $8

.Lcollect2:
  mov.i64     $99, caml_call_gc
  call.c      $99
  jmp         .Lcollect2

################################################################################
# caml_alloc3
################################################################################
  .text
  .globl caml_alloc3
caml_alloc3:
  .call       c
  .args       0
.Lalloc3:
  mov.i64     $0, caml_young_ptr
  ld.8.i64    $1, [$0]
  mov.i64     $2, 32
  sub.i64     $3, $1, $2

  mov.i64     $4, caml_young_limit
  ld.8.i64    $5, [$4]

  cmp.ge.i32  $6, $3, $5
  jf          $6, .Lcollect3

  st.8        [$0], $3
  mov.i64     $7, 8
  add.i64     $8, $3, $7
  ret.i64     $8

.Lcollect3:
  mov.i64     $99, caml_call_gc
  call.c      $99
  jmp         .Lcollect3

################################################################################
# caml_allocN
################################################################################
  .text
  .globl caml_allocN
caml_allocN:
  .call       c
  .args       0, i64
.LallocN:
  mov.i64     $0, caml_young_ptr
  ld.8.i64    $1, [$0]
  arg.i64     $2, 0
  sub.i64     $3, $1, $2

  mov.i64     $4, caml_young_limit
  ld.8.i64    $5, [$4]

  cmp.ge.i32  $6, $3, $5
  jf          $6, .LcollectN

  st.8        [$0], $3
  mov.i64     $7, 8
  add.i64     $8, $3, $7
  ret.i64     $8

.LcollectN:
  mov.i64     $99, caml_call_gc
  call.c      $99
  jmp         .LcollectN

################################################################################
# caml_c_call_i64
################################################################################
  .text
  .globl caml_c_call_i64
caml_c_call_i64:
  .call       ext
  .args       0, i64, i64

  mov.i64     $0, $sp
  ld.8.i64    $1, [$0]

  mov.i64     $2, caml_last_return_address
  st.8        [$2], $1
  mov.i64     $3, caml_bottom_of_stack
  st.8        [$3], $0

  arg.i64     $4, 0
  arg.i64     $5, 1
  tcall.i64.c $4, $5

################################################################################
# caml_c_call_i64_i64
################################################################################
  .text
  .globl caml_c_call_i64_i64
caml_c_call_i64_i64:
  .call       ext
  .args       0, i64, i64, i64

  mov.i64     $0, $sp
  ld.8.i64    $1, [$0]

  mov.i64     $2, caml_last_return_address
  st.8        [$2], $1
  mov.i64     $3, caml_bottom_of_stack
  st.8        [$3], $0

  arg.i64     $4, 0
  arg.i64     $5, 1
  arg.i64     $6, 2
  tcall.i64.c $4, $5, $6

################################################################################
# caml_c_call_i64_i64_i64
################################################################################
  .text
  .globl caml_c_call_i64_i64_i64
caml_c_call_i64_i64_i64:
  .call       ext
  .args       0, i64, i64, i64, i64

  mov.i64     $0, $sp
  ld.8.i64    $1, [$0]

  mov.i64     $2, caml_last_return_address
  st.8        [$2], $1
  mov.i64     $3, caml_bottom_of_stack
  st.8        [$3], $0

  arg.i64     $4, 0
  arg.i64     $5, 1
  arg.i64     $6, 2
  arg.i64     $7, 3
  tcall.i64.c $4, $5, $6, $7

################################################################################
# caml_c_call_i64_i64_i64_i64
################################################################################
  .text
  .globl caml_c_call_i64_i64_i64_i64
caml_c_call_i64_i64_i64_i64:
  .call       ext
  .args       0, i64, i64, i64, i64, i64, i64

  mov.i64     $0, $sp
  ld.8.i64    $1, [$0]

  mov.i64     $2, caml_last_return_address
  st.8        [$2], $1
  mov.i64     $3, caml_bottom_of_stack
  st.8        [$3], $0

  arg.i64     $4, 0
  arg.i64     $5, 1
  arg.i64     $6, 2
  arg.i64     $7, 3
  arg.i64     $8, 4
  arg.i64     $9, 5
  tcall.i64.c $4, $5, $6, $7, $8, $9

################################################################################
# caml_c_call_i64_i64_i64_i64_i64
################################################################################
  .text
  .globl caml_c_call_i64_i64_i64_i64_i64
caml_c_call_i64_i64_i64_i64_i64:
  .call       ext
  .args       0, i64, i64, i64, i64, i64

  mov.i64     $0, $sp
  ld.8.i64    $1, [$0]

  mov.i64     $2, caml_last_return_address
  st.8        [$2], $1
  mov.i64     $3, caml_bottom_of_stack
  st.8        [$3], $0

  arg.i64     $4, 0
  arg.i64     $5, 1
  arg.i64     $6, 2
  arg.i64     $7, 3
  arg.i64     $8, 4
  tcall.i64.c $4, $5, $6, $7, $8

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
# caml_callback_exn
################################################################################
  .text
  .globl caml_callback_exn
caml_callback_exn:
  .call             c
  .args             0, i64, i64

  arg.i64           $0, 0
  arg.i64           $1, 1
  ld.8.i64          $2, [$0]

  undef.i64         $3
  mov.i64           $4, caml_start_program_impl
  tcall.i64.c       $4, $2, $1, $0, $3, $3

################################################################################
# caml_callback2_exn
################################################################################
  .text
  .globl caml_callback2_exn
caml_callback2_exn:
  .call             c
  .args             0, i64, i64, i64

  arg.i64           $0, 0
  arg.i64           $1, 1
  arg.i64           $2, 2

  undef.i64         $3
  mov.i64           $4, caml_start_program_impl
  mov.i64           $5, caml_apply2
  tcall.i64.c       $4, $5, $1, $2, $0, $3

################################################################################
# caml_callback3_exn
################################################################################
  .text
  .globl caml_callback3_exn
caml_callback3_exn:
  .call             c
  .args             0, i64, i64, i64, i64

  arg.i64           $0, 0
  arg.i64           $1, 1
  arg.i64           $2, 2
  arg.i64           $3, 3

  mov.i64           $4, caml_start_program_impl
  mov.i64           $5, caml_apply3
  tcall.i64.c       $4, $5, $1, $2, $3, $0

################################################################################
# caml_stack_overflow
################################################################################
  .text
  .globl caml_raise_exception
caml_raise_exception:
  .call       c
  .args       0, i64

  # Check if traces are active.
  mov.i64     $0, caml_backtrace_active
  ld.4.i32    $1, [$0]
  mov.i32     $2, 1
  cmp.eq.i32  $3, $1, $2
  jt          $3, .Lactive
.Lthrow:
  # Raise the exception here.
  arg.i64     $10, 0
  mov.i64     $11, caml_exn_val
  st.8        [$11], $10

  # Load the frame.
  mov.i64     $14, caml_exception_pointer
  mov.i64     $15, 8
  ld.8.i64    $16, [$14]
  add.i64     $17, $16, $15
  add.i64     $18, $17, $15

  ld.8.i64    $19, [$16]    # Address.
  ld.8.i64    $20, [$17]    # Stack pointer.
  ld.8.i64    $21, [$18]    # Next frame.

  # Jump.
  set.i64     $sp, $20
  st.8        [$14], $21
  ji          $19
.Lactive:
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
  mov.i64           $0, caml_program
  mov.i64           $1, caml_start_program_impl
  undef.i64         $2
  tcall.i64.c       $1, $0, $2, $2, $2, $2

################################################################################
# caml_start_program_impl
################################################################################
caml_start_program_impl:
  .call             c
  .stack            24
  .args             0, i64, i64, i64, i64, i64
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
  frame.i64         $53, 16           # Pointer to exn chain.
  ld.8.u64          $56, [$6]
  st.8              [$53], $56
  st.8              [$6], $51         # Pointer to this structure.

  # Invoke the OCaml program.
  arg.i64           $10, 0
  arg.i64           $30, 1
  arg.i64           $31, 2
  arg.i64           $32, 3
  arg.i64           $33, 4
  invoke.i64.ocaml  $11, $10, $30, $31, $32, $33, .Lhandler @caml_root_frame

.Lreturn:
  phi.i64           $12, .Lentry, $11, .Lhandler, $23

  # Restore global variables, popping the handler.
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
