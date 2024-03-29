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


#define offset_young_ptr            0
#define offset_young_limit          8
#define offset_exception_pointer    16
#define offset_bottom_of_stack      200
#define offset_last_return_address  208
#define offset_gc_regs              216
#define offset_backtrace_active     224
#define offset_backtrace_pos        232
#define offset_callback_link        400



################################################################################
# caml_callback_asm
################################################################################
  .section .text
  .globl caml_callback_asm
caml_callback_asm:
  .call             c
  .args             i64, i64, i64

  arg.i64           $0, 0              # Caml_state
  arg.i64           $1, 1              # closure
  arg.i64           $2, 2              # args
  load.i64          $10, [$1]          # Code
  load.i64          $20, [$2]          # Arg 0
  undef.i64         $22                # Arg 2
  undef.i64         $23                # Arg 3
  mov.i64           $30, caml_start_program_helper
  tcall.i64.c       $30, $0, $10, $20, $1, $22, $23
  .end

################################################################################
# caml_callback2_asm
################################################################################
  .section .text
  .globl caml_callback2_asm
caml_callback2_asm:
  .call             c
  .args             i64, i64, i64

  arg.i64           $0, 0              # Caml_state
  arg.i64           $1, 1              # closure
  arg.i64           $2, 2              # args

  load.i64          $20, [$2]          # Arg 0

  mov.i64           $98, 8
  add.i64           $99, $2, $98
  load.i64          $21, [$99]         # Arg 1

  undef.i64         $23                # Arg 3
  mov.i64           $30, caml_start_program_helper
  mov.i64           $10, caml_apply2
  tcall.i64.c       $30, $0, $10, $20, $21, $1, $23
  .end

################################################################################
# caml_callback3_asm
################################################################################
  .section .text
  .globl caml_callback3_asm
caml_callback3_asm:
  .call             c
  .args             i64, i64, i64
  arg.i64           $0, 0              # Caml_state
  arg.i64           $1, 1              # closure
  arg.i64           $2, 2              # args

  load.i64          $20, [$2]          # Arg 0

  mov.i64           $98, 8
  add.i64           $99, $2, $98
  load.i64          $21, [$99]         # Arg 1

  mov.i64           $98, 16
  add.i64           $99, $2, $98
  load.i64          $22, [$99]         # Arg 2

  mov.i64           $30, caml_start_program_helper
  mov.i64           $10, caml_apply3
  tcall.i64.c       $30, $0, $10, $20, $21, $22, $1
  .end

################################################################################
# caml_raise_exn
################################################################################
  .section .text
  .globl caml_raise_exn
caml_raise_exn:
  .call       caml
#if defined(__llir_x86_64__)
  .args       i64, i64, i64
  arg.i64     $0, 0
  arg.i64     $1, 1
  arg.i64     $4, 2

  # Load the exception pointer.
  mov.i64     $10, offset_exception_pointer
  add.i64     $11, $0, $10
  load.i64    $3, [$11]
#else
  .args       i64, i64, i64, i64, i64
  arg.i64     $0, 0
  arg.i64     $1, 1
  arg.i64     $2, 2
  arg.i64     $3, 3
  arg.i64     $4, 4

  # Store the young limit
  mov.i64     $10, offset_young_limit
  add.i64     $11, $0, $10
  store       [$11], $2
#endif

  # Store the young pointer.
  mov.i64     $10, offset_young_ptr
  add.i64     $11, $0, $10
  store       [$11], $1

  # Check if traces are active.
  mov.i64     $10, offset_backtrace_active
  add.i64     $11, $0, $10
  load.i32    $12, [$11]
  mov.i32     $13, 1
  cmp.eq.i8   $11, $12, $13
  jt          $11, .Lactive.caml_raise_exn
.Lthrow.caml_raise_exn:
  # Raise the exception here.
  mov.i64     $15, 8
  add.i64     $17, $3, $15
  add.i64     $18, $17, $15

  load.i64    $19, [$3]     # Address.
  load.i64    $20, [$17]    # Stack pointer.
  load.i64    $21, [$18]    # Next frame.

#if defined(__llir_x86_64__)
  # Store the exception pointer
  mov.i64     $10, offset_exception_pointer
  add.i64     $11, $0, $10
  store       [$11], $21
  raise.caml  $19, $20, $0, $1, $4
#else
  raise.caml  $19, $20, $0, $1, $2, $21, $4
#endif
.Lactive.caml_raise_exn:
  get.i64     $11, $ret_addr
  get.i64     $12, $frame_addr

  mov.i64     $17, caml_stash_backtrace
  call.c      $17, $4, $11, $12, $3

  jump         .Lthrow.caml_raise_exn
  .end


################################################################################
# caml_raise_exception
################################################################################
  .section .text
  .globl caml_raise_exception
caml_raise_exception:
  .call       c
  .args       i64, i64

  # Fetch the address of the state object.
  arg.i64     $100, 0
  arg.i64     $101, 1

  # Check if traces are active.
  mov.i64     $0, offset_backtrace_active
  add.i64     $0,  $0, $100
  load.i32    $1, [$0]
  mov.i32     $2, 1
  cmp.eq.i8   $3, $1, $2
  jt          $3, .Lactive.caml_raise_exception
.Lthrow.caml_raise_exception:
  # Load the frame.
  mov.i64     $14, offset_exception_pointer
  add.i64     $14, $100, $14
  mov.i64     $15, 8
  load.i64    $16, [$14]
  add.i64     $17, $16, $15
  add.i64     $18, $17, $15

  load.i64    $19, [$16]    # Address.
  load.i64    $20, [$17]    # Stack pointer.
  load.i64    $21, [$18]    # Next frame.

#ifdef __llir_x86_64__
  store       [$14], $21

  mov.i64     $10, offset_young_ptr
  add.i64     $11, $100, $10
  load.i64    $1, [$11]

  raise.caml  $19, $20, $100, $1, $101
#else
  mov.i64     $10, offset_young_ptr
  add.i64     $11, $100, $10
  load.i64    $1, [$11]

  mov.i64     $10, offset_young_limit
  add.i64     $11, $100, $10
  load.i64    $2, [$11]

  raise.caml  $19, $20, $100, $1, $2, $21, $101
#endif
.Lactive.caml_raise_exception:
  mov.i64     $11, offset_last_return_address
  add.i64     $11, $11, $100
  load.i64    $12, [$11]
  mov.i64     $13, offset_bottom_of_stack
  add.i64     $13, $13, $100
  load.i64    $14, [$13]
  mov.i64     $15, offset_exception_pointer
  add.i64     $15, $15, $100
  load.i64    $16, [$15]

  mov.i64     $17, caml_stash_backtrace
  call.c      $17, $101, $12, $14, $16

  jump        .Lthrow.caml_raise_exception
  .end


################################################################################
# caml_start_program
################################################################################
  .section .text
  .globl caml_start_program
caml_start_program:
  .call             c
  .stack_object     0, 24, 8
  .stack_object     1, 32, 8
  .args             i64

  arg.i64           $0, 0              # Caml_state
  mov.i64           $10, caml_program  # Code
  undef.i64         $20                # Arg 0
  undef.i64         $21                # Arg 1
  undef.i64         $22                # Arg 2
  undef.i64         $23                # Arg 3
  mov.i64           $30, caml_start_program_helper
  tcall.i64.c       $30, $0, $10, $20, $21, $22, $23
  .end

################################################################################
# caml_start_program_helper
################################################################################
caml_start_program_helper:
  .call                     c
  .stack_object             0, 24, 8
  .stack_object             1, 32, 8
  .args                     i64, i64, i64, i64, i64, i64
.Lentry_impl:
  # Prepare arguments.
  arg.i64                   $0, 0                 # Caml_state

  mov.i64                   $70, offset_young_ptr # Caml_state->young_ptr
  add.i64                   $19, $0, $70
  load.i64                  $1, [$19]

#ifdef __llir_x86_64__
#else
  mov.i64                   $70, offset_young_limit # Caml_state->young_limit
  add.i64                   $19, $0, $70
  load.i64                  $2, [$19]
#endif

  # Save global variables.
  frame.i64                 $40, 1, 0
  mov.i64                   $70, offset_bottom_of_stack
  add.i64                   $10, $0, $70
  load.i64                  $21, [$10]
  store                     [$40], $21

  frame.i64                 $41, 1, 8
  mov.i64                   $70, offset_last_return_address
  add.i64                   $11, $0, $70
  load.i64                  $21, [$11]
  store                     [$41], $21

  frame.i64                 $42, 1, 16
  mov.i64                   $70, offset_gc_regs
  add.i64                   $12, $0, $70
  load.i64                  $22, [$12]
  store                     [$42], $22

  frame.i64                 $43, 1, 24
  mov.i64                   $70, offset_callback_link
  add.i64                   $13, $0, $70
  load.i64                  $23, [$13]
  store                     [$43], $23

  store                     [$13], $40

  # Create an exception frame.
  frame.i64                 $51, 0, 0            # Pointer to handler.
  mov.i64                   $54, .Lhandler_impl
  store                     [$51], $54
  frame.i64                 $52, 0, 8            # Pointer to stack frame.
  get.i64                   $55, $sp
  store                     [$52], $55
  frame.i64                 $53, 0, 16           # Pointer to exn chain.
  mov.i64                   $70, offset_exception_pointer # Caml_state->exception_pointer
  add.i64                   $14, $0, $70
  load.i64                  $3, [$14]
  store                     [$53], $3
  store                     [$14], $51            # Pointer to this structure.

  # Invoke the OCaml program.
  arg.i64                   $100, 1            # Code pointer
  arg.i64                   $101, 2            # Arg 0
  arg.i64                   $102, 3            # Arg 1
  arg.i64                   $103, 4            # Arg 2
  arg.i64                   $104, 5            # Arg 3
#ifdef __llir_x86_64__
  invoke.i64.i64.i64.caml         $80, $81,           $89, $100, $0, $1,          $101, $102, $103, $104, .Lhandler_impl @caml_frame
#else
  invoke.i64.i64.i64.i64.i64.caml $80, $81, $82, $83, $89, $100, $0, $1, $2, $51, $101, $102, $103, $104, .Lhandler_impl @caml_frame
#endif

.Lreturn_impl:
  phi.i64                   $300, .Lentry_impl, $80, .Lhandler_impl, $400 # Caml_state
  phi.i64                   $301, .Lentry_impl, $81, .Lhandler_impl, $401 # Caml_state->young_ptr
#ifdef __llir_x86_64__
#else
  phi.i64                   $302, .Lentry_impl, $82, .Lhandler_impl, $402 # Caml_state->young_limit
#endif
  phi.i64                   $350, .Lentry_impl, $89, .Lhandler_impl, $450 # return value

  # Save young pointer.
  mov.i64                   $70, offset_young_ptr
  add.i64                   $200, $300, $70
  store                     [$200], $301

#ifdef __llir_x86_64__
#else
  mov.i64                   $70, offset_young_limit
  add.i64                   $200, $300, $70
  store                     [$200], $302
#endif

  # Restore global variables, popping the handler.
  load.i64                  $400, [$53]
  mov.i64                   $70, offset_exception_pointer
  add.i64                   $14, $300, $70
  store                     [$14], $400

  frame.i64                 $40, 1, 0
  load.i64                  $400, [$40]
  mov.i64                   $70, offset_bottom_of_stack
  add.i64                   $10, $300, $70
  store                     [$10], $400

  frame.i64                 $41, 1, 8
  load.i64                  $400, [$41]

  mov.i64                   $70, offset_last_return_address
  add.i64                   $11, $300, $70
  store                     [$11], $400

  frame.i64                 $42, 1, 16
  load.i64                  $400, [$42]
  mov.i64                   $70, offset_gc_regs
  add.i64                   $12, $300, $70
  store                     [$12], $400

  frame.i64                 $43, 1, 24
  load.i64                  $400, [$43]
  mov.i64                   $70, offset_callback_link
  add.i64                   $13, $300, $70
  store                     [$13], $400

  ret                       $350

.Lhandler_impl:
  # Exception handler, marks value as exception.
#ifdef __llir_x86_64__
  landing_pad.i64.i64.i64.caml          $400, $401,             $600
#else
  landing_pad.i64.i64.i64.i64.i64.caml  $400, $401, $402, $403, $600
#endif
  mov.i64                               $202, 2
  or.i64                                $450, $600, $202
  jump                                   .Lreturn_impl
  .end

################################################################################
# caml_alloc1
################################################################################
caml_alloc1:
  .visibility             global_default
  .call caml_alloc
#ifdef __llir_x86_64__
  .args                 i64, i64

  arg.i64               $0, 0       # Caml_state
  arg.i64               $1, 1       # Caml_state->young_ptr

  mov.i64               $100, 8
  add.i64               $101, $0, $100
  load.i64              $2, [$101]    # Caml_state->young_limit

  mov.i64               $102, 16
  sub.i64               $103, $1, $102

  cmp.i8.uge            $104, $103, $2
  jump_cond             $104, .Lno_gc_alloc1, .Lgc_alloc1
.Lgc_alloc1:
  mov.i64               $105, caml_call_gc
  tcall.i64.i64.caml_gc $105, $0, $103
.Lno_gc_alloc1:
  ret                   $0, $103
  .end
#else
  .args                           i64, i64, i64, i64

  arg.i64                         $0, 0       # Caml_state
  arg.i64                         $1, 1       # Caml_state->young_ptr
  arg.i64                         $2, 2       # Caml_state->young_limit
  arg.i64                         $3, 3       # Caml_state->exception_pointer

  mov.i64                         $100, 8
  add.i64                         $101, $0, $100

  mov.i64                         $102, 16
  sub.i64                         $103, $1, $102

  cmp.i8.uge                      $104, $103, $2
  jump_cond                       $104, .Lno_gc_alloc1, .Lgc_alloc1
.Lgc_alloc1:
  mov.i64                         $105, caml_call_gc
  tcall.i64.i64.i64.i64.caml_gc   $105, $0, $103, $2, $3
.Lno_gc_alloc1:
  ret                             $0, $103, $2, $3
  .end
#endif

################################################################################
# caml_alloc2
################################################################################
caml_alloc2:
  .visibility                 global_default
  .call caml_alloc
#ifdef __llir_x86_64__
  .args                 i64, i64

  arg.i64               $0, 0       # Caml_state
  arg.i64               $1, 1       # Caml_state->young_ptr

  mov.i64               $100, 8
  add.i64               $101, $0, $100
  load.i64              $2, [$101]    # Caml_state->young_limit

  mov.i64               $102, 24
  sub.i64               $103, $1, $102

  cmp.i8.uge            $104, $103, $2
  jump_cond             $104, .Lno_gc_alloc2, .Lgc_alloc2
.Lgc_alloc2:
  mov.i64               $105, caml_call_gc
  tcall.i64.i64.caml_gc $105, $0, $103
.Lno_gc_alloc2:
  ret                   $0, $103
  .end
#else
  .args                           i64, i64, i64, i64

  arg.i64                         $0, 0       # Caml_state
  arg.i64                         $1, 1       # Caml_state->young_ptr
  arg.i64                         $2, 2       # Caml_state->young_limit
  arg.i64                         $3, 3       # Caml_state->exception_pointer

  mov.i64                         $100, 8
  add.i64                         $101, $0, $100

  mov.i64                         $102, 24
  sub.i64                         $103, $1, $102

  cmp.i8.uge                      $104, $103, $2
  jump_cond                       $104, .Lno_gc_alloc2, .Lgc_alloc2
.Lgc_alloc2:
  mov.i64                         $105, caml_call_gc
  tcall.i64.i64.i64.i64.caml_gc   $105, $0, $103, $2, $3
.Lno_gc_alloc2:
  ret                             $0, $103, $2, $3
  .end
#endif

################################################################################
# caml_alloc3
################################################################################
caml_alloc3:
  .visibility                 global_default
  .call caml_alloc
#ifdef __llir_x86_64__
  .args                 i64, i64

  arg.i64               $0, 0       # Caml_state
  arg.i64               $1, 1       # Caml_state->young_ptr

  mov.i64               $100, 8
  add.i64               $101, $0, $100
  load.i64              $2, [$101]    # Caml_state->young_limit

  mov.i64               $102, 32
  sub.i64               $103, $1, $102

  cmp.i8.uge            $104, $103, $2
  jump_cond             $104, .Lno_gc_alloc3, .Lgc_alloc3
.Lgc_alloc3:
  mov.i64               $105, caml_call_gc
  tcall.i64.i64.caml_gc $105, $0, $103
.Lno_gc_alloc3:
  ret                   $0, $103
  .end
#else
  .args                           i64, i64, i64, i64

  arg.i64                         $0, 0       # Caml_state
  arg.i64                         $1, 1       # Caml_state->young_ptr
  arg.i64                         $2, 2       # Caml_state->young_limit
  arg.i64                         $3, 3       # Caml_state->exception_pointer

  mov.i64                         $100, 8
  add.i64                         $101, $0, $100

  mov.i64                         $102, 32
  sub.i64                         $103, $1, $102

  cmp.i8.uge                      $104, $103, $2
  jump_cond                       $104, .Lno_gc_alloc3, .Lgc_alloc3
.Lgc_alloc3:
  mov.i64                         $105, caml_call_gc
  tcall.i64.i64.i64.i64.caml_gc   $105, $0, $103, $2, $3
.Lno_gc_alloc3:
  ret                             $0, $103, $2, $3
  .end
#endif

################################################################################
# caml_allocN
################################################################################
caml_allocN:
  .visibility             global_default
  .call                   caml_alloc
#ifdef __llir_x86_64__
  .args                   i64, i64

  arg.i64                 $0, 0       # Caml_state
  arg.i64                 $1, 1       # Caml_state->young_ptr

  mov.i64                 $100, 8
  add.i64                 $101, $0, $100
  load.i64                $2, [$101]    # Caml_state->young_limit

  cmp.i8.uge              $102, $1, $2
  jump_cond               $102, .Lno_gc_allocN, .Lgc_allocN
.Lgc_allocN:
  mov.i64                 $103, caml_call_gc
  tcall.i64.i64.caml_gc   $103, $0, $1
.Lno_gc_allocN:
  ret                     $0, $1
  .end
#else
  .args                         i64, i64, i64, i64

  arg.i64                       $0, 0       # Caml_state
  arg.i64                       $1, 1       # Caml_state->young_ptr
  arg.i64                       $2, 2       # Caml_state->young_limit
  arg.i64                       $3, 3       # Caml_state->exception_pointer

  cmp.i8.uge                    $102, $1, $2
  jump_cond                     $102, .Lno_gc_allocN, .Lgc_allocN
.Lgc_allocN:
  mov.i64                       $103, caml_call_gc
  tcall.i64.i64.i64.i64.caml_gc $103, $0, $1, $2, $3
.Lno_gc_allocN:
  ret                           $0, $1, $2, $3
  .end
#endif

################################################################################
# caml_ml_array_bound_error
################################################################################
caml_ml_array_bound_error:
  .visibility             global_default
  .call                   caml
  .noinline
#ifdef __llir_x86_64__
  .args                   i64, i64

  arg.i64                 $0, 0     # Caml_state
  arg.i64                 $1, 1     # Caml_state->young_ptr

  mov.i64                 $100, offset_young_ptr
  add.i64                 $101, $0, $100
  store                   [$101], $1
#else
  .args                   i64, i64, i64, i64

  arg.i64                 $0, 0     # Caml_state
  arg.i64                 $1, 1     # Caml_state->young_ptr
  arg.i64                 $2, 2     # Caml_state->young_ptr
  arg.i64                 $3, 3     # Caml_state->exception_pointer

  mov.i64                 $100, offset_young_ptr
  add.i64                 $101, $0, $100
  store                   [$101], $1

  mov.i64                 $100, offset_young_limit
  add.i64                 $101, $0, $100
  store                   [$101], $2

  mov.i64                 $100, offset_exception_pointer
  add.i64                 $101, $0, $100
  store                   [$101], $3
#endif

  get.i64                 $4, $ret_addr
  mov.i64                 $5, offset_last_return_address
  add.i64                 $6, $0, $5
  store                   [$6], $4

  get.i64                 $7, $frame_addr
  mov.i64                 $8, offset_bottom_of_stack
  add.i64                 $9, $0, $8
  store                   [$9], $7

  mov.i64                 $4, caml_array_bound_error
  tcall.c                 $4
