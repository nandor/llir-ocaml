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
#define offset_exception_value      24
#define offset_bottom_of_stack      208
#define offset_last_return_address  216
#define offset_gc_regs              224
#define offset_backtrace_active     232
#define offset_backtrace_pos        240
#define offset_callback_link        408



################################################################################
# caml_callback_asm
################################################################################
  .text
  .globl caml_callback_asm
caml_callback_asm:
  .call             c
  .args             i64, i64, i64

  arg.i64           $0, 0              # Caml_state
  arg.i64           $1, 1              # closure
  arg.i64           $2, 2              # args
  ld.i64            $10, [$1]          # Code
  ld.i64            $20, [$2]          # Arg 0
  undef.i64         $22                # Arg 2
  undef.i64         $23                # Arg 3
  mov.i64           $30, caml_start_program_helper
  tcall.i64.c       $30, $0, $10, $20, $1, $22, $23

################################################################################
# caml_callback2_asm
################################################################################
  .text
  .globl caml_callback2_asm
caml_callback2_asm:
  .call             c
  .args             i64, i64, i64

  arg.i64           $0, 0              # Caml_state
  arg.i64           $1, 1              # closure
  arg.i64           $2, 2              # args

  ld.i64            $20, [$2]          # Arg 0

  mov.i64           $98, 8
  add.i64           $99, $2, $98
  ld.i64            $21, [$99]         # Arg 1

  undef.i64         $23                # Arg 3
  mov.i64           $30, caml_start_program_helper
  mov.i64           $10, caml_apply2
  tcall.i64.c       $30, $0, $10, $20, $21, $1, $23

################################################################################
# caml_callback3_asm
################################################################################
  .text
  .globl caml_callback3_asm
caml_callback3_asm:
  .call             c
  .args             i64, i64, i64
  arg.i64           $0, 0              # Caml_state
  arg.i64           $1, 1              # closure
  arg.i64           $2, 2              # args

  ld.i64            $20, [$2]          # Arg 0

  mov.i64           $98, 8
  add.i64           $99, $2, $98
  ld.i64            $21, [$99]         # Arg 1

  mov.i64           $98, 16
  add.i64           $99, $2, $98
  ld.i64            $22, [$99]         # Arg 2

  mov.i64           $30, caml_start_program_helper
  mov.i64           $10, caml_apply3
  tcall.i64.c       $30, $0, $10, $20, $21, $22, $1

################################################################################
# caml_raise_exn
################################################################################
  .text
  .globl caml_raise_exn
caml_raise_exn:
  .call       c
  .args       i64

  # Fetch the address of the state object.
  mov.i64     $101, Caml_state
  ld.i64      $100, [$101]

  # Check if traces are active.
  mov.i64     $0, offset_backtrace_active
  add.i64     $0, $0, $100
  ld.4.i32    $1, [$0]
  mov.i32     $2, 1
  cmp.eq.i8   $3, $1, $2
  jt          $3, .Lactive.caml_raise_exn
.Lthrow.caml_raise_exn:
  # Raise the exception here.
  arg.i64     $10, 0
  mov.i64     $11, offset_exception_value
  add.i64     $11, $11, $100
  st          [$11], $10

  # Load the frame.
  mov.i64     $14, offset_exception_pointer
  add.i64     $14, $14, $100
  mov.i64     $15, 8
  ld.i64      $16, [$14]
  add.i64     $17, $16, $15
  add.i64     $18, $17, $15

  ld.i64      $19, [$16]    # Address.
  ld.i64      $20, [$17]    # Stack pointer.
  ld.i64      $21, [$18]    # Next frame.

  # Jump.
  set.i64     $rsp, $20
  st          [$14], $21
  ji          $19
.Lactive.caml_raise_exn:
  arg.i64     $10, 0

  mov.i64     $11, $ret_addr
  mov.i64     $12, $frame_addr
  mov.i64     $13, offset_exception_pointer
  add.i64     $13, $13, $100
  ld.i64      $14, [$13]

  mov.i64     $17, caml_stash_backtrace
  call.c      $17, $10, $11, $12, $14

  jmp         .Lthrow.caml_raise_exn


################################################################################
# caml_raise_exception
################################################################################
  .text
  .globl caml_raise_exception
caml_raise_exception:
  .call       c
  .args       i64, i64

  # Fetch the address of the state object.
  arg.i64     $100, 0

  # Check if traces are active.
  mov.i64     $0, offset_backtrace_active
  add.i64     $0,  $0, $100
  ld.4.i32    $1, [$0]
  mov.i32     $2, 1
  cmp.eq.i8   $3, $1, $2
  jt          $3, .Lactive.caml_raise_exception
.Lthrow.caml_raise_exception:
  # Raise the exception here.
  arg.i64     $10, 1
  mov.i64     $11, offset_exception_value
  add.i64     $11, $11, $100
  st          [$11], $10

  # Load the frame.
  mov.i64     $14, offset_exception_pointer
  add.i64     $14, $14, $100
  mov.i64     $15, 8
  ld.i64      $16, [$14]
  add.i64     $17, $16, $15
  add.i64     $18, $17, $15

  ld.i64      $19, [$16]    # Address.
  ld.i64      $20, [$17]    # Stack pointer.
  ld.i64      $21, [$18]    # Next frame.

  # Jump.
  set.i64     $rsp, $20
  st          [$14], $21
  ji          $19
.Lactive.caml_raise_exception:
  arg.i64     $10, 1

  mov.i64     $11, offset_last_return_address
  add.i64     $11, $11, $100
  ld.i64      $12, [$11]
  mov.i64     $13, offset_bottom_of_stack
  add.i64     $13, $13, $100
  ld.i64      $14, [$13]
  mov.i64     $15, offset_exception_pointer
  add.i64     $15, $15, $100
  ld.i64      $16, [$15]

  mov.i64     $17, caml_stash_backtrace
  call.c      $17, $10, $12, $14, $16

  jmp .Lthrow.caml_raise_exception


################################################################################
# caml_start_program
################################################################################
  .text
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

################################################################################
# caml_start_program_helper
################################################################################
caml_start_program_helper:
  .call             c
  .stack_object     0, 24, 8
  .stack_object     1, 32, 8
  .args             i64, i64, i64, i64, i64, i64
.Lentry_impl:
  # Save global variables.
  arg.i64           $999, 0         # Caml_state

  frame.i64         $40, 1, 0
  mov.i64           $70, offset_bottom_of_stack
  add.i64           $10, $999, $70
  ld.i64            $21, [$10]
  st                [$40], $21

  frame.i64         $41, 1, 8
  mov.i64           $70, offset_last_return_address
  add.i64           $11, $999, $70
  ld.i64            $21, [$11]
  st                [$41], $21

  frame.i64         $42, 1, 16
  mov.i64           $70, offset_gc_regs
  add.i64           $12, $999, $70
  ld.i64            $22, [$12]
  st                [$42], $22

  frame.i64         $43, 1, 24
  mov.i64           $70, offset_callback_link
  add.i64           $13, $999, $70
  ld.i64            $23, [$13]
  st                [$43], $23

  st                [$13], $40

  # Create an exception frame.
  frame.i64         $51, 0, 0            # Pointer to handler.
  mov.i64           $54, .Lhandler_impl
  st                [$51], $54
  frame.i64         $52, 0, 8            # Pointer to stack frame.
  mov.i64           $55, $rsp
  st                [$52], $55
  frame.i64         $53, 0, 16           # Pointer to exn chain.
  mov.i64           $70, offset_exception_pointer
  add.i64           $6, $999, $70
  ld.i64            $56, [$6]
  st                [$53], $56
  st                [$6], $51            # Pointer to this structure.

  # Invoke the OCaml program.
  arg.i64           $90, 1            # Code pointer
  arg.i64           $91, 2            # Arg 0
  arg.i64           $92, 3            # Arg 1
  arg.i64           $93, 4            # Arg 2
  arg.i64           $94, 5            # Arg 3
  invoke.i64.caml   $80, $90, $91, $92, $93, $94, .Lhandler_impl @caml_frame

.Lreturn_impl:
  phi.i64           $300, .Lentry_impl, $80, .Lhandler_impl, $203

  # Restore global variables, popping the handler.
  ld.i64            $400, [$53]
  st                [$6], $400

  ld.i64            $400, [$40]
  st                [$10], $400

  ld.i64            $400, [$41]
  st                [$11], $400

  ld.i64            $400, [$42]
  st                [$12], $400

  ld.i64            $400, [$43]
  st                [$13], $400

  ret.i64           $300

.Lhandler_impl:
  # Exception handler, marks value as exception.
  mov.i64           $70, offset_exception_value
  add.i64           $200, $999, $70
  ld.i64            $201, [$200]
  mov.i64           $202, 2
  or.i64            $203, $201, $202
  jmp               .Lreturn_impl

################################################################################
# caml_stack_overflow
################################################################################
  .text
  .globl caml_stack_overflow
caml_stack_overflow:
  trap
