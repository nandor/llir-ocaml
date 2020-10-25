/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                 Nandor Licker, University of Cambridge                 */
/*                                                                        */
/*   Copyright 2018--     Nandor Licker                                   */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

    .section .text
    .globl  call_gen_code
call_gen_codei:
    .call c
    .args                    i64, i64, i64, i64, i64
    mov.i64                  $100, Caml_state
    mov.i64                  $101, 0
    arg.i64                  $0, 0
    arg.i64                  $1, 1
    arg.i64                  $2, 2
    arg.i64                  $3, 3
    arg.i64                  $4, 4
    call.i64.i64.i32.caml    $5, $6, $7, $0, $100, $101, $1, $2, $3, $4
    ret                      $7
    .end


    .globl  call_gen_code
call_gen_codel:
    .call c
    .args            i64, i64, i64, i64, i64
    mov.i64                  $100, Caml_state
    mov.i64                  $101, 0
    arg.i64                  $0, 0
    arg.i64                  $1, 1
    arg.i64                  $2, 2
    arg.i64                  $3, 3
    arg.i64                  $4, 4
    call.i64.i64.i64.caml    $5, $6, $7, $0, $100, $101, $1, $2, $3, $4
    ret                      $7
    .end


    .globl  call_gen_code
call_gen_codef:
    .call c
    .args                    i64, i64, i64, i64, i64
    mov.i64                  $100, Caml_state
    mov.i64                  $101, 0
    arg.i64                  $0, 0
    arg.i64                  $1, 1
    arg.i64                  $2, 2
    arg.i64                  $3, 3
    arg.i64                  $4, 4
    call.i64.i64.f64.caml    $5, $6, $7, $0, $100, $101, $1, $2, $3, $4
    ret                      $7
    .end

    .globl  call_gen_code
call_gen_code:
    .call c
    .args                    i64, i64, i64, i64, i64
    mov.i64                  $100, Caml_state
    mov.i64                  $101, 0
    arg.i64                  $0, 0
    arg.i64                  $1, 1
    arg.i64                  $2, 2
    arg.i64                  $3, 3
    arg.i64                  $4, 4
    tcall.i64.i64.caml       $0, $100, $101, $1, $2, $3, $4
    .end

    .section .data
    .globl  caml_negf_mask
    .align  16
caml_negf_mask:
    .quad   0x8000000000000000
    .end

    .globl  caml_absf_mask
    .align 16
caml_absf_mask:
    .quad   0x7FFFFFFFFFFFFFFF
    .end

    .globl Caml_state
Caml_state:
    .space  416
    .end
