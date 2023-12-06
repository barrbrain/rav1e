/* Copyright (c) 2023, The rav1e contributors. All rights reserved
 *
 * This source code is subject to the terms of the BSD 2 Clause License and
 * the Alliance for Open Media Patent License 1.0. If the BSD 2 Clause License
 * was not distributed with this source code in the LICENSE file, you can
 * obtain it at www.aomedia.org/license/software. If the Alliance for Open
 * Media Patent License 1.0 was not distributed with this source code in the
 * PATENTS file, you can obtain it at www.aomedia.org/license/patent.
 */

#include "src/arm/asm.S"
#include "util.S"

// x0: src: *const u8,
// x1: src_stride: isize,
// x2: dst: *const u8,
// x3: dst_stride: isize,
// x4: scale: *const u32,
// x5: scale_stride: isize,
function weighted_sse_4x4_neon, export=1
    add     x11, x1, x1
    add     x12, x3, x3
    add     x8, x1, x1, lsl 1
    add     x9, x3, x3, lsl 1
    movi    v17.4s,  #0
    mov     w10, #1
L(wsse_w4):
    ldr     s0, [x0]
    ldr     s4, [x2]
    ldr     s1, [x0, x1]
    ldr     s5, [x2, x3]
    ldr     s2, [x0, x11]
    ldr     s6, [x2, x12]
    ldr     s3, [x0, x8]
    ldr     s7, [x2, x9]
    ldr     s16, [x4]
    add     x0, x0, x1, lsl 2
    add     x2, x2, x3, lsl 2
    add     x4, x4, x5
    subs    w10, w10, #1
    mov     v0.s[1], v1.s[0]
    mov     v4.s[1], v5.s[0]
    mov     v2.s[1], v3.s[0]
    mov     v6.s[1], v7.s[0]
    uabd    v0.8b, v0.8b, v4.8b  // diff pixel values
    uabd    v1.8b, v2.8b, v6.8b
    umull   v0.8h, v0.8b, v0.8b  // square
    umull   v1.8h, v1.8b, v1.8b
    uaddl   v2.4s, v0.4h, v1.4h  // accumulate
    uaddl2  v3.4s, v0.8h, v1.8h
    add     v0.4s, v2.4s, v3.4s
    addv    s0, v0.4s
    umull   v0.2d, v0.2s, v16.2s // apply scale
    urshr   d0, d0, #8
    add     v17.2d, v17.2d, v0.2d
    bne     L(wsse_w4)
    fmov    x0, d17
    ret
endfunc

.macro weighted_sse width, height
function weighted_sse_\width\()x\height\()_neon, export=1
    add     x11, x1, x1
    add     x12, x3, x3
    add     x8, x1, x1, lsl 1
    add     x9, x3, x3, lsl 1
    movi    v17.4s,  #0
    mov     w10, #(\height / 4)
    b       L(wsse_w4)
endfunc
.endm

weighted_sse 4, 8
weighted_sse 4, 16
