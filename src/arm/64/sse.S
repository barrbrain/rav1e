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

function weighted_sse_8x8_neon, export=1
    add     x11, x1, x1
    add     x12, x3, x3
    add     x8, x1, x1, lsl 1
    add     x9, x3, x3, lsl 1
    movi    v17.4s,  #0
    mov     w10, #2
L(wsse_w8):
    ldr     d0, [x0]
    ldr     d4, [x2]
    ldr     d1, [x0, x1]
    ldr     d5, [x2, x3]
    ldr     d2, [x0, x11]
    ldr     d6, [x2, x12]
    ldr     d3, [x0, x8]
    ldr     d7, [x2, x9]
    ldr     d16, [x4]
    add     x0, x0, x1, lsl 2
    add     x2, x2, x3, lsl 2
    add     x4, x4, x5
    subs    w10, w10, #1
    mov     v0.d[1], v1.d[0]
    mov     v4.d[1], v5.d[0]
    mov     v2.d[1], v3.d[0]
    mov     v6.d[1], v7.d[0]
    uabd    v0.16b, v0.16b, v4.16b  // diff pixel values
    uabd    v1.16b, v2.16b, v6.16b
    umull2  v2.8h, v0.16b, v0.16b   // square
    umull2  v3.8h, v1.16b, v1.16b
    umull   v0.8h, v0.8b, v0.8b
    umull   v1.8h, v1.8b, v1.8b
    uaddlp  v2.4s, v2.8h            // accumulate
    uaddlp  v3.4s, v3.8h
    uaddlp  v0.4s, v0.8h
    uaddlp  v1.4s, v1.8h
    uaddlp  v2.2d, v2.4s
    uadalp  v2.2d, v3.4s
    uadalp  v2.2d, v0.4s
    uadalp  v2.2d, v1.4s
    xtn     v0.2s, v2.2d
    umull   v0.2d, v0.2s, v16.2s    // apply scale
    urshr   v0.2d, v0.2d, #8
    add     v17.2d, v17.2d, v0.2d
    bne     L(wsse_w8)
    mov     v0.d[0], v17.d[1]
    add     d0, d0, d17
    fmov    x0, d0
    ret
endfunc

function weighted_sse_16x16_neon, export=1
    add     x11, x1, x1
    add     x12, x3, x3
    add     x8, x1, x1, lsl 1
    add     x9, x3, x3, lsl 1
    movi    v17.4s,  #0
    mov     w10, #4
L(wsse_w16):
    ldr     q0, [x0]
    ldr     q4, [x2]
    ldr     q1, [x0, x1]
    ldr     q5, [x2, x3]
    ldr     q2, [x0, x11]
    ldr     q6, [x2, x12]
    ldr     q3, [x0, x8]
    ldr     q7, [x2, x9]
    ldr     q16, [x4]
    add     x0, x0, x1, lsl 2
    add     x2, x2, x3, lsl 2
    add     x4, x4, x5
    subs    w10, w10, #1
    mov     v18.d[0], v16.d[1]
    uabd    v0.16b, v0.16b, v4.16b  // diff pixel values
    uabd    v1.16b, v1.16b, v5.16b
    uabd    v2.16b, v2.16b, v6.16b
    uabd    v3.16b, v3.16b, v7.16b
    umull2  v4.8h, v0.16b, v0.16b   // square
    umull2  v5.8h, v1.16b, v1.16b
    umull2  v6.8h, v2.16b, v2.16b
    umull2  v7.8h, v3.16b, v3.16b
    umull   v0.8h, v0.8b, v0.8b
    umull   v1.8h, v1.8b, v1.8b
    umull   v2.8h, v2.8b, v2.8b
    umull   v3.8h, v3.8b, v3.8b
    uaddlp  v4.4s, v4.8h            // accumulate
    uaddlp  v5.4s, v5.8h
    uaddlp  v6.4s, v6.8h
    uaddlp  v7.4s, v7.8h
    uaddlp  v0.4s, v0.8h
    uaddlp  v1.4s, v1.8h
    uaddlp  v2.4s, v2.8h
    uaddlp  v3.4s, v3.8h
    uaddlp  v4.2d, v4.4s
    uadalp  v4.2d, v5.4s
    uadalp  v4.2d, v6.4s
    uadalp  v4.2d, v7.4s
    xtn     v4.2s, v4.2d
    uaddlp  v0.2d, v0.4s
    uadalp  v0.2d, v1.4s
    uadalp  v0.2d, v2.4s
    uadalp  v0.2d, v3.4s
    xtn     v0.2s, v0.2d
    umull   v4.2d, v4.2s, v18.2s    // apply scale
    umull   v0.2d, v0.2s, v16.2s
    urshr   v4.2d, v4.2d, #8
    urshr   v0.2d, v0.2d, #8
    add     v17.2d, v17.2d, v4.2d
    add     v17.2d, v17.2d, v0.2d
    bne     L(wsse_w16)
    mov     v0.d[0], v17.d[1]
    add     d0, d0, d17
    fmov    x0, d0
    ret
endfunc

function weighted_sse_32x32_neon, export=1
    movi    v17.4s,  #0
    mov     w10, #8
L(wsse_w32):
    ldp     q0, q22, [x0]
    ldp     q4, q26, [x2]
    add     x0, x0, x1
    add     x2, x2, x3
    ldp     q1, q23, [x0]
    ldp     q5, q27, [x2]
    add     x0, x0, x1
    add     x2, x2, x3
    ldp     q2, q24, [x0]
    ldp     q6, q28, [x2]
    add     x0, x0, x1
    add     x2, x2, x3
    ldp     q3, q25, [x0]
    ldp     q7, q29, [x2]
    add     x0, x0, x1
    add     x2, x2, x3
    ldp     q16, q19, [x4]
    add     x4, x4, x5
    subs    w10, w10, #1
    mov     v18.d[0], v16.d[1]
    mov     v20.d[0], v19.d[1]
    uabd    v0.16b, v0.16b, v4.16b     // diff pixel values
    uabd    v1.16b, v1.16b, v5.16b
    uabd    v2.16b, v2.16b, v6.16b
    uabd    v3.16b, v3.16b, v7.16b
    uabd    v22.16b, v22.16b, v26.16b
    uabd    v23.16b, v23.16b, v27.16b
    uabd    v24.16b, v24.16b, v28.16b
    uabd    v25.16b, v25.16b, v29.16b
    umull2  v4.8h, v0.16b, v0.16b      // square
    umull2  v5.8h, v1.16b, v1.16b
    umull2  v6.8h, v2.16b, v2.16b
    umull2  v7.8h, v3.16b, v3.16b
    umull2  v26.8h, v22.16b, v22.16b
    umull2  v27.8h, v23.16b, v23.16b
    umull2  v28.8h, v24.16b, v24.16b
    umull2  v29.8h, v25.16b, v25.16b
    umull   v0.8h, v0.8b, v0.8b
    umull   v1.8h, v1.8b, v1.8b
    umull   v2.8h, v2.8b, v2.8b
    umull   v3.8h, v3.8b, v3.8b
    umull   v22.8h, v22.8b, v22.8b
    umull   v23.8h, v23.8b, v23.8b
    umull   v24.8h, v24.8b, v24.8b
    umull   v25.8h, v25.8b, v25.8b
    uaddlp  v4.4s, v4.8h               // accumulate
    uadalp  v4.4s, v5.8h
    uadalp  v4.4s, v6.8h
    uadalp  v4.4s, v7.8h
    uaddlp  v26.4s, v26.8h
    uadalp  v26.4s, v27.8h
    uadalp  v26.4s, v28.8h
    uadalp  v26.4s, v29.8h
    uaddlp  v0.4s, v0.8h
    uadalp  v0.4s, v1.8h
    uadalp  v0.4s, v2.8h
    uadalp  v0.4s, v3.8h
    uaddlp  v22.4s, v22.8h
    uadalp  v22.4s, v23.8h
    uadalp  v22.4s, v24.8h
    uadalp  v22.4s, v25.8h
    uaddlp  v4.2d, v4.4s
    uaddlp  v26.2d, v26.4s
    uaddlp  v0.2d, v0.4s
    uaddlp  v22.2d, v22.4s
    xtn     v4.2s, v4.2d
    xtn     v26.2s, v26.2d
    xtn     v0.2s, v0.2d
    xtn     v22.2s, v22.2d
    umull   v4.2d, v4.2s, v18.2s       // apply scale
    umull   v26.2d, v26.2s, v20.2s
    umull   v0.2d, v0.2s, v16.2s
    umull   v22.2d, v22.2s, v19.2s
    urshr   v4.2d, v4.2d, #8
    urshr   v26.2d, v26.2d, #8
    urshr   v0.2d, v0.2d, #8
    urshr   v22.2d, v22.2d, #8
    add     v4.2d, v4.2d, v26.2d
    add     v0.2d, v0.2d, v22.2d
    add     v17.2d, v17.2d, v4.2d
    add     v17.2d, v17.2d, v0.2d
    bne     L(wsse_w32)
    mov     v0.d[0], v17.d[1]
    add     d0, d0, d17
    fmov    x0, d0
    ret
endfunc

function weighted_sse_64x64_neon, export=1
    mov     w8, #64
    sxtw    x9, w8
    movi    v17.4s,  #0
    mov     w10, #16
L(wsse_w32up):
    ldp     q0, q22, [x0]
    ldp     q4, q26, [x2]
    add     x0, x0, x1
    add     x2, x2, x3
    ldp     q1, q23, [x0]
    ldp     q5, q27, [x2]
    add     x0, x0, x1
    add     x2, x2, x3
    ldp     q2, q24, [x0]
    ldp     q6, q28, [x2]
    add     x0, x0, x1
    add     x2, x2, x3
    ldp     q3, q25, [x0]
    ldp     q7, q29, [x2]
    add     x0, x0, x1
    add     x2, x2, x3
    sub     x0, x0, x1, lsl 2
    sub     x2, x2, x3, lsl 2
    add     x0, x0, #32
    add     x2, x2, #32
    ldp     q16, q19, [x4]
    add     x4, x4, #32
    mov     v18.d[0], v16.d[1]
    mov     v20.d[0], v19.d[1]
    uabd    v0.16b, v0.16b, v4.16b     // diff pixel values
    uabd    v1.16b, v1.16b, v5.16b
    uabd    v2.16b, v2.16b, v6.16b
    uabd    v3.16b, v3.16b, v7.16b
    uabd    v22.16b, v22.16b, v26.16b
    uabd    v23.16b, v23.16b, v27.16b
    uabd    v24.16b, v24.16b, v28.16b
    uabd    v25.16b, v25.16b, v29.16b
    umull2  v4.8h, v0.16b, v0.16b      // square
    umull2  v5.8h, v1.16b, v1.16b
    umull2  v6.8h, v2.16b, v2.16b
    umull2  v7.8h, v3.16b, v3.16b
    umull2  v26.8h, v22.16b, v22.16b
    umull2  v27.8h, v23.16b, v23.16b
    umull2  v28.8h, v24.16b, v24.16b
    umull2  v29.8h, v25.16b, v25.16b
    umull   v0.8h, v0.8b, v0.8b
    umull   v1.8h, v1.8b, v1.8b
    umull   v2.8h, v2.8b, v2.8b
    umull   v3.8h, v3.8b, v3.8b
    umull   v22.8h, v22.8b, v22.8b
    umull   v23.8h, v23.8b, v23.8b
    umull   v24.8h, v24.8b, v24.8b
    umull   v25.8h, v25.8b, v25.8b
    uaddlp  v4.4s, v4.8h               // accumulate
    uadalp  v4.4s, v5.8h
    uadalp  v4.4s, v6.8h
    uadalp  v4.4s, v7.8h
    uaddlp  v26.4s, v26.8h
    uadalp  v26.4s, v27.8h
    uadalp  v26.4s, v28.8h
    uadalp  v26.4s, v29.8h
    uaddlp  v0.4s, v0.8h
    uadalp  v0.4s, v1.8h
    uadalp  v0.4s, v2.8h
    uadalp  v0.4s, v3.8h
    uaddlp  v22.4s, v22.8h
    uadalp  v22.4s, v23.8h
    uadalp  v22.4s, v24.8h
    uadalp  v22.4s, v25.8h
    uaddlp  v4.2d, v4.4s
    uaddlp  v26.2d, v26.4s
    uaddlp  v0.2d, v0.4s
    uaddlp  v22.2d, v22.4s
    xtn     v4.2s, v4.2d
    xtn     v26.2s, v26.2d
    xtn     v0.2s, v0.2d
    xtn     v22.2s, v22.2d
    umull   v4.2d, v4.2s, v18.2s       // apply scale
    umull   v26.2d, v26.2s, v20.2s
    umull   v0.2d, v0.2s, v16.2s
    umull   v22.2d, v22.2s, v19.2s
    urshr   v4.2d, v4.2d, #8
    urshr   v26.2d, v26.2d, #8
    urshr   v0.2d, v0.2d, #8
    urshr   v22.2d, v22.2d, #8
    add     v4.2d, v4.2d, v26.2d
    add     v0.2d, v0.2d, v22.2d
    add     v17.2d, v17.2d, v4.2d
    add     v17.2d, v17.2d, v0.2d
    subs    w8, w8, #32
    bne     L(wsse_w32up)
    mov     w8, w9
    sub     x0, x0, x9
    sub     x2, x2, x9
    add     x0, x0, x1, lsl 2
    add     x2, x2, x3, lsl 2
    sub     x4, x4, x9
    add     x4, x4, x5
    subs    w10, w10, #1
    bne     L(wsse_w32up)
    mov     v0.d[0], v17.d[1]
    add     d0, d0, d17
    fmov    x0, d0
    ret
endfunc

.macro weighted_sse width, height
function weighted_sse_\width\()x\height\()_neon, export=1
.if \width <= 16
    add     x11, x1, x1
    add     x12, x3, x3
    add     x8, x1, x1, lsl 1
    add     x9, x3, x3, lsl 1
.elseif \width >= 64
    mov     w8, #(\width)
    sxtw    x9, w8
.endif
    movi    v17.4s,  #0
    mov     w10, #(\height / 4)
.if \width == 4
    b       L(wsse_w4)
.elseif \width == 8
    b       L(wsse_w8)
.elseif \width == 16
    b       L(wsse_w16)
.elseif \width == 32
    b       L(wsse_w32)
.else
    b       L(wsse_w32up)
.endif
endfunc
.endm

weighted_sse 4, 8
weighted_sse 4, 16
weighted_sse 8, 4
weighted_sse 8, 16
weighted_sse 8, 32
weighted_sse 16, 4
weighted_sse 16, 8
weighted_sse 16, 32
weighted_sse 16, 64
weighted_sse 32, 8
weighted_sse 32, 16
weighted_sse 32, 64
weighted_sse 64, 16
weighted_sse 64, 32
weighted_sse 64, 128
weighted_sse 128, 64
weighted_sse 128, 128
