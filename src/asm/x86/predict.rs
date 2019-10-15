// Copyright (c) 2019, The rav1e contributors. All rights reserved
//
// This source code is subject to the terms of the BSD 2 Clause License and
// the Alliance for Open Media Patent License 1.0. If the BSD 2 Clause License
// was not distributed with this source code in the LICENSE file, you can
// obtain it at www.aomedia.org/license/software. If the Alliance for Open
// Media Patent License 1.0 was not distributed with this source code in the
// PATENTS file, you can obtain it at www.aomedia.org/license/patent.

use crate::cpu_features::CpuFeatureLevel;
use crate::predict::{self, native};
use crate::tiling::PlaneRegionMut;
use crate::util::AlignedArray;
use crate::Pixel;
use libc;
#[cfg(target_arch = "x86")]
use std::arch::x86::*;
#[cfg(target_arch = "x86_64")]
use std::arch::x86_64::*;
use std::mem::size_of;
use std::ptr;

macro_rules! decl_angular_ipred_fn {
  ($($f:ident),+) => {
    extern {
      $(
        fn $f(
          dst: *mut u8, stride: libc::ptrdiff_t, topleft: *const u8,
          width: libc::c_int, height: libc::c_int, angle: libc::c_int,
        );
      )*
    }
  };
}

decl_angular_ipred_fn! {
  rav1e_ipred_dc_avx2,
  rav1e_ipred_dc_128_avx2,
  rav1e_ipred_dc_left_avx2,
  rav1e_ipred_dc_top_avx2,
  rav1e_ipred_h_avx2,
  rav1e_ipred_h_ssse3,
  rav1e_ipred_v_avx2,
  rav1e_ipred_paeth_avx2,
  rav1e_ipred_smooth_avx2,
  rav1e_ipred_smooth_h_avx2,
  rav1e_ipred_smooth_v_avx2
}

macro_rules! decl_cfl_pred_fn {
  ($($f:ident),+) => {
    extern {
      $(
        fn $f(
          dst: *mut u8, stride: libc::ptrdiff_t, topleft: *const u8,
          width: libc::c_int, height: libc::c_int, ac: *const u8,
          alpha: libc::c_int,
        );
      )*
    }
  };
}

decl_cfl_pred_fn! {
  rav1e_ipred_cfl_avx2,
  rav1e_ipred_cfl_128_avx2,
  rav1e_ipred_cfl_left_avx2,
  rav1e_ipred_cfl_top_avx2
}

pub trait Intra<T>: native::Intra<T>
where
  T: Pixel,
{
  fn pred_dc(
    output: &mut PlaneRegionMut<'_, T>, above: &[T], left: &[T],
    cpu: CpuFeatureLevel,
  ) {
    if size_of::<T>() == 1 && cpu >= CpuFeatureLevel::AVX2 {
      return unsafe {
        rav1e_ipred_dc_avx2(
          output.data_ptr_mut() as *mut _,
          output.plane_cfg.stride as libc::ptrdiff_t,
          above.as_ptr().offset(-1) as *const _,
          Self::W as libc::c_int,
          Self::H as libc::c_int,
          0,
        )
      };
    }

    <Self as native::Intra<T>>::pred_dc(output, above, left, cpu)
  }

  fn pred_dc_128(
    output: &mut PlaneRegionMut<'_, T>, bit_depth: usize, cpu: CpuFeatureLevel,
  ) {
    if size_of::<T>() == 1 && cpu >= CpuFeatureLevel::AVX2 {
      return unsafe {
        rav1e_ipred_dc_128_avx2(
          output.data_ptr_mut() as *mut _,
          output.plane_cfg.stride as libc::ptrdiff_t,
          ptr::null(),
          Self::W as libc::c_int,
          Self::H as libc::c_int,
          0,
        )
      };
    }

    <Self as native::Intra<T>>::pred_dc_128(output, bit_depth, cpu)
  }

  fn pred_dc_left(
    output: &mut PlaneRegionMut<'_, T>, _above: &[T], left: &[T],
    cpu: CpuFeatureLevel,
  ) {
    if size_of::<T>() == 1 && cpu >= CpuFeatureLevel::AVX2 {
      return unsafe {
        rav1e_ipred_dc_left_avx2(
          output.data_ptr_mut() as *mut _,
          output.plane_cfg.stride as libc::ptrdiff_t,
          left.as_ptr().add(Self::H) as *const _,
          Self::W as libc::c_int,
          Self::H as libc::c_int,
          0,
        )
      };
    }

    <Self as native::Intra<T>>::pred_dc_left(output, _above, left, cpu)
  }

  fn pred_dc_top(
    output: &mut PlaneRegionMut<'_, T>, above: &[T], _left: &[T],
    cpu: CpuFeatureLevel,
  ) {
    if size_of::<T>() == 1 && cpu >= CpuFeatureLevel::AVX2 {
      return unsafe {
        rav1e_ipred_dc_top_avx2(
          output.data_ptr_mut() as *mut _,
          output.plane_cfg.stride as libc::ptrdiff_t,
          above.as_ptr().offset(-1) as *const _,
          Self::W as libc::c_int,
          Self::H as libc::c_int,
          0,
        )
      };
    }

    <Self as native::Intra<T>>::pred_dc_top(output, above, _left, cpu)
  }

  fn pred_h(
    output: &mut PlaneRegionMut<'_, T>, left: &[T], cpu: CpuFeatureLevel,
  ) {
    if size_of::<T>() == 1 && cpu >= CpuFeatureLevel::SSSE3 {
      return unsafe {
        (if cpu >= CpuFeatureLevel::AVX2 {
          rav1e_ipred_h_avx2
        } else {
          rav1e_ipred_h_ssse3
        })(
          output.data_ptr_mut() as *mut _,
          output.plane_cfg.stride as libc::ptrdiff_t,
          left.as_ptr().add(Self::H) as *const _,
          Self::W as libc::c_int,
          Self::H as libc::c_int,
          0,
        )
      };
    }

    <Self as native::Intra<T>>::pred_h(output, left, cpu)
  }

  fn pred_v(
    output: &mut PlaneRegionMut<'_, T>, above: &[T], cpu: CpuFeatureLevel,
  ) {
    if size_of::<T>() == 1 && cpu >= CpuFeatureLevel::AVX2 {
      return unsafe {
        rav1e_ipred_v_avx2(
          output.data_ptr_mut() as *mut _,
          output.plane_cfg.stride as libc::ptrdiff_t,
          above.as_ptr().offset(-1) as *const _,
          Self::W as libc::c_int,
          Self::H as libc::c_int,
          0,
        )
      };
    }

    <Self as native::Intra<T>>::pred_v(output, above, cpu)
  }

  fn pred_paeth(
    output: &mut PlaneRegionMut<'_, T>, above: &[T], left: &[T],
    above_left: T, cpu: CpuFeatureLevel,
  ) {
    if size_of::<T>() == 1 && cpu >= CpuFeatureLevel::AVX2 {
      return unsafe {
        rav1e_ipred_paeth_avx2(
          output.data_ptr_mut() as *mut _,
          output.plane_cfg.stride as libc::ptrdiff_t,
          above.as_ptr().offset(-1) as *const _,
          Self::W as libc::c_int,
          Self::H as libc::c_int,
          0,
        )
      };
    }

    <Self as native::Intra<T>>::pred_paeth(
      output, above, left, above_left, cpu,
    )
  }

  fn pred_smooth(
    output: &mut PlaneRegionMut<'_, T>, above: &[T], left: &[T],
    cpu: CpuFeatureLevel,
  ) {
    if size_of::<T>() == 1 && cpu >= CpuFeatureLevel::AVX2 {
      return unsafe {
        rav1e_ipred_smooth_avx2(
          output.data_ptr_mut() as *mut _,
          output.plane_cfg.stride as libc::ptrdiff_t,
          above.as_ptr().offset(-1) as *const _,
          Self::W as libc::c_int,
          Self::H as libc::c_int,
          0,
        )
      };
    }

    <Self as native::Intra<T>>::pred_smooth(output, above, left, cpu)
  }

  fn pred_smooth_h(
    output: &mut PlaneRegionMut<'_, T>, above: &[T], left: &[T],
    cpu: CpuFeatureLevel,
  ) {
    if size_of::<T>() == 1 && cpu >= CpuFeatureLevel::AVX2 {
      return unsafe {
        rav1e_ipred_smooth_h_avx2(
          output.data_ptr_mut() as *mut _,
          output.plane_cfg.stride as libc::ptrdiff_t,
          above.as_ptr().offset(-1) as *const _,
          Self::W as libc::c_int,
          Self::H as libc::c_int,
          0,
        )
      };
    }

    <Self as native::Intra<T>>::pred_smooth_h(output, above, left, cpu)
  }

  fn pred_smooth_v(
    output: &mut PlaneRegionMut<'_, T>, above: &[T], left: &[T],
    cpu: CpuFeatureLevel,
  ) {
    if size_of::<T>() == 1 && cpu >= CpuFeatureLevel::AVX2 {
      return unsafe {
        rav1e_ipred_smooth_v_avx2(
          output.data_ptr_mut() as *mut _,
          output.plane_cfg.stride as libc::ptrdiff_t,
          above.as_ptr().offset(-1) as *const _,
          Self::W as libc::c_int,
          Self::H as libc::c_int,
          0,
        )
      };
    }

    <Self as native::Intra<T>>::pred_smooth_v(output, above, left, cpu)
  }

  #[target_feature(enable = "ssse3")]
  unsafe fn pred_cfl_ssse3(
    output: *mut T, stride: usize, ac: *const i16, alpha: i16,
    bit_depth: usize,
  ) {
    let alpha_sign = _mm_set1_epi16(alpha);
    let alpha_q12 = _mm_slli_epi16(_mm_abs_epi16(alpha_sign), 9);
    let dc_scalar: u32 = (*output).into();
    let dc_q0 = _mm_set1_epi16(dc_scalar as i16);
    let max = _mm_set1_epi16((1 << bit_depth) - 1);

    for j in 0..Self::H {
      let luma = ac.add(Self::W * j);
      let line = output.add(stride * j);

      let mut i = 0isize;
      let mut last = _mm_setzero_si128();
      while (i as usize) < Self::W {
        let ac_q3 = _mm_loadu_si128(luma.offset(i) as *const _);
        let ac_sign = _mm_sign_epi16(alpha_sign, ac_q3);
        let abs_scaled_luma_q0 =
          _mm_mulhrs_epi16(_mm_abs_epi16(ac_q3), alpha_q12);
        let scaled_luma_q0 = _mm_sign_epi16(abs_scaled_luma_q0, ac_sign);
        let pred = _mm_add_epi16(scaled_luma_q0, dc_q0);
        if size_of::<T>() == 1 {
          if Self::W < 16 {
            let res = _mm_packus_epi16(pred, pred);
            if Self::W == 4 {
              *(line.offset(i) as *mut i32) = _mm_cvtsi128_si32(res);
            } else {
              _mm_storel_epi64(line.offset(i) as *mut _, res);
            }
          } else if (i & 15) == 0 {
            last = pred;
          } else {
            let res = _mm_packus_epi16(last, pred);
            _mm_storeu_si128(line.offset(i - 8) as *mut _, res);
          }
        } else {
          let res =
            _mm_min_epi16(max, _mm_max_epi16(pred, _mm_setzero_si128()));
          if Self::W == 4 {
            _mm_storel_epi64(line.offset(i) as *mut _, res);
          } else {
            _mm_storeu_si128(line.offset(i) as *mut _, res);
          }
        }
        i += 8;
      }
    }
  }

  fn pred_cfl_inner(
    output: &mut PlaneRegionMut<'_, T>, ac: &[i16], alpha: i16,
    bit_depth: usize, cpu: CpuFeatureLevel,
  ) {
    if alpha == 0 {
      return;
    }
    assert!(32 >= Self::W);
    assert!(ac.len() >= 32 * (Self::H - 1) + Self::W);
    assert!(output.plane_cfg.stride >= Self::W);
    assert!(output.rows_iter().len() >= Self::H);

    if cpu >= CpuFeatureLevel::SSSE3 {
      return unsafe {
        Self::pred_cfl_ssse3(
          output.data_ptr_mut(),
          output.plane_cfg.stride,
          ac.as_ptr(),
          alpha,
          bit_depth,
        )
      };
    }

    <Self as Intra<T>>::pred_cfl_inner(output, &ac, alpha, bit_depth, cpu);
  }

  fn pred_cfl(
    output: &mut PlaneRegionMut<'_, T>, ac: &[i16], alpha: i16,
    bit_depth: usize, above: &[T], left: &[T], cpu: CpuFeatureLevel,
  ) {
    {
      if size_of::<T>() == 1 && cpu >= CpuFeatureLevel::AVX2 {
        return unsafe {
          rav1e_ipred_cfl_avx2(
            output.data_ptr_mut() as *mut _,
            output.plane_cfg.stride as libc::ptrdiff_t,
            above.as_ptr().offset(-1) as *const _,
            Self::W as libc::c_int,
            Self::H as libc::c_int,
            ac.as_ptr() as *const _,
            alpha as libc::c_int,
          )
        };
      }
    }
    <Self as Intra<T>>::pred_dc(output, above, left, cpu);
    <Self as Intra<T>>::pred_cfl_inner(output, &ac, alpha, bit_depth, cpu);
  }

  fn pred_cfl_128(
    output: &mut PlaneRegionMut<'_, T>, ac: &[i16], alpha: i16,
    bit_depth: usize, cpu: CpuFeatureLevel,
  ) {
    {
      if size_of::<T>() == 1 && cpu >= CpuFeatureLevel::AVX2 {
        return unsafe {
          rav1e_ipred_cfl_128_avx2(
            output.data_ptr_mut() as *mut _,
            output.plane_cfg.stride as libc::ptrdiff_t,
            ptr::null(),
            Self::W as libc::c_int,
            Self::H as libc::c_int,
            ac.as_ptr() as *const _,
            alpha as libc::c_int,
          )
        };
      }
    }
    <Self as Intra<T>>::pred_dc_128(output, bit_depth, cpu);
    <Self as Intra<T>>::pred_cfl_inner(output, &ac, alpha, bit_depth, cpu);
  }

  fn pred_cfl_left(
    output: &mut PlaneRegionMut<'_, T>, ac: &[i16], alpha: i16,
    bit_depth: usize, above: &[T], left: &[T], cpu: CpuFeatureLevel,
  ) {
    {
      if size_of::<T>() == 1 && cpu >= CpuFeatureLevel::AVX2 {
        return unsafe {
          rav1e_ipred_cfl_left_avx2(
            output.data_ptr_mut() as *mut _,
            output.plane_cfg.stride as libc::ptrdiff_t,
            above.as_ptr().offset(-1) as *const _,
            Self::W as libc::c_int,
            Self::H as libc::c_int,
            ac.as_ptr() as *const _,
            alpha as libc::c_int,
          )
        };
      }
    }
    <Self as Intra<T>>::pred_dc_left(output, above, left, cpu);
    <Self as Intra<T>>::pred_cfl_inner(output, &ac, alpha, bit_depth, cpu);
  }

  fn pred_cfl_top(
    output: &mut PlaneRegionMut<'_, T>, ac: &[i16], alpha: i16,
    bit_depth: usize, above: &[T], left: &[T], cpu: CpuFeatureLevel,
  ) {
    {
      if size_of::<T>() == 1 && cpu >= CpuFeatureLevel::AVX2 {
        return unsafe {
          rav1e_ipred_cfl_top_avx2(
            output.data_ptr_mut() as *mut _,
            output.plane_cfg.stride as libc::ptrdiff_t,
            above.as_ptr().offset(-1) as *const _,
            Self::W as libc::c_int,
            Self::H as libc::c_int,
            ac.as_ptr() as *const _,
            alpha as libc::c_int,
          )
        };
      }
    }
    <Self as Intra<T>>::pred_dc_top(output, above, left, cpu);
    <Self as Intra<T>>::pred_cfl_inner(output, &ac, alpha, bit_depth, cpu);
  }
}

macro_rules! block_intra_asm_impl {
  ($W:expr, $H:expr) => {
    paste::item! {
      impl<T: Pixel> Intra<T> for predict::[<Block $W x $H>] {}
    }
  };
}

macro_rules! blocks_intra_asm_impl {
  ($(($W:expr, $H:expr)),+) => {
    $(
      block_intra_asm_impl! { $W, $H }
    )*
  }
}

blocks_intra_asm_impl! { (4, 4), (8, 8), (16, 16), (32, 32), (64, 64) }
blocks_intra_asm_impl! { (4, 8), (8, 16), (16, 32), (32, 64) }
blocks_intra_asm_impl! { (8, 4), (16, 8), (32, 16), (64, 32) }
blocks_intra_asm_impl! { (4, 16), (8, 32), (16, 64) }
blocks_intra_asm_impl! { (16, 4), (32, 8), (64, 16) }

#[rustfmt::skip]
static FILTER_INTRA_TAPS: AlignedArray<[[i8; 64]; 5]> = AlignedArray::new([
  [ -6,  10,  -5,   2,  -3,   1,  -3,   1,
    -4,   6,  -3,   2,  -3,   2,  -3,   1,
     0,   0,  10,   0,   1,  10,   1,   2,
     0,   0,   6,   0,   2,   6,   2,   2,
     0,  12,   0,   9,   0,   7,  10,   5,
     0,   2,   0,   2,   0,   2,   6,   3,
     0,   0,   0,   0,   0,   0,   0,   0,
    12,   0,   9,   0,   7,   0,   5,   0],
  [-10,  16,  -6,   0,  -4,   0,  -2,   0,
   -10,  16,  -6,   0,  -4,   0,  -2,   0,
     0,   0,  16,   0,   0,  16,   0,   0,
     0,   0,  16,   0,   0,  16,   0,   0,
     0,  10,   0,   6,   0,   4,  16,   2,
     0,   0,   0,   0,   0,   0,  16,   0,
     0,   0,   0,   0,   0,   0,   0,   0,
    10,   0,   6,   0,   4,   0,   2,   0],
  [ -8,   8,  -8,   0,  -8,   0,  -8,   0,
    -4,   4,  -4,   0,  -4,   0,  -4,   0,
     0,   0,   8,   0,   0,   8,   0,   0,
     0,   0,   4,   0,   0,   4,   0,   0,
     0,  16,   0,  16,   0,  16,   8,  16,
     0,   0,   0,   0,   0,   0,   4,   0,
     0,   0,   0,   0,   0,   0,   0,   0,
    16,   0,  16,   0,  16,   0,  16,   0],
  [ -2,   8,  -1,   3,  -1,   2,   0,   1,
    -1,   4,  -1,   3,  -1,   2,  -1,   2,
     0,   0,   8,   0,   3,   8,   2,   3,
     0,   0,   4,   0,   3,   4,   2,   3,
     0,  10,   0,   6,   0,   4,   8,   2,
     0,   3,   0,   4,   0,   4,   4,   3,
     0,   0,   0,   0,   0,   0,   0,   0,
    10,   0,   6,   0,   4,   0,   3,   0],
  [-12,  14, -10,   0,  -9,   0,  -8,   0,
   -10,  12,  -9,   1,  -8,   0,  -7,   0,
     0,   0,  14,   0,   0,  14,   0,   0,
     0,   0,  12,   0,   0,  12,   0,   1,
     0,  14,   0,  12,   0,  11,  14,  10,
     0,   0,   0,   0,   0,   1,  12,   1,
     0,   0,   0,   0,   0,   0,   0,   0,
    14,   0,  12,   0,  11,   0,   9,   0]
]);

#[no_mangle]
pub static rav1e_filter_intra_taps: &[[i8; 64]] = &FILTER_INTRA_TAPS.array;

#[rustfmt::skip]
#[no_mangle]
pub static rav1e_dr_intra_derivative: &[i16; 90] = &[
  // More evenly spread out angles and limited to 10-bit
  // Values that are 0 will never be used
     0, 0, 0,       // Approx angle
  1023, 0, 0,       // 3, ...
   547, 0, 0,       // 6, ...
   372, 0, 0, 0, 0, // 9, ...
   273, 0, 0,       // 14, ...
   215, 0, 0,       // 17, ...
   178, 0, 0,       // 20, ...
   151, 0, 0,       // 23, ... (113 & 203 are base angles)
   132, 0, 0,       // 26, ...
   116, 0, 0,       // 29, ...
   102, 0, 0, 0,    // 32, ...
    90, 0, 0,       // 36, ...
    80, 0, 0,       // 39, ...
    71, 0, 0,       // 42, ...
    64, 0, 0,       // 45, ... (45 & 135 are base angles)
    57, 0, 0,       // 48, ...
    51, 0, 0,       // 51, ...
    45, 0, 0, 0,    // 54, ...
    40, 0, 0,       // 58, ...
    35, 0, 0,       // 61, ...
    31, 0, 0,       // 64, ...
    27, 0, 0,       // 67, ... (67 & 157 are base angles)
    23, 0, 0,       // 70, ...
    19, 0, 0,       // 73, ...
    15, 0, 0, 0, 0, // 76, ...
    11, 0, 0,       // 81, ...
     7, 0, 0,       // 84, ...
     3, 0, 0,       // 87, ...
];
