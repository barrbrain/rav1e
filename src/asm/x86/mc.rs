// Copyright (c) 2019, The rav1e contributors. All rights reserved
//
// This source code is subject to the terms of the BSD 2 Clause License and
// the Alliance for Open Media Patent License 1.0. If the BSD 2 Clause License
// was not distributed with this source code in the LICENSE file, you can
// obtain it at www.aomedia.org/license/software. If the Alliance for Open
// Media Patent License 1.0 was not distributed with this source code in the
// PATENTS file, you can obtain it at www.aomedia.org/license/patent.

use crate::cpu_features::CpuFeatureLevel;
use crate::frame::*;
use crate::mc::FilterMode::*;
use crate::mc::*;
use crate::tiling::*;
use crate::util::*;

type PutFn = unsafe extern fn(
  dst: *mut u8,
  dst_stride: isize,
  src: *const u8,
  src_stride: isize,
  width: i32,
  height: i32,
  col_frac: i32,
  row_frac: i32,
);

type PutHBDFn = unsafe extern fn(
  dst: *mut u8,
  dst_stride: isize,
  src: *const u8,
  src_stride: isize,
  width: i32,
  height: i32,
  col_frac: i32,
  row_frac: i32,
  bit_depth: i32,
);

type PrepFn = unsafe extern fn(
  tmp: *mut i16,
  src: *const u8,
  src_stride: isize,
  width: i32,
  height: i32,
  col_frac: i32,
  row_frac: i32,
);

type PrepHBDFn = unsafe extern fn(
  tmp: *mut i16,
  src: *const u16,
  src_stride: isize,
  width: i32,
  height: i32,
  col_frac: i32,
  row_frac: i32,
  bit_depth: i32,
);

type AvgFn = unsafe extern fn(
  dst: *mut u8,
  dst_stride: isize,
  tmp1: *const i16,
  tmp2: *const i16,
  width: i32,
  height: i32,
);

type AvgHBDFn = unsafe extern fn(
  dst: *mut u16,
  dst_stride: isize,
  tmp1: *const i16,
  tmp2: *const i16,
  width: i32,
  height: i32,
  bit_depth: i32,
);

// gets an index that can be mapped to a function for a pair of filter modes
const fn get_2d_mode_idx(mode_x: FilterMode, mode_y: FilterMode) -> usize {
  (mode_x as usize + 4 * (mode_y as usize)) & 15
}

pub fn put_8tap<T: Pixel>(
  dst: &mut PlaneRegionMut<'_, T>, src: PlaneSlice<'_, T>, width: usize,
  height: usize, col_frac: i32, row_frac: i32, mode_x: FilterMode,
  mode_y: FilterMode, bit_depth: usize, cpu: CpuFeatureLevel,
) {
  let call_native = |dst: &mut PlaneRegionMut<'_, T>| {
    native::put_8tap(
      dst, src, width, height, col_frac, row_frac, mode_x, mode_y, bit_depth,
      cpu,
    );
  };
  #[cfg(feature = "check_asm")]
  let ref_dst = {
    let mut copy = dst.scratch_copy();
    call_native(&mut copy.as_region_mut());
    copy
  };
  match T::type_enum() {
    PixelType::U8 => {
      match PUT_FNS[cpu.as_index()][get_2d_mode_idx(mode_x, mode_y)] {
        Some(func) => unsafe {
          (func)(
            dst.data_ptr_mut() as *mut _,
            T::to_asm_stride(dst.plane_cfg.stride),
            src.as_ptr() as *const _,
            T::to_asm_stride(src.plane.cfg.stride),
            width as i32,
            height as i32,
            col_frac,
            row_frac,
          );
        },
        None => call_native(dst),
      }
    }
    PixelType::U16 => {
      match PUT_HBD_FNS[cpu.as_index()][get_2d_mode_idx(mode_x, mode_y)] {
        Some(func) => unsafe {
          (func)(
            dst.data_ptr_mut() as *mut _,
            T::to_asm_stride(dst.plane_cfg.stride),
            src.as_ptr() as *const _,
            T::to_asm_stride(src.plane.cfg.stride),
            width as i32,
            height as i32,
            col_frac,
            row_frac,
            bit_depth as i32,
          );
        },
        None => call_native(dst),
      }
    }
  }
  #[cfg(feature = "check_asm")]
  {
    for (dst_row, ref_row) in
      dst.rows_iter().zip(ref_dst.as_region().rows_iter())
    {
      for (dst, reference) in dst_row.iter().zip(ref_row) {
        assert_eq!(*dst, *reference);
      }
    }
  }
}

pub fn prep_8tap<T: Pixel>(
  tmp: &mut [i16], src: PlaneSlice<'_, T>, width: usize, height: usize,
  col_frac: i32, row_frac: i32, mode_x: FilterMode, mode_y: FilterMode,
  bit_depth: usize, cpu: CpuFeatureLevel,
) {
  let call_native = |tmp: &mut [i16]| {
    native::prep_8tap(
      tmp, src, width, height, col_frac, row_frac, mode_x, mode_y, bit_depth,
      cpu,
    );
  };
  #[cfg(feature = "check_asm")]
  let ref_tmp = {
    let mut copy = vec![0; width * height];
    copy[..].copy_from_slice(&tmp[..width * height]);
    call_native(&mut copy);
    copy
  };
  match T::type_enum() {
    PixelType::U8 => {
      match PREP_FNS[cpu.as_index()][get_2d_mode_idx(mode_x, mode_y)] {
        Some(func) => unsafe {
          (func)(
            tmp.as_mut_ptr(),
            src.as_ptr() as *const _,
            T::to_asm_stride(src.plane.cfg.stride),
            width as i32,
            height as i32,
            col_frac,
            row_frac,
          );
        },
        None => call_native(tmp),
      }
    }
    PixelType::U16 => {
      match PREP_HBD_FNS[cpu.as_index()][get_2d_mode_idx(mode_x, mode_y)] {
        Some(func) => unsafe {
          (func)(
            tmp.as_mut_ptr() as *mut _,
            src.as_ptr() as *const _,
            T::to_asm_stride(src.plane.cfg.stride),
            width as i32,
            height as i32,
            col_frac,
            row_frac,
            bit_depth as i32,
          );
        },
        None => call_native(tmp),
      }
    }
  }
  #[cfg(feature = "check_asm")]
  {
    assert_eq!(&tmp[..width * height], &ref_tmp[..]);
  }
}

pub fn mc_avg<T: Pixel>(
  dst: &mut PlaneRegionMut<'_, T>, tmp1: &[i16], tmp2: &[i16], width: usize,
  height: usize, bit_depth: usize, cpu: CpuFeatureLevel,
) {
  let call_native = |dst: &mut PlaneRegionMut<'_, T>| {
    native::mc_avg(dst, tmp1, tmp2, width, height, bit_depth, cpu);
  };
  #[cfg(feature = "check_asm")]
  let ref_dst = {
    let mut copy = dst.scratch_copy();
    call_native(&mut copy.as_region_mut());
    copy
  };
  match T::type_enum() {
    PixelType::U8 => match AVG_FNS[cpu.as_index()] {
      Some(func) => unsafe {
        (func)(
          dst.data_ptr_mut() as *mut _,
          T::to_asm_stride(dst.plane_cfg.stride),
          tmp1.as_ptr(),
          tmp2.as_ptr(),
          width as i32,
          height as i32,
        );
      },
      None => call_native(dst),
    },
    PixelType::U16 => match AVG_HBD_FNS[cpu.as_index()] {
      Some(func) => unsafe {
        (func)(
          dst.data_ptr_mut() as *mut _,
          T::to_asm_stride(dst.plane_cfg.stride),
          tmp1.as_ptr(),
          tmp2.as_ptr(),
          width as i32,
          height as i32,
          bit_depth as i32,
        );
      },
      None => call_native(dst),
    },
  }
  #[cfg(feature = "check_asm")]
  {
    for (dst_row, ref_row) in
      dst.rows_iter().zip(ref_dst.as_region().rows_iter())
    {
      for (dst, reference) in dst_row.iter().zip(ref_row) {
        assert_eq!(*dst, *reference);
      }
    }
  }
}

macro_rules! decl_mc_fns {
  ($(($mode_x:expr, $mode_y:expr, $func_name:ident)),+) => {
    extern {
      $(
        fn $func_name(
          dst: *mut u8, dst_stride: isize, src: *const u8, src_stride: isize,
          w: i32, h: i32, mx: i32, my: i32
        );
      )*
    }

    static PUT_FNS_AVX2: [Option<PutFn>; 16] = {
      let mut out: [Option<PutFn>; 16] = [None; 16];
      $(
        out[get_2d_mode_idx($mode_x, $mode_y)] = Some($func_name);
      )*
      out
    };
  }
}

decl_mc_fns!(
  (REGULAR, REGULAR, rav1e_put_8tap_regular_avx2),
  (REGULAR, SMOOTH, rav1e_put_8tap_regular_smooth_avx2),
  (REGULAR, SHARP, rav1e_put_8tap_regular_sharp_avx2),
  (SMOOTH, REGULAR, rav1e_put_8tap_smooth_regular_avx2),
  (SMOOTH, SMOOTH, rav1e_put_8tap_smooth_avx2),
  (SMOOTH, SHARP, rav1e_put_8tap_smooth_sharp_avx2),
  (SHARP, REGULAR, rav1e_put_8tap_sharp_regular_avx2),
  (SHARP, SMOOTH, rav1e_put_8tap_sharp_smooth_avx2),
  (SHARP, SHARP, rav1e_put_8tap_sharp_avx2),
  (BILINEAR, BILINEAR, rav1e_put_bilin_avx2)
);

pub(crate) static PUT_FNS: [[Option<PutFn>; 16]; CpuFeatureLevel::len()] = {
  let mut out = [[None; 16]; CpuFeatureLevel::len()];
  out[CpuFeatureLevel::AVX2 as usize] = PUT_FNS_AVX2;
  out
};

pub(crate) static PUT_HBD_FNS: [[Option<PutHBDFn>; 16];
  CpuFeatureLevel::len()] = [[None; 16]; CpuFeatureLevel::len()];

macro_rules! decl_mct_fns {
  ($(($mode_x:expr, $mode_y:expr, $func_name:ident)),+) => {
    extern {
      $(
        fn $func_name(
          tmp: *mut i16, src: *const u8, src_stride: libc::ptrdiff_t, w: i32,
          h: i32, mx: i32, my: i32
        );
      )*
    }

    static PREP_FNS_AVX2: [Option<PrepFn>; 16] = {
      let mut out: [Option<PrepFn>; 16] = [None; 16];
      $(
        out[get_2d_mode_idx($mode_x, $mode_y)] = Some($func_name);
      )*
      out
    };
  }
}

decl_mct_fns!(
  (REGULAR, REGULAR, rav1e_prep_8tap_regular_avx2),
  (REGULAR, SMOOTH, rav1e_prep_8tap_regular_smooth_avx2),
  (REGULAR, SHARP, rav1e_prep_8tap_regular_sharp_avx2),
  (SMOOTH, REGULAR, rav1e_prep_8tap_smooth_regular_avx2),
  (SMOOTH, SMOOTH, rav1e_prep_8tap_smooth_avx2),
  (SMOOTH, SHARP, rav1e_prep_8tap_smooth_sharp_avx2),
  (SHARP, REGULAR, rav1e_prep_8tap_sharp_regular_avx2),
  (SHARP, SMOOTH, rav1e_prep_8tap_sharp_smooth_avx2),
  (SHARP, SHARP, rav1e_prep_8tap_sharp_avx2),
  (BILINEAR, BILINEAR, rav1e_prep_bilin_avx2)
);

pub(crate) static PREP_FNS: [[Option<PrepFn>; 16]; CpuFeatureLevel::len()] = {
  let mut out = [[None; 16]; CpuFeatureLevel::len()];
  out[CpuFeatureLevel::AVX2 as usize] = PREP_FNS_AVX2;
  out
};

pub(crate) static PREP_HBD_FNS: [[Option<PrepHBDFn>; 16];
  CpuFeatureLevel::len()] = [[None; 16]; CpuFeatureLevel::len()];

extern {
  fn rav1e_avg_avx2(
    dst: *mut u8, dst_stride: libc::ptrdiff_t, tmp1: *const i16,
    tmp2: *const i16, w: i32, h: i32,
  );
}

pub(crate) static AVG_FNS: [Option<AvgFn>; CpuFeatureLevel::len()] = {
  let mut out: [Option<AvgFn>; CpuFeatureLevel::len()] =
    [None; CpuFeatureLevel::len()];
  out[CpuFeatureLevel::AVX2 as usize] = Some(rav1e_avg_avx2);
  out
};

pub(crate) static AVG_HBD_FNS: [Option<AvgHBDFn>; CpuFeatureLevel::len()] =
  [None; CpuFeatureLevel::len()];

#[rustfmt::skip]
static MC_WARP_FILTER: AlignedArray<[[i8; 8]; 193]> = AlignedArray::new([
  // [-1, 0)
  [ 0, 127,   0, 0,   0,   1, 0, 0 ], [ 0, 127,   0, 0,  -1,   2, 0, 0 ],
  [ 1, 127,  -1, 0,  -3,   4, 0, 0 ], [ 1, 126,  -2, 0,  -4,   6, 1, 0 ],
  [ 1, 126,  -3, 0,  -5,   8, 1, 0 ], [ 1, 125,  -4, 0,  -6,  11, 1, 0 ],
  [ 1, 124,  -4, 0,  -7,  13, 1, 0 ], [ 2, 123,  -5, 0,  -8,  15, 1, 0 ],
  [ 2, 122,  -6, 0,  -9,  18, 1, 0 ], [ 2, 121,  -6, 0, -10,  20, 1, 0 ],
  [ 2, 120,  -7, 0, -11,  22, 2, 0 ], [ 2, 119,  -8, 0, -12,  25, 2, 0 ],
  [ 3, 117,  -8, 0, -13,  27, 2, 0 ], [ 3, 116,  -9, 0, -13,  29, 2, 0 ],
  [ 3, 114, -10, 0, -14,  32, 3, 0 ], [ 3, 113, -10, 0, -15,  35, 2, 0 ],
  [ 3, 111, -11, 0, -15,  37, 3, 0 ], [ 3, 109, -11, 0, -16,  40, 3, 0 ],
  [ 3, 108, -12, 0, -16,  42, 3, 0 ], [ 4, 106, -13, 0, -17,  45, 3, 0 ],
  [ 4, 104, -13, 0, -17,  47, 3, 0 ], [ 4, 102, -14, 0, -17,  50, 3, 0 ],
  [ 4, 100, -14, 0, -17,  52, 3, 0 ], [ 4,  98, -15, 0, -18,  55, 4, 0 ],
  [ 4,  96, -15, 0, -18,  58, 3, 0 ], [ 4,  94, -16, 0, -18,  60, 4, 0 ],
  [ 4,  91, -16, 0, -18,  63, 4, 0 ], [ 4,  89, -16, 0, -18,  65, 4, 0 ],
  [ 4,  87, -17, 0, -18,  68, 4, 0 ], [ 4,  85, -17, 0, -18,  70, 4, 0 ],
  [ 4,  82, -17, 0, -18,  73, 4, 0 ], [ 4,  80, -17, 0, -18,  75, 4, 0 ],
  [ 4,  78, -18, 0, -18,  78, 4, 0 ], [ 4,  75, -18, 0, -17,  80, 4, 0 ],
  [ 4,  73, -18, 0, -17,  82, 4, 0 ], [ 4,  70, -18, 0, -17,  85, 4, 0 ],
  [ 4,  68, -18, 0, -17,  87, 4, 0 ], [ 4,  65, -18, 0, -16,  89, 4, 0 ],
  [ 4,  63, -18, 0, -16,  91, 4, 0 ], [ 4,  60, -18, 0, -16,  94, 4, 0 ],
  [ 3,  58, -18, 0, -15,  96, 4, 0 ], [ 4,  55, -18, 0, -15,  98, 4, 0 ],
  [ 3,  52, -17, 0, -14, 100, 4, 0 ], [ 3,  50, -17, 0, -14, 102, 4, 0 ],
  [ 3,  47, -17, 0, -13, 104, 4, 0 ], [ 3,  45, -17, 0, -13, 106, 4, 0 ],
  [ 3,  42, -16, 0, -12, 108, 3, 0 ], [ 3,  40, -16, 0, -11, 109, 3, 0 ],
  [ 3,  37, -15, 0, -11, 111, 3, 0 ], [ 2,  35, -15, 0, -10, 113, 3, 0 ],
  [ 3,  32, -14, 0, -10, 114, 3, 0 ], [ 2,  29, -13, 0,  -9, 116, 3, 0 ],
  [ 2,  27, -13, 0,  -8, 117, 3, 0 ], [ 2,  25, -12, 0,  -8, 119, 2, 0 ],
  [ 2,  22, -11, 0,  -7, 120, 2, 0 ], [ 1,  20, -10, 0,  -6, 121, 2, 0 ],
  [ 1,  18,  -9, 0,  -6, 122, 2, 0 ], [ 1,  15,  -8, 0,  -5, 123, 2, 0 ],
  [ 1,  13,  -7, 0,  -4, 124, 1, 0 ], [ 1,  11,  -6, 0,  -4, 125, 1, 0 ],
  [ 1,   8,  -5, 0,  -3, 126, 1, 0 ], [ 1,   6,  -4, 0,  -2, 126, 1, 0 ],
  [ 0,   4,  -3, 0,  -1, 127, 1, 0 ], [ 0,   2,  -1, 0,   0, 127, 0, 0 ],
  // [0, 1)
  [  0,   0,   1, 0, 0, 127,   0,  0 ], [  0,  -1,   2, 0, 0, 127,   0,  0 ],
  [  0,  -3,   4, 1, 1, 127,  -2,  0 ], [  0,  -5,   6, 1, 1, 127,  -2,  0 ],
  [  0,  -6,   8, 1, 2, 126,  -3,  0 ], [ -1,  -7,  11, 2, 2, 126,  -4, -1 ],
  [ -1,  -8,  13, 2, 3, 125,  -5, -1 ], [ -1, -10,  16, 3, 3, 124,  -6, -1 ],
  [ -1, -11,  18, 3, 4, 123,  -7, -1 ], [ -1, -12,  20, 3, 4, 122,  -7, -1 ],
  [ -1, -13,  23, 3, 4, 121,  -8, -1 ], [ -2, -14,  25, 4, 5, 120,  -9, -1 ],
  [ -1, -15,  27, 4, 5, 119, -10, -1 ], [ -1, -16,  30, 4, 5, 118, -11, -1 ],
  [ -2, -17,  33, 5, 6, 116, -12, -1 ], [ -2, -17,  35, 5, 6, 114, -12, -1 ],
  [ -2, -18,  38, 5, 6, 113, -13, -1 ], [ -2, -19,  41, 6, 7, 111, -14, -2 ],
  [ -2, -19,  43, 6, 7, 110, -15, -2 ], [ -2, -20,  46, 6, 7, 108, -15, -2 ],
  [ -2, -20,  49, 6, 7, 106, -16, -2 ], [ -2, -21,  51, 7, 7, 104, -16, -2 ],
  [ -2, -21,  54, 7, 7, 102, -17, -2 ], [ -2, -21,  56, 7, 8, 100, -18, -2 ],
  [ -2, -22,  59, 7, 8,  98, -18, -2 ], [ -2, -22,  62, 7, 8,  96, -19, -2 ],
  [ -2, -22,  64, 7, 8,  94, -19, -2 ], [ -2, -22,  67, 8, 8,  91, -20, -2 ],
  [ -2, -22,  69, 8, 8,  89, -20, -2 ], [ -2, -22,  72, 8, 8,  87, -21, -2 ],
  [ -2, -21,  74, 8, 8,  84, -21, -2 ], [ -2, -22,  77, 8, 8,  82, -21, -2 ],
  [ -2, -21,  79, 8, 8,  79, -21, -2 ], [ -2, -21,  82, 8, 8,  77, -22, -2 ],
  [ -2, -21,  84, 8, 8,  74, -21, -2 ], [ -2, -21,  87, 8, 8,  72, -22, -2 ],
  [ -2, -20,  89, 8, 8,  69, -22, -2 ], [ -2, -20,  91, 8, 8,  67, -22, -2 ],
  [ -2, -19,  94, 8, 7,  64, -22, -2 ], [ -2, -19,  96, 8, 7,  62, -22, -2 ],
  [ -2, -18,  98, 8, 7,  59, -22, -2 ], [ -2, -18, 100, 8, 7,  56, -21, -2 ],
  [ -2, -17, 102, 7, 7,  54, -21, -2 ], [ -2, -16, 104, 7, 7,  51, -21, -2 ],
  [ -2, -16, 106, 7, 6,  49, -20, -2 ], [ -2, -15, 108, 7, 6,  46, -20, -2 ],
  [ -2, -15, 110, 7, 6,  43, -19, -2 ], [ -2, -14, 111, 7, 6,  41, -19, -2 ],
  [ -1, -13, 113, 6, 5,  38, -18, -2 ], [ -1, -12, 114, 6, 5,  35, -17, -2 ],
  [ -1, -12, 116, 6, 5,  33, -17, -2 ], [ -1, -11, 118, 5, 4,  30, -16, -1 ],
  [ -1, -10, 119, 5, 4,  27, -15, -1 ], [ -1,  -9, 120, 5, 4,  25, -14, -2 ],
  [ -1,  -8, 121, 4, 3,  23, -13, -1 ], [ -1,  -7, 122, 4, 3,  20, -12, -1 ],
  [ -1,  -7, 123, 4, 3,  18, -11, -1 ], [ -1,  -6, 124, 3, 3,  16, -10, -1 ],
  [ -1,  -5, 125, 3, 2,  13,  -8, -1 ], [ -1,  -4, 126, 2, 2,  11,  -7, -1 ],
  [  0,  -3, 126, 2, 1,   8,  -6,  0 ], [  0,  -2, 127, 1, 1,   6,  -5,  0 ],
  [  0,  -2, 127, 1, 1,   4,  -3,  0 ], [  0,   0, 127, 0, 0,   2,  -1,  0 ],
  // [1, 2)
  [ 0, 0, 127,   0, 0,   1,   0, 0 ], [ 0, 0, 127,   0, 0,  -1,   2, 0 ],
  [ 0, 1, 127,  -1, 0,  -3,   4, 0 ], [ 0, 1, 126,  -2, 0,  -4,   6, 1 ],
  [ 0, 1, 126,  -3, 0,  -5,   8, 1 ], [ 0, 1, 125,  -4, 0,  -6,  11, 1 ],
  [ 0, 1, 124,  -4, 0,  -7,  13, 1 ], [ 0, 2, 123,  -5, 0,  -8,  15, 1 ],
  [ 0, 2, 122,  -6, 0,  -9,  18, 1 ], [ 0, 2, 121,  -6, 0, -10,  20, 1 ],
  [ 0, 2, 120,  -7, 0, -11,  22, 2 ], [ 0, 2, 119,  -8, 0, -12,  25, 2 ],
  [ 0, 3, 117,  -8, 0, -13,  27, 2 ], [ 0, 3, 116,  -9, 0, -13,  29, 2 ],
  [ 0, 3, 114, -10, 0, -14,  32, 3 ], [ 0, 3, 113, -10, 0, -15,  35, 2 ],
  [ 0, 3, 111, -11, 0, -15,  37, 3 ], [ 0, 3, 109, -11, 0, -16,  40, 3 ],
  [ 0, 3, 108, -12, 0, -16,  42, 3 ], [ 0, 4, 106, -13, 0, -17,  45, 3 ],
  [ 0, 4, 104, -13, 0, -17,  47, 3 ], [ 0, 4, 102, -14, 0, -17,  50, 3 ],
  [ 0, 4, 100, -14, 0, -17,  52, 3 ], [ 0, 4,  98, -15, 0, -18,  55, 4 ],
  [ 0, 4,  96, -15, 0, -18,  58, 3 ], [ 0, 4,  94, -16, 0, -18,  60, 4 ],
  [ 0, 4,  91, -16, 0, -18,  63, 4 ], [ 0, 4,  89, -16, 0, -18,  65, 4 ],
  [ 0, 4,  87, -17, 0, -18,  68, 4 ], [ 0, 4,  85, -17, 0, -18,  70, 4 ],
  [ 0, 4,  82, -17, 0, -18,  73, 4 ], [ 0, 4,  80, -17, 0, -18,  75, 4 ],
  [ 0, 4,  78, -18, 0, -18,  78, 4 ], [ 0, 4,  75, -18, 0, -17,  80, 4 ],
  [ 0, 4,  73, -18, 0, -17,  82, 4 ], [ 0, 4,  70, -18, 0, -17,  85, 4 ],
  [ 0, 4,  68, -18, 0, -17,  87, 4 ], [ 0, 4,  65, -18, 0, -16,  89, 4 ],
  [ 0, 4,  63, -18, 0, -16,  91, 4 ], [ 0, 4,  60, -18, 0, -16,  94, 4 ],
  [ 0, 3,  58, -18, 0, -15,  96, 4 ], [ 0, 4,  55, -18, 0, -15,  98, 4 ],
  [ 0, 3,  52, -17, 0, -14, 100, 4 ], [ 0, 3,  50, -17, 0, -14, 102, 4 ],
  [ 0, 3,  47, -17, 0, -13, 104, 4 ], [ 0, 3,  45, -17, 0, -13, 106, 4 ],
  [ 0, 3,  42, -16, 0, -12, 108, 3 ], [ 0, 3,  40, -16, 0, -11, 109, 3 ],
  [ 0, 3,  37, -15, 0, -11, 111, 3 ], [ 0, 2,  35, -15, 0, -10, 113, 3 ],
  [ 0, 3,  32, -14, 0, -10, 114, 3 ], [ 0, 2,  29, -13, 0,  -9, 116, 3 ],
  [ 0, 2,  27, -13, 0,  -8, 117, 3 ], [ 0, 2,  25, -12, 0,  -8, 119, 2 ],
  [ 0, 2,  22, -11, 0,  -7, 120, 2 ], [ 0, 1,  20, -10, 0,  -6, 121, 2 ],
  [ 0, 1,  18,  -9, 0,  -6, 122, 2 ], [ 0, 1,  15,  -8, 0,  -5, 123, 2 ],
  [ 0, 1,  13,  -7, 0,  -4, 124, 1 ], [ 0, 1,  11,  -6, 0,  -4, 125, 1 ],
  [ 0, 1,   8,  -5, 0,  -3, 126, 1 ], [ 0, 1,   6,  -4, 0,  -2, 126, 1 ],
  [ 0, 0,   4,  -3, 0,  -1, 127, 1 ], [ 0, 0,   2,  -1, 0,   0, 127, 0 ],
  // dummy (replicate row index 191)
  [ 0, 0,   2,  -1, 0,   0, 127, 0 ]
]);

#[no_mangle]
pub static rav1e_mc_warp_filter: &[[i8; 8]] = &MC_WARP_FILTER.array;
