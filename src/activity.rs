// Copyright (c) 2017-2020, The rav1e contributors. All rights reserved
//
// This source code is subject to the terms of the BSD 2 Clause License and
// the Alliance for Open Media Patent License 1.0. If the BSD 2 Clause License
// was not distributed with this source code in the LICENSE file, you can
// obtain it at www.aomedia.org/license/software. If the Alliance for Open
// Media Patent License 1.0 was not distributed with this source code in the
// PATENTS file, you can obtain it at www.aomedia.org/license/patent.

use crate::frame::*;
use crate::rdo::{ssim_boost, DistortionScale};
use crate::tiling::*;
use crate::util::*;
use itertools::izip;
use rust_hawktracer::*;

#[derive(Debug, Default, Clone)]
pub struct ActivityMask {
  variances: Vec<u32>,
  // Width and height of the original frame that is masked
  width: usize,
  height: usize,
  // Side of unit (square) activity block in log2
  granularity: usize,
}

impl ActivityMask {
  #[hawktracer(activity_mask_from_plane)]
  pub fn from_plane<T: Pixel>(luma_plane: &Plane<T>) -> ActivityMask {
    let PlaneConfig { width, height, .. } = luma_plane.cfg;

    let granularity = 3;
    let w_in_b = width.align_power_of_two_and_shift(granularity);
    let h_in_b = height.align_power_of_two_and_shift(granularity);

    let aligned_luma = Rect {
      x: 0_isize,
      y: 0_isize,
      width: width.align_power_of_two(granularity),
      height: height.align_power_of_two(granularity),
    };
    let luma = PlaneRegion::new(luma_plane, aligned_luma);

    let mut variances = Vec::with_capacity(w_in_b * h_in_b);

    for y in 0..h_in_b {
      for x in 0..w_in_b {
        let block_rect = Area::Rect {
          x: (x << granularity) as isize,
          y: (y << granularity) as isize,
          width: 8,
          height: 8,
        };

        let block = luma.subregion(block_rect);
        let variance = variance_8x8(&block);
        variances.push(variance);
      }
    }
    ActivityMask { variances, width, height, granularity }
  }

  pub fn variance_at(&self, x: usize, y: usize) -> Option<f64> {
    let w_in_b = self.width.align_power_of_two_and_shift(self.granularity);
    let h_in_b = self.height.align_power_of_two_and_shift(self.granularity);
    if x > w_in_b || y > h_in_b {
      None
    } else {
      Some(*self.variances.get(x + w_in_b * y).unwrap() as f64)
    }
  }

  pub fn mean_activity_of(&self, rect: Rect) -> Option<f64> {
    let Rect { x, y, width, height } = rect;
    let (x, y) = (x as usize, y as usize);
    let granularity = self.granularity;
    let (dec_x, dec_y) = (x >> granularity, y >> granularity);
    let (dec_width, dec_height) =
      (width >> granularity, height >> granularity);

    if x > self.width
      || y > self.height
      || (x + width) > self.width
      || (y + height) > self.height
      || dec_width == 0
      || dec_height == 0
    {
      // Region lies out of the frame or is smaller than 8x8 on some axis
      None
    } else {
      let activity = self
        .variances
        .chunks_exact(self.width.align_power_of_two_and_shift(granularity))
        .skip(dec_y)
        .take(dec_height)
        .map(|row| {
          row
            .iter()
            .skip(dec_x)
            .take(dec_width)
            .map(|&a| a as u64)
            .sum::<u64>()
        })
        .sum::<u64>() as f64
        / (dec_width as f64 * dec_height as f64);

      Some(activity.cbrt().sqrt())
    }
  }

  pub fn fill_activity_scales(
    &self, bit_depth: usize, activity_scales: &mut Box<[DistortionScale]>,
  ) {
    for (dst, &src) in activity_scales.iter_mut().zip(self.variances.iter()) {
      *dst = ssim_boost(src as i64, src as i64, bit_depth);
    }
  }

  #[cfg(feature = "dump_lookahead_data")]
  pub fn dump(&self, data_location: std::path::PathBuf, input_frameno: u64) {
    let plane = &self.variances;
    let file_name = format!("{:010}-variance", input_frameno);
    let buf: Vec<_> =
      plane.iter().map(|&p| (p as f32 * 4.).sqrt() as u8).collect();
    image::GrayImage::from_vec(
      self.width.align_power_of_two_and_shift(self.granularity) as u32,
      self.height.align_power_of_two_and_shift(self.granularity) as u32,
      buf,
    )
    .unwrap()
    .save(data_location.join(file_name).with_extension("png"))
    .unwrap();
  }
}

// The microbenchmarks perform better with inlining turned off
#[inline(never)]
fn variance_8x8<T: Pixel>(src: &PlaneRegion<'_, T>) -> u32 {
  debug_assert!(src.plane_cfg.xdec == 0);
  debug_assert!(src.plane_cfg.ydec == 0);

  // Sum into columns to improve auto-vectorization
  let mut sum_s_cols: [u16; 8] = [0; 8];
  let mut sum_s2_cols: [u32; 8] = [0; 8];

  // Check upfront that 8 rows are available.
  let _row = &src[7];

  for j in 0..8 {
    let row = &src[j][0..8];
    for (sum_s, sum_s2, s) in izip!(&mut sum_s_cols, &mut sum_s2_cols, row) {
      // Don't convert directly to u32 to allow better vectorization
      let s: u16 = u16::cast_from(*s);
      *sum_s += s;

      // Convert to u32 to avoid overflows when multiplying
      let s: u32 = s as u32;
      *sum_s2 += s * s;
    }
  }

  // Sum together the sum of columns
  let sum_s = sum_s_cols.iter().map(|&a| u32::cast_from(a)).sum::<u32>();
  let sum_s2 = sum_s2_cols.iter().sum::<u32>();

  // Use sums to calculate variance
  sum_s2 - ((sum_s * sum_s + 32) >> 6)
}
