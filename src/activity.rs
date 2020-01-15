// Copyright (c) 2017-2019, The rav1e contributors. All rights reserved
//
// This source code is subject to the terms of the BSD 2 Clause License and
// the Alliance for Open Media Patent License 1.0. If the BSD 2 Clause License
// was not distributed with this source code in the LICENSE file, you can
// obtain it at www.aomedia.org/license/software. If the Alliance for Open
// Media Patent License 1.0 was not distributed with this source code in the
// PATENTS file, you can obtain it at www.aomedia.org/license/patent.

use crate::frame::*;
use crate::hawktracer::*;
use crate::tiling::*;
use crate::util::*;

use itertools::izip;

#[derive(Debug, Default, Clone)]
pub struct ActivityMask {
  variances: Vec<i64>,
  scales: Vec<f64>,
  // Width and height of the original frame that is masked
  width: usize,
  height: usize,
  // Side of unit (square) activity block in log2
  granularity: usize,
}

impl ActivityMask {
  #[hawktracer(activity_mask_from_plane)]
  pub fn from_plane<T: Pixel>(
    luma_plane: &Plane<T>, bit_depth: usize,
  ) -> ActivityMask {
    let PlaneConfig { width, height, .. } = luma_plane.cfg;

    let granularity = 3;

    let aligned_luma = Rect {
      x: 0_isize,
      y: 0_isize,
      width: (width >> granularity) << granularity,
      height: (height >> granularity) << granularity,
    };
    let luma = PlaneRegion::new(luma_plane, aligned_luma);

    let mut variances =
      Vec::with_capacity((height >> granularity) * (width >> granularity));

    for y in 0..height >> granularity {
      for x in 0..width >> granularity {
        let block_rect = Area::Rect {
          x: (x << granularity) as isize,
          y: (y << granularity) as isize,
          width: 8,
          height: 8,
        };
        let block = luma.subregion(block_rect);

        // Sum into columns to improve auto-vectorization
        let mut sum_s_cols: [u16; 8] = [0; 8];
        let mut sum_s2_cols: [u32; 8] = [0; 8];

        for j in 0..8 {
          let row = &block[j][0..8];
          for (sum_s, sum_s2, s) in
            izip!(&mut sum_s_cols, &mut sum_s2_cols, row)
          {
            // Don't convert directly to u32 to allow better vectorization
            let s: u16 = u16::cast_from(*s);
            *sum_s += s;

            // Convert to u32 to avoid overflows when multiplying
            let s: u32 = s as u32;
            *sum_s2 += s * s;
          }
        }

        // Sum together the sum of columns
        let sum_s: i64 =
          sum_s_cols.iter().map(|&a| u32::cast_from(a)).sum::<u32>() as i64;
        let sum_s2: i64 = sum_s2_cols.iter().sum::<u32>() as i64;

        // Use sums to calculate variance
        let variance = sum_s2 - ((sum_s * sum_s + 32) >> 6);
        variances.push(variance);
      }
    }

    let coeff_shift = bit_depth - 8;
    let scales = variances
      .iter()
      .map(|&svar| {
        (4033_f64 / 16_384_f64)
          * (svar + svar + (16_384 << (2 * coeff_shift))) as f64
          / f64::sqrt(
            ((16_265_089i64 << (4 * coeff_shift)) + svar * svar) as f64,
          )
          * 0.869_873_046_875f64
          + 0.150_146_484_375f64
      })
      .collect::<Vec<f64>>();
    ActivityMask { variances, scales, width, height, granularity }
  }

  #[inline(always)]
  pub fn variance_at(&self, x: usize, y: usize) -> Option<i64> {
    let (x, y) = (x >> self.granularity, y >> self.granularity);
    let (dec_width, dec_height) =
      (self.width >> self.granularity, self.height >> self.granularity);
    if x >= dec_width || y >= dec_height {
      None
    } else {
      Some(*self.variances.get(x + dec_width * y).unwrap())
    }
  }

  #[inline(always)]
  pub fn scale_at(&self, x: usize, y: usize) -> f64 {
    let (x, y) = (x >> self.granularity, y >> self.granularity);
    let (dec_width, dec_height) =
      (self.width >> self.granularity, self.height >> self.granularity);
    if x >= dec_width || y >= dec_height {
      1.0
    } else {
      self.scales[x + dec_width * y]
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
        .chunks_exact(self.width >> granularity)
        .skip(dec_y)
        .take(dec_height)
        .map(|row| row.iter().skip(dec_x).take(dec_width).sum::<i64>())
        .sum::<i64>() as f64
        / (dec_width as f64 * dec_height as f64);

      Some(activity.cbrt().sqrt())
    }
  }
}
