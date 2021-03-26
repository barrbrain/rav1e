// Copyright (c) 2018-2020, The rav1e contributors. All rights reserved
//
// This source code is subject to the terms of the BSD 2 Clause License and
// the Alliance for Open Media Patent License 1.0. If the BSD 2 Clause License
// was not distributed with this source code in the LICENSE file, you can
// obtain it at www.aomedia.org/license/software. If the Alliance for Open
// Media Patent License 1.0 was not distributed with this source code in the
// PATENTS file, you can obtain it at www.aomedia.org/license/patent.

use crate::api::lookahead::*;
use crate::api::EncoderConfig;
use crate::cpu_features::CpuFeatureLevel;
use crate::encoder::Sequence;
use crate::frame::*;
use crate::util::{CastFromPrimitive, Pixel};
use rust_hawktracer::*;
use std::cmp;
use std::sync::Arc;

/// Runs keyframe detection on frames from the lookahead queue.
pub struct SceneChangeDetector<T: Pixel> {
  /// Minimum average difference between YUV deltas that will trigger a scene change.
  threshold: usize,
  /// Fast scene cut detection mode, uses simple SAD instead of encoder cost estimates.
  fast_mode: bool,
  /// scaling factor for fast scene detection
  scale_factor: usize,
  // Frame buffer for scaled frames
  frame_buffer: Vec<Plane<T>>,
  /// Number of pixels in scaled frame for fast mode
  pixels: usize,
  /// The bit depth of the video.
  bit_depth: usize,
  /// The CPU feature level to be used.
  cpu_feature_level: CpuFeatureLevel,
  encoder_config: EncoderConfig,
  lookahead_distance: usize,
  sequence: Arc<Sequence>,
}

impl<T: Pixel> SceneChangeDetector<T> {
  pub fn new(
    encoder_config: EncoderConfig, cpu_feature_level: CpuFeatureLevel,
    lookahead_distance: usize, sequence: Arc<Sequence>,
  ) -> Self {
    // This implementation is based on a Python implementation at
    // https://pyscenedetect.readthedocs.io/en/latest/reference/detection-methods/.
    // The Python implementation uses HSV values and a threshold of 30. Comparing the
    // YUV values was sufficient in most cases, and avoided a more costly YUV->RGB->HSV
    // conversion, but the deltas needed to be scaled down. The deltas for keyframes
    // in YUV were about 1/3 to 1/2 of what they were in HSV, but non-keyframes were
    // very unlikely to have a delta greater than 3 in YUV, whereas they may reach into
    // the double digits in HSV. Therefore, 12 was chosen as a reasonable default threshold.
    // This may be adjusted later.
    //
    // This threshold is only used for the fast scenecut implementation.
    const BASE_THRESHOLD: usize = 20;
    let bit_depth = encoder_config.bit_depth;
    let fast_mode = encoder_config.speed_settings.fast_scene_detection
      || encoder_config.low_latency;

    // Scale factor for fast scene detection
    let scale_factor =
      if fast_mode { detect_scale_factor(&sequence) } else { 1 as usize };

    // Pixel count for fast scenedetect
    let pixels = if fast_mode {
      (sequence.max_frame_height as usize / scale_factor)
        * (sequence.max_frame_width as usize / scale_factor)
    } else {
      1
    };

    let frame_buffer = Vec::with_capacity(2);

    Self {
      threshold: BASE_THRESHOLD * bit_depth / 8,
      fast_mode,
      scale_factor,
      frame_buffer,
      pixels,
      bit_depth,
      cpu_feature_level,
      encoder_config,
      lookahead_distance,
      sequence,
    }
  }

  /// Runs keyframe detection on the next frame in the lookahead queue.
  ///
  /// This function requires that a subset of input frames
  /// is passed to it in order, and that `keyframes` is only
  /// updated from this method. `input_frameno` should correspond
  /// to the second frame in `frame_set`.
  ///
  /// This will gracefully handle the first frame in the video as well.
  #[hawktracer(analyze_next_frame)]
  pub fn analyze_next_frame(
    &mut self, frame_set: &[Arc<Frame<T>>], input_frameno: u64,
    previous_keyframe: u64,
  ) -> bool {
    // Find the distance to the previous keyframe.
    let distance = input_frameno - previous_keyframe;

    // Handle minimum and maximum key frame intervals.
    if distance < self.encoder_config.min_key_frame_interval {
      return false;
    }
    if distance >= self.encoder_config.max_key_frame_interval {
      return true;
    }

    if self.encoder_config.speed_settings.no_scene_detection {
      return false;
    }

    // Set our scenecut method
    let result = if self.fast_mode {
      self.fast_scenecut(frame_set[0].clone(), frame_set[1].clone())
    } else {
      self.cost_scenecut(
        frame_set[0].clone(),
        frame_set[1].clone(),
        input_frameno,
        previous_keyframe,
      )
    };

    debug!(
      "[SC-Detect] Frame {}: T={:.1} P={:.1} {}",
      input_frameno,
      result.threshold,
      result.inter_cost,
      if result.has_scenecut { "Scenecut" } else { "No cut" }
    );
    let keyframe_check = result.has_scenecut;
    keyframe_check
  }

  /// The fast algorithm detects fast cuts using a raw difference
  /// in pixel values between the scaled frames.
  #[hawktracer(fast_scenecut)]
  fn fast_scenecut(
    &mut self, frame1: Arc<Frame<T>>, frame2: Arc<Frame<T>>,
  ) -> ScenecutResult {
    // Downscaling both frames for comparison

    if self.frame_buffer.len() == 0 {
      let frame1_scaled = frame1.planes[0].downscale(self.scale_factor);
      self.frame_buffer.push(frame1_scaled);

      let frame2_scaled = frame2.planes[0].downscale(self.scale_factor);
      self.frame_buffer.push(frame2_scaled);
    } else {
      self.frame_buffer[0] = self.frame_buffer[1].clone();
      frame2.planes[0].downscale(self.scale_factor);
    }

    let delta =
      self.delta_in_planes(&self.frame_buffer[0], &self.frame_buffer[1]);
    let threshold = self.threshold;
    ScenecutResult {
      intra_cost: threshold as f64,
      threshold: threshold as f64,
      inter_cost: delta as f64,
      has_scenecut: delta >= threshold as f64,
    }
  }

  /// Run a comparison between two frames to determine if they qualify for a scenecut.
  ///
  /// Using block intra and inter costs
  /// to determine which method would be more efficient
  /// for coding this frame.
  #[hawktracer(cost_scenecut)]
  fn cost_scenecut(
    &self, frame1: Arc<Frame<T>>, frame2: Arc<Frame<T>>, frameno: u64,
    previous_keyframe: u64,
  ) -> ScenecutResult {
    let frame2_ref2 = Arc::clone(&frame2);
    let (intra_cost, inter_cost) = crate::rayon::join(
      move || {
        let intra_costs = estimate_intra_costs(
          &*frame2,
          self.bit_depth,
          self.cpu_feature_level,
        );
        intra_costs.iter().map(|&cost| cost as u64).sum::<u64>() as f64
          / intra_costs.len() as f64
      },
      move || {
        let inter_costs = estimate_inter_costs(
          frame2_ref2,
          frame1,
          self.bit_depth,
          self.encoder_config,
          self.sequence.clone(),
        );
        inter_costs.iter().map(|&cost| cost as u64).sum::<u64>() as f64
          / inter_costs.len() as f64
      },
    );

    // Sliding scale, more likely to choose a keyframe
    // as we get farther from the last keyframe.
    // Based on x264 scenecut code.
    //
    // `THRESH_MAX` determines how likely we are
    // to choose a keyframe, between 0.0-1.0.
    // Higher values mean we are more likely to choose a keyframe.
    // `0.4` was chosen based on trials of the `scenecut-720p` set in AWCY,
    // as it appeared to provide the best average compression.
    // This also matches the default scenecut threshold in x264.
    const THRESH_MAX: f64 = 0.4;
    const THRESH_MIN: f64 = THRESH_MAX * 0.25;
    let distance_from_keyframe = frameno - previous_keyframe;
    let min_keyint = self.encoder_config.min_key_frame_interval;
    let max_keyint = self.encoder_config.max_key_frame_interval;
    let bias = if distance_from_keyframe <= min_keyint / 4 {
      THRESH_MIN / 4.0
    } else if distance_from_keyframe <= min_keyint {
      THRESH_MIN * distance_from_keyframe as f64 / min_keyint as f64
    } else {
      THRESH_MIN
        + (THRESH_MAX - THRESH_MIN)
          * (distance_from_keyframe - min_keyint) as f64
          / (max_keyint - min_keyint) as f64
    };
    let threshold = intra_cost * (1.0 - bias);

    ScenecutResult {
      intra_cost,
      threshold,
      inter_cost,
      has_scenecut: inter_cost > threshold,
    }
  }

  /// Calculates delta beetween 2 planes
  /// returns average for pixel
  #[hawktracer(delta_in_planes)]
  fn delta_in_planes(&self, plane1: &Plane<T>, plane2: &Plane<T>) -> f64 {
    let mut delta = 0;

    let lines = plane1.rows_iter().zip(plane2.rows_iter());

    for (l1, l2) in lines {
      let delta_line = l1
        .iter()
        .zip(l2.iter())
        .map(|(&p1, &p2)| {
          (i16::cast_from(p1) - i16::cast_from(p2)).abs() as usize
        })
        .sum::<usize>();
      delta += delta_line;
    }
    delta as f64 / self.pixels as f64
  }
}

/// Scaling factor for frame in scenedetection
fn detect_scale_factor(sequence: &Arc<Sequence>) -> usize {
  let small_edge =
    cmp::min(sequence.max_frame_height, sequence.max_frame_width) as usize;
  let scale_factor = match small_edge {
    0..=480 => 1,
    481..=720 => 2,
    721..=1080 => 3,
    1081..=1600 => 4,
    1601..=std::usize::MAX => 6,
    _ => 1,
  } as usize;
  debug!(
    "Scene detection scale factor {}, [{},{}] -> [{},{}]",
    scale_factor,
    sequence.max_frame_width,
    sequence.max_frame_height,
    sequence.max_frame_width as usize / scale_factor,
    sequence.max_frame_height as usize / scale_factor
  );
  scale_factor
}

/// This struct primarily exists for returning metrics to the caller
/// for logging debug information.
#[derive(Debug, Clone, Copy)]
struct ScenecutResult {
  intra_cost: f64,
  inter_cost: f64,
  threshold: f64,
  has_scenecut: bool,
}
