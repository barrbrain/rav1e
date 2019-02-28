
pub const RDO_NUM_BINS: usize =  50;
pub const RDO_MAX_BIN: usize = 10000;
pub const RATE_EST_MAX_BIN: usize = 100000;
pub const RDO_QUANT_BINS: usize = 8;
pub const RDO_QUANT_DIV: usize = 256/RDO_QUANT_BINS;
pub const RDO_BIN_SIZE: u64 = (RDO_MAX_BIN / RDO_NUM_BINS) as u64;
pub const RATE_EST_BIN_SIZE: u64 = (RATE_EST_MAX_BIN / RDO_NUM_BINS) as u64;

use crate::partition::*;

pub static RDO_RATE_TABLE: [[[u64; RDO_NUM_BINS]; TxSize::TX_SIZES_ALL]; RDO_QUANT_BINS] = [
    [[0; RDO_NUM_BINS]; TxSize::TX_SIZES_ALL]; RDO_QUANT_BINS];
