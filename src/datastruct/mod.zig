//! Data Structures Module
//!
//! Reusable data structures following the Ghostty pattern.

const std = @import("std");

pub const segmented_pool = @import("segmented_pool.zig");
pub const SegmentedPool = segmented_pool.SegmentedPool;

test {
    std.testing.refAllDecls(@This());
}
