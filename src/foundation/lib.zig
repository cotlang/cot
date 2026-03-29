//! Shared foundation types for the Cot compiler.
//!
//! Language-agnostic: types, source positions, target platform, debug logging.
//! Both libcot (frontend) and libcir (IR) depend on these.
//! A TypeScript or other frontend would use the same foundation.

pub const types = @import("types.zig");
pub const source = @import("source.zig");
pub const target = @import("target.zig");
pub const debug = @import("debug.zig");

test {
    _ = types;
    _ = source;
    _ = target;
    _ = debug;
}
