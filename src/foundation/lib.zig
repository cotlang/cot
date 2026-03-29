//! Shared foundation types for the Cot compiler.
//! Both libcot (frontend) and libcir (IR) depend on these.

pub const types = @import("types.zig");
pub const source = @import("source.zig");
pub const token = @import("token.zig");
pub const errors = @import("errors.zig");
pub const target = @import("target.zig");
pub const debug = @import("debug.zig");
pub const comptime_val = @import("comptime.zig");

test {
    _ = types;
    _ = source;
    _ = token;
    _ = errors;
    _ = target;
    _ = debug;
    _ = comptime_val;
}
