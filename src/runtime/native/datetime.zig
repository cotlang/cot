//! Date/time native functions
//!
//! Namespace: std.datetime
//! Functions: date, time
//!
//! In .cot files: requires `import std.datetime` then call as `std.datetime.date()`
//! In .dbl files: available directly as `date()` (DBL compatibility)

const std = @import("std");
const native = @import("native.zig");
const NativeContext = native.NativeContext;
const NativeError = native.NativeError;
const NativeFn = native.NativeFn;
const Value = native.Value;

/// Register all datetime functions with both namespaced and short names
pub fn register(registry: anytype) !void {
    // Namespaced names (std.datetime.*)
    try registry.registerNative("std.time.date", date);
    try registry.registerNative("std.time.time", time);

    // Short names (DBL compatibility)
    try registry.registerNative("date", date);
    try registry.registerNative("time", time);
}

/// DATE - Get current date as YYYYMMDD
pub fn date(ctx: *NativeContext) NativeError!?Value {
    _ = ctx;
    const now = std.time.timestamp();
    const epoch_secs: i64 = @intCast(now);
    const epoch = std.time.epoch.EpochSeconds{ .secs = @intCast(epoch_secs) };
    const day = epoch.getEpochDay();
    const year_day = day.calculateYearDay();
    const month_day = year_day.calculateMonthDay();

    const year: i64 = year_day.year;
    const month: i64 = @intFromEnum(month_day.month);
    const day_num: i64 = month_day.day_index + 1;

    const result = year * 10000 + month * 100 + day_num;
    return Value.initInt(result);
}

/// TIME - Get current time as HHMMSS
pub fn time(ctx: *NativeContext) NativeError!?Value {
    _ = ctx;
    const now = std.time.timestamp();
    const epoch_secs: i64 = @intCast(now);
    const epoch = std.time.epoch.EpochSeconds{ .secs = @intCast(epoch_secs) };
    const day_secs = epoch.getDaySeconds();

    const hours = day_secs.getHoursIntoDay();
    const mins = day_secs.getMinutesIntoHour();
    const secs = day_secs.getSecondsIntoMinute();

    const result: i64 = @as(i64, hours) * 10000 + @as(i64, mins) * 100 + @as(i64, secs);
    return Value.initInt(result);
}
