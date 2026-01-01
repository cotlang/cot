//! Bytecode Profiler
//!
//! Collects execution statistics for performance analysis and optimization.
//! Use this to identify hot paths, frequent opcode pairs (superinstruction candidates),
//! and type patterns for specialization.
//!
//! ## Usage
//!
//! Enable profiling by building with `-Dprofiling=true`:
//! ```
//! zig build -Dprofiling=true
//! ```
//!
//! In code:
//! ```zig
//! var profiler = Profiler.init(allocator);
//! defer profiler.deinit();
//!
//! // In VM execution loop:
//! profiler.recordOpcode(opcode, prev_opcode);
//!
//! // After execution:
//! try profiler.report(std.io.getStdErr().writer());
//! ```

const std = @import("std");
const Opcode = @import("opcodes.zig").Opcode;

/// Number of opcodes in the instruction set
const OPCODE_COUNT = @typeInfo(Opcode).Enum.fields.len;

/// Profiling data for a single execution run
pub const Profiler = struct {
    allocator: std.mem.Allocator,

    /// Opcode execution counts
    opcode_counts: [OPCODE_COUNT]u64,

    /// Opcode pair frequencies [prev][current]
    /// Used to identify superinstruction candidates
    opcode_pairs: [OPCODE_COUNT][OPCODE_COUNT]u64,

    /// Previous opcode (for pair tracking)
    prev_opcode: ?Opcode,

    /// Total instructions executed
    total_instructions: u64,

    /// Hot instruction addresses (IP -> count)
    /// Tracks which code locations are executed most
    hot_ips: std.AutoHashMap(u32, u64),

    /// Type feedback at specific IPs
    /// Maps IP -> observed type patterns
    type_feedback: std.AutoHashMap(u32, TypeFeedback),

    /// Timing data
    start_time: i64,
    end_time: i64,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .opcode_counts = [_]u64{0} ** OPCODE_COUNT,
            .opcode_pairs = [_][OPCODE_COUNT]u64{[_]u64{0} ** OPCODE_COUNT} ** OPCODE_COUNT,
            .prev_opcode = null,
            .total_instructions = 0,
            .hot_ips = std.AutoHashMap(u32, u64).init(allocator),
            .type_feedback = std.AutoHashMap(u32, TypeFeedback).init(allocator),
            .start_time = 0,
            .end_time = 0,
        };
    }

    pub fn deinit(self: *Self) void {
        self.hot_ips.deinit();
        self.type_feedback.deinit();
    }

    /// Start profiling session
    pub fn start(self: *Self) void {
        self.start_time = std.time.milliTimestamp();
    }

    /// End profiling session
    pub fn stop(self: *Self) void {
        self.end_time = std.time.milliTimestamp();
    }

    /// Record an opcode execution
    pub fn recordOpcode(self: *Self, opcode: Opcode) void {
        const idx = @intFromEnum(opcode);
        self.opcode_counts[idx] += 1;
        self.total_instructions += 1;

        // Record pair frequency
        if (self.prev_opcode) |prev| {
            const prev_idx = @intFromEnum(prev);
            self.opcode_pairs[prev_idx][idx] += 1;
        }

        self.prev_opcode = opcode;
    }

    /// Record instruction address for hot-path tracking
    pub fn recordIP(self: *Self, ip: u32) void {
        const entry = self.hot_ips.getOrPut(ip) catch return;
        if (entry.found_existing) {
            entry.value_ptr.* += 1;
        } else {
            entry.value_ptr.* = 1;
        }
    }

    /// Record type feedback at an IP
    pub fn recordTypeFeedback(self: *Self, ip: u32, type_tag: u8) void {
        const entry = self.type_feedback.getOrPut(ip) catch return;
        if (entry.found_existing) {
            entry.value_ptr.recordType(type_tag);
        } else {
            entry.value_ptr.* = TypeFeedback.init();
            entry.value_ptr.recordType(type_tag);
        }
    }

    /// Reset all counters
    pub fn reset(self: *Self) void {
        @memset(&self.opcode_counts, 0);
        for (&self.opcode_pairs) |*row| {
            @memset(row, 0);
        }
        self.prev_opcode = null;
        self.total_instructions = 0;
        self.hot_ips.clearRetainingCapacity();
        self.type_feedback.clearRetainingCapacity();
        self.start_time = 0;
        self.end_time = 0;
    }

    /// Generate profiling report
    pub fn report(self: *Self, writer: anytype) !void {
        try writer.print("\n", .{});
        try writer.print("╔══════════════════════════════════════════════════════════════╗\n", .{});
        try writer.print("║              BYTECODE PROFILER REPORT                        ║\n", .{});
        try writer.print("╚══════════════════════════════════════════════════════════════╝\n", .{});
        try writer.print("\n", .{});

        // Summary
        const duration_ms = self.end_time - self.start_time;
        const ips: f64 = if (duration_ms > 0)
            @as(f64, @floatFromInt(self.total_instructions)) / (@as(f64, @floatFromInt(duration_ms)) / 1000.0)
        else
            0;

        try writer.print("SUMMARY\n", .{});
        try writer.print("───────────────────────────────────────────────────────────────\n", .{});
        try writer.print("  Total instructions:  {d:>12}\n", .{self.total_instructions});
        try writer.print("  Execution time:      {d:>12} ms\n", .{duration_ms});
        try writer.print("  Instructions/sec:    {d:>12.0}\n", .{ips});
        try writer.print("\n", .{});

        // Top opcodes
        try self.reportTopOpcodes(writer, 20);

        // Top opcode pairs (superinstruction candidates)
        try self.reportTopPairs(writer, 20);

        // Hot IPs
        try self.reportHotIPs(writer, 10);
    }

    fn reportTopOpcodes(self: *Self, writer: anytype, limit: usize) !void {
        try writer.print("TOP OPCODES\n", .{});
        try writer.print("───────────────────────────────────────────────────────────────\n", .{});

        // Sort opcodes by count
        var sorted: [OPCODE_COUNT]struct { opcode: Opcode, count: u64 } = undefined;
        for (0..OPCODE_COUNT) |i| {
            sorted[i] = .{
                .opcode = @enumFromInt(i),
                .count = self.opcode_counts[i],
            };
        }

        std.mem.sort(@TypeOf(sorted[0]), &sorted, {}, struct {
            fn lessThan(_: void, a: anytype, b: anytype) bool {
                return a.count > b.count; // Descending
            }
        }.lessThan);

        // Print top N
        const n = @min(limit, OPCODE_COUNT);
        for (sorted[0..n]) |entry| {
            if (entry.count == 0) break;
            const pct: f64 = if (self.total_instructions > 0)
                @as(f64, @floatFromInt(entry.count)) / @as(f64, @floatFromInt(self.total_instructions)) * 100.0
            else
                0;
            try writer.print("  {s:<20} {d:>12} ({d:>5.1}%)\n", .{
                @tagName(entry.opcode),
                entry.count,
                pct,
            });
        }
        try writer.print("\n", .{});
    }

    fn reportTopPairs(self: *Self, writer: anytype, limit: usize) !void {
        try writer.print("TOP OPCODE PAIRS (Superinstruction Candidates)\n", .{});
        try writer.print("───────────────────────────────────────────────────────────────\n", .{});

        // Collect all pairs into a list
        var pairs = std.ArrayList(struct { prev: Opcode, curr: Opcode, count: u64 }).init(self.allocator);
        defer pairs.deinit();

        for (0..OPCODE_COUNT) |prev_idx| {
            for (0..OPCODE_COUNT) |curr_idx| {
                const count = self.opcode_pairs[prev_idx][curr_idx];
                if (count > 0) {
                    pairs.append(.{
                        .prev = @enumFromInt(prev_idx),
                        .curr = @enumFromInt(curr_idx),
                        .count = count,
                    }) catch continue;
                }
            }
        }

        // Sort by count descending
        std.mem.sort(@TypeOf(pairs.items[0]), pairs.items, {}, struct {
            fn lessThan(_: void, a: anytype, b: anytype) bool {
                return a.count > b.count;
            }
        }.lessThan);

        // Print top N
        const n = @min(limit, pairs.items.len);
        for (pairs.items[0..n]) |entry| {
            const pct: f64 = if (self.total_instructions > 0)
                @as(f64, @floatFromInt(entry.count)) / @as(f64, @floatFromInt(self.total_instructions)) * 100.0
            else
                0;
            try writer.print("  {s:<20} -> {s:<20} {d:>10} ({d:>5.1}%)\n", .{
                @tagName(entry.prev),
                @tagName(entry.curr),
                entry.count,
                pct,
            });
        }
        try writer.print("\n", .{});
    }

    fn reportHotIPs(self: *Self, writer: anytype, limit: usize) !void {
        try writer.print("HOT INSTRUCTION ADDRESSES\n", .{});
        try writer.print("───────────────────────────────────────────────────────────────\n", .{});

        if (self.hot_ips.count() == 0) {
            try writer.print("  (IP tracking not enabled)\n\n", .{});
            return;
        }

        // Collect and sort
        var sorted = std.ArrayList(struct { ip: u32, count: u64 }).init(self.allocator);
        defer sorted.deinit();

        var iter = self.hot_ips.iterator();
        while (iter.next()) |entry| {
            sorted.append(.{ .ip = entry.key_ptr.*, .count = entry.value_ptr.* }) catch continue;
        }

        std.mem.sort(@TypeOf(sorted.items[0]), sorted.items, {}, struct {
            fn lessThan(_: void, a: anytype, b: anytype) bool {
                return a.count > b.count;
            }
        }.lessThan);

        const n = @min(limit, sorted.items.len);
        for (sorted.items[0..n]) |entry| {
            try writer.print("  0x{X:0>4}: {d:>12} executions\n", .{ entry.ip, entry.count });
        }
        try writer.print("\n", .{});
    }

    /// Export data as JSON for external analysis
    pub fn exportJson(self: *Self, writer: anytype) !void {
        try writer.print("{{\n", .{});
        try writer.print("  \"total_instructions\": {d},\n", .{self.total_instructions});
        try writer.print("  \"duration_ms\": {d},\n", .{self.end_time - self.start_time});

        // Opcodes
        try writer.print("  \"opcodes\": {{\n", .{});
        var first = true;
        for (0..OPCODE_COUNT) |i| {
            if (self.opcode_counts[i] > 0) {
                if (!first) try writer.print(",\n", .{});
                first = false;
                const op: Opcode = @enumFromInt(i);
                try writer.print("    \"{s}\": {d}", .{ @tagName(op), self.opcode_counts[i] });
            }
        }
        try writer.print("\n  }},\n", .{});

        // Pairs
        try writer.print("  \"pairs\": [\n", .{});
        first = true;
        for (0..OPCODE_COUNT) |prev_idx| {
            for (0..OPCODE_COUNT) |curr_idx| {
                const count = self.opcode_pairs[prev_idx][curr_idx];
                if (count > 0) {
                    if (!first) try writer.print(",\n", .{});
                    first = false;
                    const prev: Opcode = @enumFromInt(prev_idx);
                    const curr: Opcode = @enumFromInt(curr_idx);
                    try writer.print("    {{\"{s}\": \"{s}\", \"count\": {d}}}", .{
                        @tagName(prev),
                        @tagName(curr),
                        count,
                    });
                }
            }
        }
        try writer.print("\n  ]\n", .{});

        try writer.print("}}\n", .{});
    }
};

/// Quickening threshold - number of executions before specializing
pub const QUICKEN_THRESHOLD: u32 = 100;

/// Maximum deoptimizations before blacklisting
pub const MAX_DEOPT_COUNT: u32 = 3;

/// Type feedback for a single IP
/// Tracks which types have been observed at a particular instruction
pub const TypeFeedback = struct {
    /// Bit flags for observed types
    observed_types: u16,
    /// Total observations
    observation_count: u32,
    /// Is this monomorphic (single type seen)?
    monomorphic: bool,
    /// Has this site been quickened (specialized)?
    quickened: bool,
    /// Number of times this site has been deoptimized
    deopt_count: u32,
    /// Is this site blacklisted (stop trying to quicken)?
    blacklisted: bool,

    // Type tag bits (matching value.Tag)
    pub const TYPE_NULL: u16 = 1 << 0;
    pub const TYPE_BOOL: u16 = 1 << 1;
    pub const TYPE_INT: u16 = 1 << 2;
    pub const TYPE_DECIMAL: u16 = 1 << 3;
    pub const TYPE_FIXED_STRING: u16 = 1 << 4;
    pub const TYPE_STRING: u16 = 1 << 5;
    pub const TYPE_RECORD: u16 = 1 << 6;
    pub const TYPE_HANDLE: u16 = 1 << 7;

    pub fn init() TypeFeedback {
        return .{
            .observed_types = 0,
            .observation_count = 0,
            .monomorphic = true,
            .quickened = false,
            .deopt_count = 0,
            .blacklisted = false,
        };
    }

    /// Check if this site should be quickened
    pub fn shouldQuicken(self: TypeFeedback, threshold: u32) bool {
        return !self.blacklisted and
            !self.quickened and
            self.monomorphic and
            self.observation_count >= threshold;
    }

    /// Mark this site as quickened
    pub fn markQuickened(self: *TypeFeedback) void {
        self.quickened = true;
    }

    /// Record a deoptimization and possibly blacklist
    pub fn recordDeopt(self: *TypeFeedback) void {
        self.quickened = false;
        self.deopt_count += 1;
        if (self.deopt_count >= MAX_DEOPT_COUNT) {
            self.blacklisted = true;
        }
    }

    pub fn recordType(self: *TypeFeedback, type_tag: u8) void {
        const bit: u16 = @as(u16, 1) << @as(u4, @intCast(type_tag));
        const prev = self.observed_types;
        self.observed_types |= bit;
        self.observation_count += 1;

        // Check if still monomorphic
        if (prev != 0 and prev != bit) {
            self.monomorphic = false;
        }
    }

    /// Check if only integers have been observed
    pub fn isAlwaysInt(self: TypeFeedback) bool {
        return self.observed_types == TYPE_INT and self.observation_count > 0;
    }

    /// Check if only strings have been observed
    pub fn isAlwaysString(self: TypeFeedback) bool {
        return (self.observed_types == TYPE_STRING or
            self.observed_types == TYPE_FIXED_STRING or
            self.observed_types == (TYPE_STRING | TYPE_FIXED_STRING)) and
            self.observation_count > 0;
    }

    /// Get number of distinct types observed
    pub fn typeCount(self: TypeFeedback) u32 {
        return @popCount(self.observed_types);
    }
};

// ============================================================================
// Tests
// ============================================================================

test "profiler basic counting" {
    var profiler = Profiler.init(std.testing.allocator);
    defer profiler.deinit();

    profiler.recordOpcode(.movi);
    profiler.recordOpcode(.movi);
    profiler.recordOpcode(.add);

    try std.testing.expectEqual(@as(u64, 3), profiler.total_instructions);
    try std.testing.expectEqual(@as(u64, 2), profiler.opcode_counts[@intFromEnum(Opcode.movi)]);
    try std.testing.expectEqual(@as(u64, 1), profiler.opcode_counts[@intFromEnum(Opcode.add)]);
}

test "profiler pair tracking" {
    var profiler = Profiler.init(std.testing.allocator);
    defer profiler.deinit();

    // Sequence: movi -> movi -> add (load two immediates then add)
    profiler.recordOpcode(.movi);
    profiler.recordOpcode(.movi);
    profiler.recordOpcode(.add);

    // movi -> movi should have count 1
    try std.testing.expectEqual(
        @as(u64, 1),
        profiler.opcode_pairs[@intFromEnum(Opcode.movi)][@intFromEnum(Opcode.movi)],
    );

    // movi -> add should have count 1
    try std.testing.expectEqual(
        @as(u64, 1),
        profiler.opcode_pairs[@intFromEnum(Opcode.movi)][@intFromEnum(Opcode.add)],
    );
}

test "type feedback monomorphic" {
    var tf = TypeFeedback.init();

    tf.recordType(2); // TYPE_INT
    tf.recordType(2);
    tf.recordType(2);

    try std.testing.expect(tf.isAlwaysInt());
    try std.testing.expect(tf.monomorphic);
    try std.testing.expectEqual(@as(u32, 1), tf.typeCount());
}

test "type feedback polymorphic" {
    var tf = TypeFeedback.init();

    tf.recordType(2); // TYPE_INT
    tf.recordType(5); // TYPE_STRING

    try std.testing.expect(!tf.monomorphic);
    try std.testing.expectEqual(@as(u32, 2), tf.typeCount());
}
