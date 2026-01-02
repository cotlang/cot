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

// ============================================================================
// JIT Compilation Tiers
// ============================================================================

/// Compilation tier for a function
pub const CompilationTier = enum(u8) {
    /// Running in bytecode interpreter
    interpreter = 0,
    /// Baseline JIT - quick compilation, type guards
    baseline = 1,
    /// Optimizing JIT - slow compilation, full optimizations
    optimized = 2,
};

/// Thresholds for tier transitions
pub const TIER_THRESHOLDS = struct {
    /// Calls needed to trigger Tier 0 → Tier 1
    pub const baseline_calls: u32 = 50;
    /// Loop iterations needed to trigger Tier 0 → Tier 1
    pub const baseline_loops: u32 = 200;
    /// Calls needed to trigger Tier 1 → Tier 2
    pub const optimized_calls: u32 = 500;
    /// Loop iterations needed to trigger Tier 1 → Tier 2
    pub const optimized_loops: u32 = 2000;
};

/// Per-function profiling data for JIT compilation decisions
pub const FunctionProfile = struct {
    /// Number of times this function has been called
    call_count: u32 = 0,
    /// Total loop iterations within this function
    loop_iterations: u32 = 0,
    /// Number of times JIT code was deoptimized
    deopt_count: u32 = 0,
    /// Current compilation tier
    tier: CompilationTier = .interpreter,
    /// Is this function blacklisted from JIT compilation?
    blacklisted: bool = false,
    /// Pointer to compiled native code (null if interpreted)
    compiled_code: ?*anyopaque = null,
    /// Size of compiled code in bytes
    compiled_size: usize = 0,

    const Self = @This();

    /// Record a function call
    pub fn recordCall(self: *Self) void {
        self.call_count +|= 1; // Saturating add to prevent overflow
    }

    /// Record a loop iteration (backward jump)
    pub fn recordLoopIteration(self: *Self) void {
        self.loop_iterations +|= 1;
    }

    /// Record a deoptimization event
    pub fn recordDeopt(self: *Self) void {
        self.deopt_count += 1;
        // Fall back to interpreter
        self.tier = .interpreter;
        self.compiled_code = null;
        self.compiled_size = 0;
        // Blacklist after too many deopts
        if (self.deopt_count >= MAX_DEOPT_COUNT) {
            self.blacklisted = true;
        }
    }

    /// Check if this function should be compiled to baseline JIT
    pub fn shouldCompileBaseline(self: Self) bool {
        if (self.blacklisted) return false;
        if (self.tier != .interpreter) return false;
        return self.call_count >= TIER_THRESHOLDS.baseline_calls or
            self.loop_iterations >= TIER_THRESHOLDS.baseline_loops;
    }

    /// Check if this function should be compiled to optimizing JIT
    pub fn shouldCompileOptimized(self: Self) bool {
        if (self.blacklisted) return false;
        if (self.tier != .baseline) return false;
        return self.call_count >= TIER_THRESHOLDS.optimized_calls or
            self.loop_iterations >= TIER_THRESHOLDS.optimized_loops;
    }

    /// Mark function as compiled to a given tier
    pub fn markCompiled(self: *Self, tier: CompilationTier, code: *anyopaque, size: usize) void {
        self.tier = tier;
        self.compiled_code = code;
        self.compiled_size = size;
    }

    /// Get hotness score (for prioritizing compilation)
    pub fn hotnessScore(self: Self) u64 {
        // Weight calls more than loop iterations
        return @as(u64, self.call_count) * 10 + @as(u64, self.loop_iterations);
    }
};

/// JIT Profiler - manages function profiles and compilation decisions
pub const JITProfiler = struct {
    allocator: std.mem.Allocator,
    /// Function profiles indexed by (module_index, routine_index)
    /// Key: u32 where high 16 bits = module_index, low 16 bits = routine_index
    profiles: std.AutoHashMap(u32, FunctionProfile),
    /// Queue of functions ready for baseline compilation
    baseline_queue: std.ArrayListUnmanaged(u32),
    /// Queue of functions ready for optimizing compilation
    optimized_queue: std.ArrayListUnmanaged(u32),
    /// Total functions profiled
    total_functions: u32,
    /// Total JIT compilations performed
    compilations_baseline: u32,
    compilations_optimized: u32,
    /// Total deoptimizations
    total_deopts: u32,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .profiles = std.AutoHashMap(u32, FunctionProfile).init(allocator),
            .baseline_queue = .{},
            .optimized_queue = .{},
            .total_functions = 0,
            .compilations_baseline = 0,
            .compilations_optimized = 0,
            .total_deopts = 0,
        };
    }

    pub fn deinit(self: *Self) void {
        self.profiles.deinit();
        self.baseline_queue.deinit(self.allocator);
        self.optimized_queue.deinit(self.allocator);
    }

    /// Create a composite key from module and routine indices
    fn makeKey(module_index: u16, routine_index: u16) u32 {
        return (@as(u32, module_index) << 16) | @as(u32, routine_index);
    }

    /// Extract module index from composite key
    fn getModuleIndex(key: u32) u16 {
        return @truncate(key >> 16);
    }

    /// Extract routine index from composite key
    fn getRoutineIndex(key: u32) u16 {
        return @truncate(key);
    }

    /// Get or create profile for a function
    pub fn getProfile(self: *Self, module_index: u16, routine_index: u16) *FunctionProfile {
        const key = makeKey(module_index, routine_index);
        const result = self.profiles.getOrPut(key) catch {
            // Fallback: return a static profile (shouldn't happen in practice)
            return &fallback_profile;
        };
        if (!result.found_existing) {
            result.value_ptr.* = FunctionProfile{};
            self.total_functions += 1;
        }
        return result.value_ptr;
    }

    var fallback_profile: FunctionProfile = .{};

    /// Record a function call and check if compilation is needed
    pub fn recordCall(self: *Self, module_index: u16, routine_index: u16) ?CompilationTier {
        const profile = self.getProfile(module_index, routine_index);
        profile.recordCall();

        // Check if we should trigger compilation
        if (profile.shouldCompileBaseline()) {
            const key = makeKey(module_index, routine_index);
            self.baseline_queue.append(self.allocator, key) catch {};
            return .baseline;
        } else if (profile.shouldCompileOptimized()) {
            const key = makeKey(module_index, routine_index);
            self.optimized_queue.append(self.allocator, key) catch {};
            return .optimized;
        }
        return null;
    }

    /// Record a loop iteration (backward jump)
    pub fn recordLoop(self: *Self, module_index: u16, routine_index: u16) ?CompilationTier {
        const profile = self.getProfile(module_index, routine_index);
        profile.recordLoopIteration();

        // Check if we should trigger compilation
        if (profile.shouldCompileBaseline()) {
            const key = makeKey(module_index, routine_index);
            self.baseline_queue.append(self.allocator, key) catch {};
            return .baseline;
        } else if (profile.shouldCompileOptimized()) {
            const key = makeKey(module_index, routine_index);
            self.optimized_queue.append(self.allocator, key) catch {};
            return .optimized;
        }
        return null;
    }

    /// Record a deoptimization
    pub fn recordDeopt(self: *Self, module_index: u16, routine_index: u16) void {
        const profile = self.getProfile(module_index, routine_index);
        profile.recordDeopt();
        self.total_deopts += 1;
    }

    /// Get the next function to compile at baseline tier
    pub fn popBaselineCandidate(self: *Self) ?struct { module: u16, routine: u16 } {
        if (self.baseline_queue.items.len == 0) return null;
        const key = self.baseline_queue.orderedRemove(0);
        return .{
            .module = getModuleIndex(key),
            .routine = getRoutineIndex(key),
        };
    }

    /// Get the next function to compile at optimized tier
    pub fn popOptimizedCandidate(self: *Self) ?struct { module: u16, routine: u16 } {
        if (self.optimized_queue.items.len == 0) return null;
        const key = self.optimized_queue.orderedRemove(0);
        return .{
            .module = getModuleIndex(key),
            .routine = getRoutineIndex(key),
        };
    }

    /// Check if a function has compiled code
    pub fn hasCompiledCode(self: *Self, module_index: u16, routine_index: u16) bool {
        const key = makeKey(module_index, routine_index);
        if (self.profiles.get(key)) |profile| {
            return profile.compiled_code != null;
        }
        return false;
    }

    /// Get compiled code for a function
    pub fn getCompiledCode(self: *Self, module_index: u16, routine_index: u16) ?*anyopaque {
        const key = makeKey(module_index, routine_index);
        if (self.profiles.get(key)) |profile| {
            return profile.compiled_code;
        }
        return null;
    }

    /// Mark a function as compiled
    pub fn markCompiled(self: *Self, module_index: u16, routine_index: u16, tier: CompilationTier, code: *anyopaque, size: usize) void {
        const profile = self.getProfile(module_index, routine_index);
        profile.markCompiled(tier, code, size);
        switch (tier) {
            .baseline => self.compilations_baseline += 1,
            .optimized => self.compilations_optimized += 1,
            .interpreter => {},
        }
    }

    /// Generate JIT profiling report
    pub fn report(self: *Self, writer: anytype) !void {
        try writer.print("\n", .{});
        try writer.print("╔══════════════════════════════════════════════════════════════╗\n", .{});
        try writer.print("║                    JIT PROFILER REPORT                       ║\n", .{});
        try writer.print("╚══════════════════════════════════════════════════════════════╝\n", .{});
        try writer.print("\n", .{});

        try writer.print("SUMMARY\n", .{});
        try writer.print("───────────────────────────────────────────────────────────────\n", .{});
        try writer.print("  Functions profiled:      {d:>8}\n", .{self.total_functions});
        try writer.print("  Baseline compilations:   {d:>8}\n", .{self.compilations_baseline});
        try writer.print("  Optimized compilations:  {d:>8}\n", .{self.compilations_optimized});
        try writer.print("  Deoptimizations:         {d:>8}\n", .{self.total_deopts});
        try writer.print("  Pending baseline:        {d:>8}\n", .{self.baseline_queue.items.len});
        try writer.print("  Pending optimized:       {d:>8}\n", .{self.optimized_queue.items.len});
        try writer.print("\n", .{});

        // Top hot functions
        try self.reportHotFunctions(writer, 10);
    }

    fn reportHotFunctions(self: *Self, writer: anytype, limit: usize) !void {
        try writer.print("HOT FUNCTIONS (by call count)\n", .{});
        try writer.print("───────────────────────────────────────────────────────────────\n", .{});

        // Collect all profiles
        var sorted = std.ArrayList(struct { key: u32, profile: FunctionProfile }).init(self.allocator);
        defer sorted.deinit();

        var iter = self.profiles.iterator();
        while (iter.next()) |entry| {
            sorted.append(.{ .key = entry.key_ptr.*, .profile = entry.value_ptr.* }) catch continue;
        }

        // Sort by hotness
        std.mem.sort(@TypeOf(sorted.items[0]), sorted.items, {}, struct {
            fn lessThan(_: void, a: anytype, b: anytype) bool {
                return a.profile.hotnessScore() > b.profile.hotnessScore();
            }
        }.lessThan);

        const n = @min(limit, sorted.items.len);
        for (sorted.items[0..n]) |entry| {
            const mod = getModuleIndex(entry.key);
            const routine = getRoutineIndex(entry.key);
            try writer.print("  [{d}:{d}] calls={d:>8} loops={d:>8} tier={s:<12} score={d}\n", .{
                mod,
                routine,
                entry.profile.call_count,
                entry.profile.loop_iterations,
                @tagName(entry.profile.tier),
                entry.profile.hotnessScore(),
            });
        }
        try writer.print("\n", .{});
    }
};

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

test "function profile tier transitions" {
    var fp = FunctionProfile{};

    // Initially in interpreter tier
    try std.testing.expectEqual(CompilationTier.interpreter, fp.tier);
    try std.testing.expect(!fp.shouldCompileBaseline());

    // Record calls until threshold
    for (0..TIER_THRESHOLDS.baseline_calls) |_| {
        fp.recordCall();
    }

    try std.testing.expect(fp.shouldCompileBaseline());
    try std.testing.expect(!fp.shouldCompileOptimized());

    // Mark as compiled to baseline
    var dummy_code: u8 = 0;
    fp.markCompiled(.baseline, &dummy_code, 100);
    try std.testing.expectEqual(CompilationTier.baseline, fp.tier);
    try std.testing.expect(!fp.shouldCompileBaseline());

    // Record more calls until optimized threshold
    for (0..TIER_THRESHOLDS.optimized_calls) |_| {
        fp.recordCall();
    }

    try std.testing.expect(fp.shouldCompileOptimized());
}

test "function profile deoptimization" {
    var fp = FunctionProfile{};
    var dummy_code: u8 = 0;

    // Compile to baseline
    fp.markCompiled(.baseline, &dummy_code, 100);
    try std.testing.expectEqual(CompilationTier.baseline, fp.tier);

    // Deopt multiple times
    fp.recordDeopt();
    try std.testing.expectEqual(CompilationTier.interpreter, fp.tier);
    try std.testing.expect(!fp.blacklisted);

    fp.recordDeopt();
    fp.recordDeopt();

    // Should be blacklisted after MAX_DEOPT_COUNT
    try std.testing.expect(fp.blacklisted);
    try std.testing.expect(!fp.shouldCompileBaseline());
}

test "jit profiler function tracking" {
    var jit = JITProfiler.init(std.testing.allocator);
    defer jit.deinit();

    // Record calls for module 0, routine 0
    for (0..30) |_| {
        _ = jit.recordCall(0, 0);
    }

    const profile = jit.getProfile(0, 0);
    try std.testing.expectEqual(@as(u32, 30), profile.call_count);
    try std.testing.expectEqual(@as(u32, 1), jit.total_functions);

    // Record calls for different routine
    for (0..20) |_| {
        _ = jit.recordCall(0, 1);
    }

    try std.testing.expectEqual(@as(u32, 2), jit.total_functions);
}

test "jit profiler compilation queue" {
    var jit = JITProfiler.init(std.testing.allocator);
    defer jit.deinit();

    // Record enough calls to trigger baseline compilation
    for (0..TIER_THRESHOLDS.baseline_calls) |_| {
        const result = jit.recordCall(0, 5);
        if (result) |tier| {
            try std.testing.expectEqual(CompilationTier.baseline, tier);
        }
    }

    // Should have one candidate in baseline queue
    try std.testing.expectEqual(@as(usize, 1), jit.baseline_queue.items.len);

    // Pop the candidate
    const candidate = jit.popBaselineCandidate();
    try std.testing.expect(candidate != null);
    try std.testing.expectEqual(@as(u16, 0), candidate.?.module);
    try std.testing.expectEqual(@as(u16, 5), candidate.?.routine);

    // Queue should be empty now
    try std.testing.expectEqual(@as(usize, 0), jit.baseline_queue.items.len);
}

test "jit profiler loop detection" {
    var jit = JITProfiler.init(std.testing.allocator);
    defer jit.deinit();

    // Record loop iterations
    for (0..TIER_THRESHOLDS.baseline_loops) |_| {
        const result = jit.recordLoop(1, 2);
        if (result) |tier| {
            try std.testing.expectEqual(CompilationTier.baseline, tier);
        }
    }

    const profile = jit.getProfile(1, 2);
    try std.testing.expectEqual(@as(u32, TIER_THRESHOLDS.baseline_loops), profile.loop_iterations);
}
