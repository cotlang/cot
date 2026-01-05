//! Unified Breakpoint Manager
//!
//! Provides breakpoint management shared between:
//! - CLI trace/debug commands
//! - LSP/Debug Adapter Protocol (DAP) integration
//! - VSCode extension
//!
//! Supports multiple breakpoint types for flexible debugging.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Types of breakpoints supported
pub const BreakpointKind = enum {
    /// Break at specific instruction pointer (IP)
    address,
    /// Break at source line number
    line,
    /// Break when entering a routine
    routine_entry,
    /// Break when exiting a routine
    routine_exit,
    /// Break on specific opcode execution
    opcode,
};

/// Action to take when breakpoint is hit
pub const BreakpointAction = enum {
    /// Pause execution (default)
    pause,
    /// Log message and continue
    log,
    /// Enable tracing and continue
    trace_on,
    /// Disable tracing and continue
    trace_off,
};

/// A single breakpoint definition
pub const Breakpoint = struct {
    /// Unique identifier for this breakpoint
    id: u32,
    /// Type of breakpoint
    kind: BreakpointKind,
    /// Whether breakpoint is active
    enabled: bool = true,
    /// Number of times this breakpoint has been hit
    hit_count: u32 = 0,
    /// Action to take when hit
    action: BreakpointAction = .pause,
    /// Optional condition expression (for conditional breakpoints)
    condition: ?[]const u8 = null,
    /// Optional hit count threshold (only break after N hits)
    hit_count_threshold: u32 = 0,

    /// Kind-specific data
    data: union(BreakpointKind) {
        address: u32,
        line: struct {
            file: ?[]const u8,
            line: u32,
        },
        routine_entry: []const u8,
        routine_exit: []const u8,
        opcode: u8, // Opcode value
    },

    /// Check if this breakpoint matches the given state
    pub fn matches(self: *const Breakpoint, state: BreakpointState) bool {
        if (!self.enabled) return false;

        return switch (self.kind) {
            .address => self.data.address == state.ip,
            .line => blk: {
                if (self.data.line.line != state.line) break :blk false;
                // If file is specified, must match
                if (self.data.line.file) |file| {
                    if (state.file) |state_file| {
                        break :blk std.mem.eql(u8, file, state_file);
                    }
                    break :blk false;
                }
                break :blk true;
            },
            .routine_entry => blk: {
                if (!state.is_call) break :blk false;
                if (state.routine) |r| {
                    break :blk std.mem.eql(u8, self.data.routine_entry, r);
                }
                break :blk false;
            },
            .routine_exit => blk: {
                if (!state.is_return) break :blk false;
                if (state.routine) |r| {
                    break :blk std.mem.eql(u8, self.data.routine_exit, r);
                }
                break :blk false;
            },
            .opcode => self.data.opcode == state.opcode,
        };
    }

    /// Record a hit and return whether to actually break
    pub fn recordHit(self: *Breakpoint) bool {
        self.hit_count += 1;
        // Check hit count threshold
        if (self.hit_count_threshold > 0 and self.hit_count < self.hit_count_threshold) {
            return false;
        }
        return self.action == .pause;
    }
};

/// State passed to breakpoint checking
pub const BreakpointState = struct {
    ip: u32 = 0,
    line: u32 = 0,
    file: ?[]const u8 = null,
    routine: ?[]const u8 = null,
    opcode: u8 = 0,
    is_call: bool = false,
    is_return: bool = false,
    call_depth: u32 = 0,
};

/// Result of checking breakpoints
pub const BreakpointResult = struct {
    /// Breakpoint that was hit (if any)
    breakpoint: ?*Breakpoint = null,
    /// Whether to pause execution
    should_pause: bool = false,
    /// Whether to enable tracing
    enable_tracing: bool = false,
    /// Whether to disable tracing
    disable_tracing: bool = false,
};

/// Unified breakpoint manager
/// Can be used by CLI, LSP, and VSCode debugging
pub const BreakpointManager = struct {
    allocator: Allocator,
    /// All breakpoints indexed by ID
    breakpoints: std.AutoHashMap(u32, Breakpoint),
    /// Fast lookup by IP for address breakpoints
    ip_breakpoints: std.AutoHashMap(u32, u32), // IP -> breakpoint ID
    /// Fast lookup by line for line breakpoints
    line_breakpoints: std.AutoHashMap(u32, u32), // line -> breakpoint ID
    /// Next breakpoint ID to assign
    next_id: u32,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .breakpoints = std.AutoHashMap(u32, Breakpoint).init(allocator),
            .ip_breakpoints = std.AutoHashMap(u32, u32).init(allocator),
            .line_breakpoints = std.AutoHashMap(u32, u32).init(allocator),
            .next_id = 1,
        };
    }

    pub fn deinit(self: *Self) void {
        self.breakpoints.deinit();
        self.ip_breakpoints.deinit();
        self.line_breakpoints.deinit();
    }

    /// Add a breakpoint and return its ID
    pub fn add(self: *Self, bp: Breakpoint) !u32 {
        var new_bp = bp;
        new_bp.id = self.next_id;
        self.next_id += 1;

        // Add to main map
        try self.breakpoints.put(new_bp.id, new_bp);

        // Add to fast lookup indexes
        switch (bp.kind) {
            .address => try self.ip_breakpoints.put(bp.data.address, new_bp.id),
            .line => try self.line_breakpoints.put(bp.data.line.line, new_bp.id),
            else => {},
        }

        return new_bp.id;
    }

    /// Add a breakpoint at a specific IP address
    pub fn addAtAddress(self: *Self, ip: u32) !u32 {
        return self.add(.{
            .id = 0, // Will be assigned
            .kind = .address,
            .data = .{ .address = ip },
        });
    }

    /// Add a breakpoint at a source line
    pub fn addAtLine(self: *Self, line: u32, file: ?[]const u8) !u32 {
        return self.add(.{
            .id = 0,
            .kind = .line,
            .data = .{ .line = .{ .file = file, .line = line } },
        });
    }

    /// Add a breakpoint at routine entry
    pub fn addAtRoutineEntry(self: *Self, routine_name: []const u8) !u32 {
        return self.add(.{
            .id = 0,
            .kind = .routine_entry,
            .data = .{ .routine_entry = routine_name },
        });
    }

    /// Add a breakpoint at routine exit
    pub fn addAtRoutineExit(self: *Self, routine_name: []const u8) !u32 {
        return self.add(.{
            .id = 0,
            .kind = .routine_exit,
            .data = .{ .routine_exit = routine_name },
        });
    }

    /// Remove a breakpoint by ID
    pub fn remove(self: *Self, id: u32) bool {
        if (self.breakpoints.get(id)) |bp| {
            // Remove from fast lookup indexes
            switch (bp.kind) {
                .address => _ = self.ip_breakpoints.remove(bp.data.address),
                .line => _ = self.line_breakpoints.remove(bp.data.line.line),
                else => {},
            }
            _ = self.breakpoints.remove(id);
            return true;
        }
        return false;
    }

    /// Remove all breakpoints
    pub fn clear(self: *Self) void {
        self.breakpoints.clearRetainingCapacity();
        self.ip_breakpoints.clearRetainingCapacity();
        self.line_breakpoints.clearRetainingCapacity();
    }

    /// Enable/disable a breakpoint
    pub fn setEnabled(self: *Self, id: u32, enabled: bool) bool {
        if (self.breakpoints.getPtr(id)) |bp| {
            bp.enabled = enabled;
            return true;
        }
        return false;
    }

    /// Get a breakpoint by ID
    pub fn get(self: *Self, id: u32) ?*const Breakpoint {
        return self.breakpoints.getPtr(id);
    }

    /// Get mutable breakpoint by ID
    pub fn getMut(self: *Self, id: u32) ?*Breakpoint {
        return self.breakpoints.getPtr(id);
    }

    /// Check if any breakpoint matches the current state
    /// Returns result indicating what action to take
    pub fn check(self: *Self, state: BreakpointState) BreakpointResult {
        var result = BreakpointResult{};

        // Fast path: check IP index first
        if (self.ip_breakpoints.get(state.ip)) |id| {
            if (self.breakpoints.getPtr(id)) |bp| {
                if (bp.matches(state)) {
                    if (bp.recordHit()) {
                        result.breakpoint = bp;
                        result.should_pause = true;
                    }
                    switch (bp.action) {
                        .pause => result.should_pause = true,
                        .trace_on => result.enable_tracing = true,
                        .trace_off => result.disable_tracing = true,
                        .log => {},
                    }
                    return result;
                }
            }
        }

        // Fast path: check line index
        if (self.line_breakpoints.get(state.line)) |id| {
            if (self.breakpoints.getPtr(id)) |bp| {
                if (bp.matches(state)) {
                    if (bp.recordHit()) {
                        result.breakpoint = bp;
                        result.should_pause = true;
                    }
                    switch (bp.action) {
                        .pause => result.should_pause = true,
                        .trace_on => result.enable_tracing = true,
                        .trace_off => result.disable_tracing = true,
                        .log => {},
                    }
                    return result;
                }
            }
        }

        // Slow path: check all breakpoints for routine/opcode types
        var iter = self.breakpoints.iterator();
        while (iter.next()) |entry| {
            const bp = entry.value_ptr;
            if (bp.kind == .routine_entry or bp.kind == .routine_exit or bp.kind == .opcode) {
                if (bp.matches(state)) {
                    if (bp.recordHit()) {
                        result.breakpoint = bp;
                        result.should_pause = true;
                    }
                    switch (bp.action) {
                        .pause => result.should_pause = true,
                        .trace_on => result.enable_tracing = true,
                        .trace_off => result.disable_tracing = true,
                        .log => {},
                    }
                    return result;
                }
            }
        }

        return result;
    }

    /// Quick check if IP has a breakpoint (for hot path)
    pub fn hasBreakpointAtIP(self: *const Self, ip: u32) bool {
        return self.ip_breakpoints.contains(ip);
    }

    /// Quick check if line has a breakpoint
    pub fn hasBreakpointAtLine(self: *const Self, line: u32) bool {
        return self.line_breakpoints.contains(line);
    }

    /// Get number of breakpoints
    pub fn count(self: *const Self) usize {
        return self.breakpoints.count();
    }

    /// Iterator over all breakpoints
    pub fn iterator(self: *const Self) std.AutoHashMap(u32, Breakpoint).Iterator {
        return self.breakpoints.iterator();
    }

    /// Get all breakpoints as a slice (for serialization)
    pub fn toSlice(self: *const Self, allocator: Allocator) ![]Breakpoint {
        var list = std.ArrayList(Breakpoint).init(allocator);
        errdefer list.deinit();

        var iter = self.breakpoints.iterator();
        while (iter.next()) |entry| {
            try list.append(entry.value_ptr.*);
        }

        return list.toOwnedSlice();
    }
};

// ============================================================================
// Tests
// ============================================================================

test "BreakpointManager: add and check IP breakpoint" {
    const allocator = std.testing.allocator;
    var mgr = BreakpointManager.init(allocator);
    defer mgr.deinit();

    const id = try mgr.addAtAddress(0x0100);
    try std.testing.expect(id > 0);

    // Should match
    const result = mgr.check(.{ .ip = 0x0100 });
    try std.testing.expect(result.should_pause);
    try std.testing.expect(result.breakpoint != null);

    // Should not match different IP
    const result2 = mgr.check(.{ .ip = 0x0200 });
    try std.testing.expect(!result2.should_pause);
}

test "BreakpointManager: add and check line breakpoint" {
    const allocator = std.testing.allocator;
    var mgr = BreakpointManager.init(allocator);
    defer mgr.deinit();

    const id = try mgr.addAtLine(42, null);
    try std.testing.expect(id > 0);

    // Should match
    const result = mgr.check(.{ .line = 42 });
    try std.testing.expect(result.should_pause);

    // Should not match different line
    const result2 = mgr.check(.{ .line = 43 });
    try std.testing.expect(!result2.should_pause);
}

test "BreakpointManager: disable breakpoint" {
    const allocator = std.testing.allocator;
    var mgr = BreakpointManager.init(allocator);
    defer mgr.deinit();

    const id = try mgr.addAtAddress(0x0100);

    // Disable it
    try std.testing.expect(mgr.setEnabled(id, false));

    // Should not match when disabled
    const result = mgr.check(.{ .ip = 0x0100 });
    try std.testing.expect(!result.should_pause);

    // Re-enable
    try std.testing.expect(mgr.setEnabled(id, true));

    // Should match again
    const result2 = mgr.check(.{ .ip = 0x0100 });
    try std.testing.expect(result2.should_pause);
}

test "BreakpointManager: hit count" {
    const allocator = std.testing.allocator;
    var mgr = BreakpointManager.init(allocator);
    defer mgr.deinit();

    const id = try mgr.addAtAddress(0x0100);

    // Hit it multiple times
    _ = mgr.check(.{ .ip = 0x0100 });
    _ = mgr.check(.{ .ip = 0x0100 });
    _ = mgr.check(.{ .ip = 0x0100 });

    // Check hit count
    const bp = mgr.get(id).?;
    try std.testing.expectEqual(@as(u32, 3), bp.hit_count);
}

test "BreakpointManager: routine entry breakpoint" {
    const allocator = std.testing.allocator;
    var mgr = BreakpointManager.init(allocator);
    defer mgr.deinit();

    _ = try mgr.addAtRoutineEntry("main");

    // Should match on call to main
    const result = mgr.check(.{ .routine = "main", .is_call = true });
    try std.testing.expect(result.should_pause);

    // Should not match other routines
    const result2 = mgr.check(.{ .routine = "helper", .is_call = true });
    try std.testing.expect(!result2.should_pause);

    // Should not match return
    const result3 = mgr.check(.{ .routine = "main", .is_return = true });
    try std.testing.expect(!result3.should_pause);
}
