//! DBL Preprocessor
//!
//! Handles text-level preprocessing before lexing:
//!   .define SYMBOL        - Define a flag
//!   .define SYMBOL ,value - Define a constant
//!   .ifdef SYMBOL         - Compile if defined
//!   .ifndef SYMBOL        - Compile if not defined
//!   .if expression        - Compile if expression true
//!   .else                 - Else branch
//!   .endc                 - End conditional
//!   .include "file"       - Textual file inclusion
//!   .undefine SYMBOL      - Remove definition
//!
//! Built-in symbols (auto-defined):
//!   OS_MACOS, OS_UNIX, OS_WINDOWS
//!   D_MACOS64, D_UNIX64, D_WIN64
//!   COT (always defined)

const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Preprocessor = struct {
    allocator: Allocator,
    source: []const u8,
    pos: usize,
    line: usize,
    defines: std.StringHashMap([]const u8),
    include_paths: std.ArrayListUnmanaged([]const u8),
    output: std.ArrayListUnmanaged(u8),
    current_file: []const u8,

    // Logical include paths (from cot.json includePaths)
    // Maps logical name -> directory path (e.g., "comdef" -> "/path/to/defines")
    logical_paths: std.StringHashMap([]const u8),

    // Project root directory (for resolving relative paths)
    project_root: ?[]const u8,

    // Conditional compilation state
    cond_stack: std.ArrayListUnmanaged(CondState),

    const CondState = struct {
        active: bool, // Are we in the active branch?
        had_true: bool, // Have we seen a true branch? (for else)
        parent_active: bool, // Was parent context active?
    };

    pub const Error = error{
        UnterminatedConditional,
        UnmatchedElse,
        UnmatchedEndc,
        InvalidDirective,
        IncludeNotFound,
        IncludeTooDeep,
        OutOfMemory,
        InvalidExpression,
    };

    pub fn init(allocator: Allocator, source: []const u8, filename: []const u8) Preprocessor {
        return Preprocessor{
            .allocator = allocator,
            .source = source,
            .pos = 0,
            .line = 1,
            .defines = std.StringHashMap([]const u8).init(allocator),
            .include_paths = .empty,
            .output = .empty,
            .current_file = filename,
            .logical_paths = std.StringHashMap([]const u8).init(allocator),
            .project_root = null,
            .cond_stack = .empty,
        };
    }

    pub fn deinit(self: *Preprocessor) void {
        // Free owned define values
        var it = self.defines.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.*.len > 0) {
                self.allocator.free(entry.value_ptr.*);
            }
            self.allocator.free(entry.key_ptr.*);
        }
        self.defines.deinit();
        self.include_paths.deinit(self.allocator);
        self.output.deinit(self.allocator);
        self.cond_stack.deinit(self.allocator);

        // Free logical paths (we own the values, not the keys which come from config)
        var lp_it = self.logical_paths.iterator();
        while (lp_it.next()) |entry| {
            self.allocator.free(entry.value_ptr.*);
        }
        self.logical_paths.deinit();
    }

    /// Add built-in platform symbols
    pub fn addBuiltinDefines(self: *Preprocessor) !void {
        // Always defined - identifies Cot compiler
        try self.addDefine("COT", "1");

        // Platform detection
        const os = @import("builtin").os.tag;
        switch (os) {
            .macos => {
                try self.addDefine("OS_MACOS", "1");
                try self.addDefine("OS_UNIX", "1");
                try self.addDefine("D_MACOS64", "1");
                try self.addDefine("D_UNIX64", "1");
            },
            .linux => {
                try self.addDefine("OS_LINUX", "1");
                try self.addDefine("OS_UNIX", "1");
                try self.addDefine("D_UNIX64", "1");
            },
            .windows => {
                try self.addDefine("OS_WINDOWS", "1");
                try self.addDefine("D_WIN64", "1");
            },
            else => {},
        }

        // Architecture
        const arch = @import("builtin").cpu.arch;
        switch (arch) {
            .x86_64 => try self.addDefine("ARCH_X64", "1"),
            .aarch64 => try self.addDefine("ARCH_ARM64", "1"),
            else => {},
        }
    }

    /// Add a define from external source (CLI -D flag)
    pub fn addDefine(self: *Preprocessor, name: []const u8, value: []const u8) !void {
        const owned_name = try self.allocator.dupe(u8, name);
        errdefer self.allocator.free(owned_name);

        const owned_value = if (value.len > 0)
            try self.allocator.dupe(u8, value)
        else
            "";

        try self.defines.put(owned_name, owned_value);
    }

    /// Add an include search path
    pub fn addIncludePath(self: *Preprocessor, path: []const u8) !void {
        try self.include_paths.append(self.allocator, path);
    }

    /// Set the project root directory (for resolving relative paths)
    pub fn setProjectRoot(self: *Preprocessor, root: []const u8) void {
        self.project_root = root;
    }

    /// Add a logical include path (from cot.json includePaths)
    /// The path will have ${ENV_VAR} expanded and relative paths resolved against project_root
    pub fn addLogicalPath(self: *Preprocessor, name: []const u8, raw_path: []const u8) !void {
        // Expand environment variables in the path
        const expanded = try self.expandEnvVars(raw_path);
        errdefer self.allocator.free(expanded);

        // Resolve relative paths against project root
        const resolved = try self.resolvePath(expanded);
        self.allocator.free(expanded);

        try self.logical_paths.put(name, resolved);
    }

    /// Expand ${VAR} environment variable references in a string
    fn expandEnvVars(self: *Preprocessor, input: []const u8) ![]u8 {
        var result: std.ArrayListUnmanaged(u8) = .empty;
        errdefer result.deinit(self.allocator);

        var i: usize = 0;
        while (i < input.len) {
            if (i + 1 < input.len and input[i] == '$' and input[i + 1] == '{') {
                // Find closing brace
                const start = i + 2;
                var end = start;
                while (end < input.len and input[end] != '}') {
                    end += 1;
                }
                if (end < input.len) {
                    const var_name = input[start..end];
                    // Look up environment variable
                    if (std.posix.getenv(var_name)) |value| {
                        try result.appendSlice(self.allocator, value);
                    }
                    // Skip past ${VAR}
                    i = end + 1;
                } else {
                    // No closing brace, copy literally
                    try result.append(self.allocator, input[i]);
                    i += 1;
                }
            } else {
                try result.append(self.allocator, input[i]);
                i += 1;
            }
        }

        return try result.toOwnedSlice(self.allocator);
    }

    /// Resolve a path relative to project root (if relative)
    fn resolvePath(self: *Preprocessor, path: []const u8) ![]u8 {
        // Check if path is already absolute
        if (path.len > 0 and (path[0] == '/' or (path.len > 1 and path[1] == ':'))) {
            return try self.allocator.dupe(u8, path);
        }

        // If we have a project root and path is relative, join them
        if (self.project_root) |root| {
            return try std.fs.path.join(self.allocator, &.{ root, path });
        }

        // No project root, return as-is
        return try self.allocator.dupe(u8, path);
    }

    /// Check if currently in an active (non-skipped) code section
    fn isActive(self: *Preprocessor) bool {
        if (self.cond_stack.items.len == 0) return true;
        const top = self.cond_stack.items[self.cond_stack.items.len - 1];
        return top.active and top.parent_active;
    }

    /// Process the source and return preprocessed output
    pub fn process(self: *Preprocessor) Error![]const u8 {
        while (self.pos < self.source.len) {
            const line_start = self.pos;
            const line_end = self.findLineEnd();
            const line = self.source[line_start..line_end];

            // Skip to content (past whitespace)
            const trimmed = std.mem.trimLeft(u8, line, " \t");

            if (trimmed.len > 0 and trimmed[0] == '.') {
                // This is a directive
                try self.handleDirective(trimmed);
            } else if (self.isActive()) {
                // Regular code line - apply text replacement and output
                try self.outputLine(line);
            }
            // else: Skip line (in inactive conditional branch)

            // Move past the line
            self.pos = line_end;
            if (self.pos < self.source.len and self.source[self.pos] == '\n') {
                self.pos += 1;
                self.line += 1;
            }
        }

        // Check for unterminated conditionals
        if (self.cond_stack.items.len > 0) {
            return Error.UnterminatedConditional;
        }

        return self.output.items;
    }

    fn findLineEnd(self: *Preprocessor) usize {
        var end = self.pos;
        while (end < self.source.len and self.source[end] != '\n') {
            end += 1;
        }
        return end;
    }

    fn handleDirective(self: *Preprocessor, line: []const u8) Error!void {
        // Parse directive name
        var i: usize = 1; // Skip the '.'
        while (i < line.len and (std.ascii.isAlphabetic(line[i]) or line[i] == '_')) {
            i += 1;
        }
        const directive_raw = line[1..i];

        // Lowercase for comparison
        var lower_buf: [32]u8 = undefined;
        const dir_len = @min(directive_raw.len, lower_buf.len);
        const directive = std.ascii.lowerString(lower_buf[0..dir_len], directive_raw[0..dir_len]);

        const rest = std.mem.trimLeft(u8, line[i..], " \t");

        // Handle conditional directives (always processed to track nesting)
        if (std.mem.eql(u8, directive, "ifdef")) {
            try self.handleIfdef(rest, false);
            return;
        } else if (std.mem.eql(u8, directive, "ifndef")) {
            try self.handleIfdef(rest, true);
            return;
        } else if (std.mem.eql(u8, directive, "if")) {
            try self.handleIf(rest);
            return;
        } else if (std.mem.eql(u8, directive, "else")) {
            try self.handleElse();
            return;
        } else if (std.mem.eql(u8, directive, "endc") or std.mem.eql(u8, directive, "end")) {
            try self.handleEndc();
            return;
        }

        // Other directives only processed if active
        if (!self.isActive()) return;

        if (std.mem.eql(u8, directive, "define")) {
            try self.handleDefine(rest);
        } else if (std.mem.eql(u8, directive, "undefine")) {
            try self.handleUndefine(rest);
        } else if (std.mem.eql(u8, directive, "include")) {
            try self.handleInclude(rest);
        }
        // Ignore unknown directives (listing controls, etc.)
    }

    fn handleDefine(self: *Preprocessor, rest: []const u8) Error!void {
        // Parse: SYMBOL or SYMBOL ,value or SYMBOL, value
        var name_end: usize = 0;
        while (name_end < rest.len and
            (std.ascii.isAlphanumeric(rest[name_end]) or rest[name_end] == '_'))
        {
            name_end += 1;
        }

        if (name_end == 0) return Error.InvalidDirective;

        const name = rest[0..name_end];
        var value: []const u8 = "1"; // Default value for flag

        // Check for value
        var value_start = name_end;
        // Skip whitespace and optional comma
        while (value_start < rest.len and (rest[value_start] == ' ' or rest[value_start] == '\t')) {
            value_start += 1;
        }
        if (value_start < rest.len and rest[value_start] == ',') {
            value_start += 1;
            // Skip whitespace after comma
            while (value_start < rest.len and (rest[value_start] == ' ' or rest[value_start] == '\t')) {
                value_start += 1;
            }
            // Rest is the value (trim trailing comment if present)
            var value_end = value_start;
            while (value_end < rest.len and rest[value_end] != ';') {
                value_end += 1;
            }
            value = std.mem.trimRight(u8, rest[value_start..value_end], " \t");
        }

        // Store the define (uppercase the name for case-insensitive lookup)
        var upper_name: [64]u8 = undefined;
        const len = @min(name.len, upper_name.len);
        _ = std.ascii.upperString(upper_name[0..len], name[0..len]);

        try self.addDefine(upper_name[0..len], value);
    }

    fn handleUndefine(self: *Preprocessor, rest: []const u8) Error!void {
        var name_end: usize = 0;
        while (name_end < rest.len and
            (std.ascii.isAlphanumeric(rest[name_end]) or rest[name_end] == '_'))
        {
            name_end += 1;
        }

        if (name_end == 0) return Error.InvalidDirective;

        const name = rest[0..name_end];

        // Uppercase for lookup
        var upper_name: [64]u8 = undefined;
        const len = @min(name.len, upper_name.len);
        _ = std.ascii.upperString(upper_name[0..len], name[0..len]);

        if (self.defines.fetchRemove(upper_name[0..len])) |kv| {
            if (kv.value.len > 0) {
                self.allocator.free(kv.value);
            }
            self.allocator.free(kv.key);
        }
    }

    fn handleIfdef(self: *Preprocessor, rest: []const u8, negate: bool) Error!void {
        // Parse symbol name
        var name_end: usize = 0;
        while (name_end < rest.len and
            (std.ascii.isAlphanumeric(rest[name_end]) or rest[name_end] == '_'))
        {
            name_end += 1;
        }

        const name = rest[0..name_end];

        // Uppercase for lookup
        var upper_name: [64]u8 = undefined;
        const len = @min(name.len, upper_name.len);
        _ = std.ascii.upperString(upper_name[0..len], name[0..len]);

        const defined = self.defines.contains(upper_name[0..len]);
        const condition = if (negate) !defined else defined;

        const parent_active = self.isActive();

        try self.cond_stack.append(self.allocator, .{
            .active = condition,
            .had_true = condition,
            .parent_active = parent_active,
        });
    }

    fn handleIf(self: *Preprocessor, rest: []const u8) Error!void {
        // Simple expression evaluation: just check if symbol is defined and truthy
        // For now, treat as .ifdef (can extend later for expressions)
        var name_end: usize = 0;
        while (name_end < rest.len and
            (std.ascii.isAlphanumeric(rest[name_end]) or rest[name_end] == '_'))
        {
            name_end += 1;
        }

        const name = rest[0..name_end];

        // Uppercase for lookup
        var upper_name: [64]u8 = undefined;
        const len = @min(name.len, upper_name.len);
        _ = std.ascii.upperString(upper_name[0..len], name[0..len]);

        // Check if defined and has truthy value
        var condition = false;
        if (self.defines.get(upper_name[0..len])) |value| {
            // Truthy: defined and not "0" or empty
            condition = value.len > 0 and !std.mem.eql(u8, value, "0");
        }

        const parent_active = self.isActive();

        try self.cond_stack.append(self.allocator, .{
            .active = condition,
            .had_true = condition,
            .parent_active = parent_active,
        });
    }

    fn handleElse(self: *Preprocessor) Error!void {
        if (self.cond_stack.items.len == 0) {
            return Error.UnmatchedElse;
        }

        const top = &self.cond_stack.items[self.cond_stack.items.len - 1];

        // Switch to else branch: active if parent active AND we haven't had a true branch
        top.active = top.parent_active and !top.had_true;
        top.had_true = true; // Mark that we've handled both branches
    }

    fn handleEndc(self: *Preprocessor) Error!void {
        if (self.cond_stack.items.len == 0) {
            return Error.UnmatchedEndc;
        }
        _ = self.cond_stack.pop();
    }

    fn handleInclude(self: *Preprocessor, rest: []const u8) Error!void {
        // Parse filename: "filename" or 'filename' or filename
        // Supports logical path syntax: 'logical:filename' or "logical:filename"
        var filename: []const u8 = undefined;
        var start: usize = 0;

        if (rest.len > 0 and (rest[0] == '"' or rest[0] == '\'')) {
            // Quoted filename (single or double quotes)
            const quote = rest[0];
            start = 1;
            var end = start;
            while (end < rest.len and rest[end] != quote) {
                end += 1;
            }
            filename = rest[start..end];
        } else {
            // Unquoted filename
            var end: usize = 0;
            while (end < rest.len and !std.ascii.isWhitespace(rest[end])) {
                end += 1;
            }
            filename = rest[0..end];
        }

        if (filename.len == 0) {
            return Error.InvalidDirective;
        }

        // Check for logical path syntax: "logical:filename"
        var resolved_path: ?[]const u8 = null;
        defer if (resolved_path) |p| self.allocator.free(p);

        if (std.mem.indexOf(u8, filename, ":")) |colon_idx| {
            // Extract logical name and actual filename
            const logical_name = filename[0..colon_idx];
            const actual_filename = filename[colon_idx + 1 ..];

            // Look up logical path
            if (self.logical_paths.get(logical_name)) |base_path| {
                // Join the base path with the filename
                resolved_path = std.fs.path.join(self.allocator, &.{ base_path, actual_filename }) catch {
                    return Error.OutOfMemory;
                };
            } else {
                // Unknown logical name - could be a Windows path like C:\ so try as-is
                // Only treat as logical if it's a short name (< 2 chars would be drive letter)
                if (colon_idx > 1) {
                    // Likely a logical path that's not defined
                    return Error.IncludeNotFound;
                }
            }
        }

        // Try to find and include the file
        const search_path = resolved_path orelse filename;
        const content = self.findAndReadInclude(search_path) catch {
            return Error.IncludeNotFound;
        };
        defer self.allocator.free(content);

        // Recursively preprocess the included content
        var sub_pp = Preprocessor.init(self.allocator, content, filename);
        defer sub_pp.deinit();

        // Copy project root and logical paths to sub-preprocessor
        sub_pp.project_root = self.project_root;
        var lp_it = self.logical_paths.iterator();
        while (lp_it.next()) |entry| {
            // We need to dupe the value since sub_pp will try to free it
            const val_copy = sub_pp.allocator.dupe(u8, entry.value_ptr.*) catch continue;
            sub_pp.logical_paths.put(entry.key_ptr.*, val_copy) catch {
                sub_pp.allocator.free(val_copy);
            };
        }

        // Copy current defines to sub-preprocessor
        var it = self.defines.iterator();
        while (it.next()) |entry| {
            sub_pp.addDefine(entry.key_ptr.*, entry.value_ptr.*) catch {};
        }

        // Copy include paths
        for (self.include_paths.items) |path| {
            sub_pp.addIncludePath(path) catch {};
        }

        // Copy condition stack (include inherits conditional state)
        for (self.cond_stack.items) |state| {
            sub_pp.cond_stack.append(sub_pp.allocator, state) catch {};
        }

        const included = sub_pp.process() catch |err| {
            return err;
        };

        // Append included content to output
        self.output.appendSlice(self.allocator, included) catch return Error.OutOfMemory;
        self.output.append(self.allocator, '\n') catch return Error.OutOfMemory;

        // Copy any new defines back
        var sub_it = sub_pp.defines.iterator();
        while (sub_it.next()) |entry| {
            if (!self.defines.contains(entry.key_ptr.*)) {
                self.addDefine(entry.key_ptr.*, entry.value_ptr.*) catch {};
            }
        }
    }

    fn findAndReadInclude(self: *Preprocessor, filename: []const u8) ![]const u8 {
        // Try current directory first
        if (self.tryReadFile(filename)) |content| {
            return content;
        }

        // Try with .dbl extension
        var with_ext_buf: [512]u8 = undefined;
        const with_ext = std.fmt.bufPrint(&with_ext_buf, "{s}.dbl", .{filename}) catch filename;
        if (self.tryReadFile(with_ext)) |content| {
            return content;
        }

        // Try include paths
        for (self.include_paths.items) |base_path| {
            var path_buf: [1024]u8 = undefined;
            const full_path = std.fmt.bufPrint(&path_buf, "{s}/{s}", .{ base_path, filename }) catch continue;
            if (self.tryReadFile(full_path)) |content| {
                return content;
            }

            // With extension
            const full_path_ext = std.fmt.bufPrint(&path_buf, "{s}/{s}.dbl", .{ base_path, filename }) catch continue;
            if (self.tryReadFile(full_path_ext)) |content| {
                return content;
            }
        }

        return error.FileNotFound;
    }

    fn tryReadFile(self: *Preprocessor, path: []const u8) ?[]const u8 {
        const file = std.fs.cwd().openFile(path, .{}) catch return null;
        defer file.close();

        const content = file.readToEndAlloc(self.allocator, 1024 * 1024) catch return null;
        return content;
    }

    fn outputLine(self: *Preprocessor, line: []const u8) Error!void {
        // Apply text replacement for defined symbols (but not inside strings)
        var result: std.ArrayListUnmanaged(u8) = .empty;
        defer result.deinit(self.allocator);

        var i: usize = 0;
        while (i < line.len) {
            // Skip quoted strings - don't do replacement inside them
            if (line[i] == '"' or line[i] == '\'') {
                const quote = line[i];
                result.append(self.allocator, line[i]) catch return Error.OutOfMemory;
                i += 1;
                // Copy until closing quote
                while (i < line.len and line[i] != quote) {
                    result.append(self.allocator, line[i]) catch return Error.OutOfMemory;
                    i += 1;
                }
                if (i < line.len) {
                    result.append(self.allocator, line[i]) catch return Error.OutOfMemory;
                    i += 1;
                }
            }
            // Check if this is the start of an identifier
            else if (std.ascii.isAlphabetic(line[i]) or line[i] == '_') {
                const start = i;
                while (i < line.len and (std.ascii.isAlphanumeric(line[i]) or line[i] == '_')) {
                    i += 1;
                }
                const ident = line[start..i];

                // Check if it's a defined symbol (uppercase for lookup)
                var upper_buf: [64]u8 = undefined;
                const len = @min(ident.len, upper_buf.len);
                _ = std.ascii.upperString(upper_buf[0..len], ident[0..len]);

                if (self.defines.get(upper_buf[0..len])) |value| {
                    // Replace with value
                    result.appendSlice(self.allocator, value) catch return Error.OutOfMemory;
                } else {
                    // Keep original
                    result.appendSlice(self.allocator, ident) catch return Error.OutOfMemory;
                }
            } else {
                result.append(self.allocator, line[i]) catch return Error.OutOfMemory;
                i += 1;
            }
        }

        self.output.appendSlice(self.allocator, result.items) catch return Error.OutOfMemory;
        self.output.append(self.allocator, '\n') catch return Error.OutOfMemory;
    }
};

// Tests
test "basic define" {
    const allocator = std.testing.allocator;
    const source =
        \\.define DEBUG
        \\.ifdef DEBUG
        \\display("debug")
        \\.endc
    ;

    var pp = Preprocessor.init(allocator, source, "test.dbl");
    defer pp.deinit();

    const result = try pp.process();
    try std.testing.expect(std.mem.indexOf(u8, result, "display") != null);
}

test "define with value" {
    const allocator = std.testing.allocator;
    const source =
        \\.define TTCHN ,1
        \\open(TTCHN, o, "tt:")
    ;

    var pp = Preprocessor.init(allocator, source, "test.dbl");
    defer pp.deinit();

    const result = try pp.process();
    try std.testing.expect(std.mem.indexOf(u8, result, "open(1, o,") != null);
}

test "ifndef guard" {
    const allocator = std.testing.allocator;
    const source =
        \\.ifndef MYCONST
        \\.define MYCONST ,42
        \\.endc
        \\x = MYCONST
    ;

    var pp = Preprocessor.init(allocator, source, "test.dbl");
    defer pp.deinit();

    const result = try pp.process();
    try std.testing.expect(std.mem.indexOf(u8, result, "x = 42") != null);
}

test "ifdef false branch" {
    const allocator = std.testing.allocator;
    const source =
        \\.ifdef UNDEFINED_SYMBOL
        \\should_not_appear
        \\.else
        \\should_appear
        \\.endc
    ;

    var pp = Preprocessor.init(allocator, source, "test.dbl");
    defer pp.deinit();

    const result = try pp.process();
    try std.testing.expect(std.mem.indexOf(u8, result, "should_not_appear") == null);
    try std.testing.expect(std.mem.indexOf(u8, result, "should_appear") != null);
}

test "nested conditionals" {
    const allocator = std.testing.allocator;
    const source =
        \\.define OUTER
        \\.ifdef OUTER
        \\.define INNER
        \\.ifdef INNER
        \\nested_active
        \\.endc
        \\.endc
    ;

    var pp = Preprocessor.init(allocator, source, "test.dbl");
    defer pp.deinit();

    const result = try pp.process();
    try std.testing.expect(std.mem.indexOf(u8, result, "nested_active") != null);
}

test "unterminated conditional" {
    const allocator = std.testing.allocator;
    const source =
        \\.ifdef DEBUG
        \\missing endc
    ;

    var pp = Preprocessor.init(allocator, source, "test.dbl");
    defer pp.deinit();

    const result = pp.process();
    try std.testing.expectError(Preprocessor.Error.UnterminatedConditional, result);
}

test "builtin defines" {
    const allocator = std.testing.allocator;
    const source =
        \\.ifdef COT
        \\cot_defined
        \\.endc
    ;

    var pp = Preprocessor.init(allocator, source, "test.dbl");
    defer pp.deinit();
    try pp.addBuiltinDefines();

    const result = try pp.process();
    try std.testing.expect(std.mem.indexOf(u8, result, "cot_defined") != null);
}
