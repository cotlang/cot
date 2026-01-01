//! Schema File Parser
//!
//! Parses schema.cot files that define database table structures.
//! Schema files use a decorator-based syntax:
//!
//! ```
//! ; Database schema definition
//! @database customers
//! @version 1
//!
//! @table customer
//!     id              ,ulid       @primary @auto
//!     name            ,text(30)   @not_null
//!     email           ,text(50)   @unique @index
//!     balance         ,decimal    @default(0)
//!     created_at      ,datetime   @audit_created
//!     updated_at      ,datetime   @audit_updated
//! @endtable
//! ```
//!
//! Also supports legacy DBL-style syntax for compatibility:
//! - d6 -> integer (6-digit decimal)
//! - a30 -> text(30)
//! - i4 -> integer (medium)
//! - d10.2 -> decimal(10, 2)

const std = @import("std");
const types = @import("types.zig");

const SchemaFile = types.SchemaFile;
const TableSchema = types.TableSchema;
const FieldSchema = types.FieldSchema;
const Decorators = types.Decorators;
const ForeignKey = types.ForeignKey;
const OnDeleteAction = types.OnDeleteAction;
const KeyDefinition = types.KeyDefinition;
const SchemaError = types.SchemaError;
const SchemaDataType = types.SchemaDataType;

/// Schema file parser
pub const SchemaParser = struct {
    source: []const u8,
    position: usize,
    line: usize,
    column: usize,
    allocator: std.mem.Allocator,

    const Self = @This();

    /// Initialize parser with source code and allocator
    pub fn init(allocator: std.mem.Allocator, source: []const u8) Self {
        return .{
            .source = source,
            .position = 0,
            .line = 1,
            .column = 1,
            .allocator = allocator,
        };
    }

    /// Parse the entire schema file
    pub fn parse(self: *Self) SchemaError!SchemaFile {
        var database_name: ?[]const u8 = null;
        var version: ?u32 = null;
        var tables: std.ArrayListAligned(TableSchema, null) = .empty;
        var keys: std.ArrayListAligned(KeyDefinition, null) = .empty;
        errdefer {
            for (tables.items) |*table| {
                table.deinit(self.allocator);
            }
            tables.deinit(self.allocator);
            for (keys.items) |*key| {
                key.deinit(self.allocator);
            }
            keys.deinit(self.allocator);
        }

        while (!self.isAtEnd()) {
            self.skipWhitespaceAndComments();
            if (self.isAtEnd()) break;

            if (self.peek() == '@') {
                _ = self.advance(); // consume '@'
                const directive = self.parseIdentifier() orelse return SchemaError.UnexpectedToken;

                if (std.mem.eql(u8, directive, "database")) {
                    self.skipWhitespace();
                    database_name = self.parseIdentifier() orelse return SchemaError.MissingField;
                } else if (std.mem.eql(u8, directive, "version")) {
                    self.skipWhitespace();
                    version = self.parseNumber() orelse return SchemaError.InvalidVersion;
                } else if (std.mem.eql(u8, directive, "table")) {
                    const table = try self.parseTable();
                    tables.append(self.allocator, table) catch return SchemaError.OutOfMemory;
                } else if (std.mem.eql(u8, directive, "key")) {
                    const key = try self.parseKeyDefinition();
                    keys.append(self.allocator, key) catch return SchemaError.OutOfMemory;
                } else {
                    return SchemaError.UnexpectedToken;
                }
            } else if (std.ascii.isAlphabetic(self.peek()) or self.peek() == '_') {
                // Skip unexpected identifiers at top level
                _ = self.parseIdentifier();
            } else {
                _ = self.advance();
            }
        }

        if (database_name == null) return SchemaError.MissingDatabase;
        if (version == null) return SchemaError.MissingVersion;

        return SchemaFile{
            .database_name = database_name.?,
            .version = version.?,
            .tables = tables.toOwnedSlice(self.allocator) catch return SchemaError.OutOfMemory,
            .keys = keys.toOwnedSlice(self.allocator) catch return SchemaError.OutOfMemory,
            .allocator = self.allocator,
        };
    }

    /// Parse a table definition
    fn parseTable(self: *Self) SchemaError!TableSchema {
        self.skipWhitespace();
        const name = self.parseIdentifier() orelse return SchemaError.MissingField;

        var fields: std.ArrayListAligned(FieldSchema, null) = .empty;
        errdefer {
            for (fields.items) |*field| {
                field.deinit(self.allocator);
            }
            fields.deinit(self.allocator);
        }

        // Parse fields until @endtable
        while (!self.isAtEnd()) {
            self.skipWhitespaceAndComments();
            if (self.isAtEnd()) break;

            if (self.peek() == '@') {
                _ = self.advance();
                const directive = self.parseIdentifier() orelse return SchemaError.UnexpectedToken;

                if (std.mem.eql(u8, directive, "endtable")) {
                    break;
                } else {
                    // This is a decorator on the previous field, but we handle decorators inline
                    // Reset position to re-parse
                    return SchemaError.UnexpectedToken;
                }
            }

            // Parse field definition
            if (std.ascii.isAlphabetic(self.peek()) or self.peek() == '_') {
                const field = try self.parseField();
                fields.append(self.allocator, field) catch return SchemaError.OutOfMemory;
            }
        }

        return TableSchema{
            .name = name,
            .fields = fields.toOwnedSlice(self.allocator) catch return SchemaError.OutOfMemory,
        };
    }

    /// Parse a field definition
    fn parseField(self: *Self) SchemaError!FieldSchema {
        const name = self.parseIdentifier() orelse return SchemaError.MissingField;

        self.skipWhitespace();

        // Expect comma before type
        if (self.peek() == ',') {
            _ = self.advance();
        }

        self.skipWhitespace();

        // Parse data type
        const data_type = self.parseDataType() orelse return SchemaError.InvalidDataType;

        // Parse decorators
        const decorators = try self.parseDecorators();

        return FieldSchema{
            .name = name,
            .data_type = data_type,
            .decorators = decorators,
        };
    }

    /// Parse data type specification
    /// Modern syntax: text, text(30), integer, decimal, decimal(10,2), bool, blob, datetime, ulid
    /// Legacy syntax: a30, d6, d10.2, i4, p8 (for backward compatibility)
    fn parseDataType(self: *Self) ?SchemaDataType {
        const start = self.position;
        const type_name = self.parseIdentifier() orelse return null;

        // Modern type names
        if (std.mem.eql(u8, type_name, "text") or std.mem.eql(u8, type_name, "string") or std.mem.eql(u8, type_name, "varchar")) {
            // Check for size: text(30)
            self.skipWhitespace();
            if (self.peek() == '(') {
                _ = self.advance();
                self.skipWhitespace();
                const size: ?usize = if (self.parseNumber()) |n| @as(usize, n) else null;
                self.skipWhitespace();
                if (self.peek() == ')') _ = self.advance();
                return .{ .text = .{ .max_length = size } };
            }
            return .{ .text = .{ .max_length = null } };
        }

        if (std.mem.eql(u8, type_name, "integer") or std.mem.eql(u8, type_name, "int")) {
            return .{ .integer = .medium };
        }

        if (std.mem.eql(u8, type_name, "bigint") or std.mem.eql(u8, type_name, "int64")) {
            return .{ .integer = .big };
        }

        if (std.mem.eql(u8, type_name, "smallint") or std.mem.eql(u8, type_name, "int16")) {
            return .{ .integer = .small };
        }

        if (std.mem.eql(u8, type_name, "tinyint") or std.mem.eql(u8, type_name, "int8")) {
            return .{ .integer = .tiny };
        }

        if (std.mem.eql(u8, type_name, "decimal") or std.mem.eql(u8, type_name, "numeric") or std.mem.eql(u8, type_name, "money")) {
            // Check for precision: decimal(10,2)
            self.skipWhitespace();
            if (self.peek() == '(') {
                _ = self.advance();
                self.skipWhitespace();
                const precision: u8 = if (self.parseNumber()) |n| @truncate(n) else 10;
                self.skipWhitespace();
                var scale: u8 = 2;
                if (self.peek() == ',') {
                    _ = self.advance();
                    self.skipWhitespace();
                    scale = if (self.parseNumber()) |n| @truncate(n) else 2;
                    self.skipWhitespace();
                }
                if (self.peek() == ')') _ = self.advance();
                return .{ .decimal = .{ .precision = precision, .scale = scale } };
            }
            return .{ .decimal = .{ .precision = 10, .scale = 2 } };
        }

        if (std.mem.eql(u8, type_name, "bool") or std.mem.eql(u8, type_name, "boolean")) {
            return .{ .boolean = {} };
        }

        if (std.mem.eql(u8, type_name, "blob") or std.mem.eql(u8, type_name, "binary")) {
            self.skipWhitespace();
            if (self.peek() == '(') {
                _ = self.advance();
                self.skipWhitespace();
                const size: ?usize = if (self.parseNumber()) |n| @as(usize, n) else null;
                self.skipWhitespace();
                if (self.peek() == ')') _ = self.advance();
                return .{ .blob = .{ .max_size = size } };
            }
            return .{ .blob = .{ .max_size = null } };
        }

        if (std.mem.eql(u8, type_name, "date")) {
            return .{ .datetime = .date };
        }

        if (std.mem.eql(u8, type_name, "time")) {
            return .{ .datetime = .time };
        }

        if (std.mem.eql(u8, type_name, "datetime") or std.mem.eql(u8, type_name, "timestamp")) {
            return .{ .datetime = .datetime };
        }

        if (std.mem.eql(u8, type_name, "ulid")) {
            return .{ .ulid = {} };
        }

        // Legacy DBL-style single-letter type codes
        if (type_name.len == 1) {
            const type_char = std.ascii.toLower(type_name[0]);

            // Reset position to just after the single char and parse size
            self.position = start + 1;

            switch (type_char) {
                'a' => {
                    const size: ?usize = if (self.parseNumber()) |n| @as(usize, n) else null;
                    return .{ .text = .{ .max_length = size } };
                },
                'd' => {
                    const total = self.parseNumber() orelse 1;

                    // Check for implied decimal (d10.2)
                    if (self.peek() == '.') {
                        _ = self.advance();
                        const precision = self.parseNumber() orelse 0;
                        return .{ .decimal = .{
                            .precision = @truncate(total),
                            .scale = @truncate(precision),
                        } };
                    }

                    // Plain decimal becomes integer (for compatibility)
                    return .{ .integer = .medium };
                },
                'i' => {
                    const size = self.parseNumber() orelse 4;
                    return .{ .integer = switch (size) {
                        1 => .tiny,
                        2 => .small,
                        4 => .medium,
                        8 => .big,
                        else => .medium,
                    } };
                },
                'p' => {
                    // Packed decimal -> blob
                    const size: ?usize = if (self.parseNumber()) |n| @as(usize, n) else null;
                    return .{ .blob = .{ .max_size = size } };
                },
                else => {},
            }
        }

        return null;
    }

    /// Parse field decorators (@primary, @not_null, @foreign(table.column), etc.)
    fn parseDecorators(self: *Self) SchemaError!Decorators {
        var decorators = Decorators{};

        while (!self.isAtEnd()) {
            self.skipWhitespace();

            // Check for newline (end of field definition)
            if (self.peek() == '\n' or self.peek() == '\r') {
                break;
            }

            // Check for comment
            if (self.peek() == ';') {
                self.skipToEndOfLine();
                break;
            }

            // Check for decorator
            if (self.peek() != '@') {
                break;
            }

            _ = self.advance(); // consume '@'

            const decorator = self.parseIdentifier() orelse return SchemaError.InvalidDecorator;

            if (std.mem.eql(u8, decorator, "primary")) {
                decorators.primary = true;
            } else if (std.mem.eql(u8, decorator, "auto")) {
                decorators.auto = true;
            } else if (std.mem.eql(u8, decorator, "not_null")) {
                decorators.not_null = true;
            } else if (std.mem.eql(u8, decorator, "unique")) {
                decorators.unique = true;
            } else if (std.mem.eql(u8, decorator, "index")) {
                decorators.index = true;
            } else if (std.mem.eql(u8, decorator, "audit_created")) {
                decorators.audit_created = true;
            } else if (std.mem.eql(u8, decorator, "audit_updated")) {
                decorators.audit_updated = true;
            } else if (std.mem.eql(u8, decorator, "default")) {
                // Parse default value: @default(value) or @default("string")
                const value = try self.parseDecoratorValue();
                decorators.default_value = value;
            } else if (std.mem.eql(u8, decorator, "foreign")) {
                // Parse foreign key: @foreign(table.column)
                const fk = try self.parseForeignKey();
                decorators.foreign_key = fk;
            } else if (std.mem.eql(u8, decorator, "on_delete")) {
                // Parse on_delete action: @on_delete(cascade)
                const action = try self.parseOnDeleteAction();
                decorators.on_delete = action;
            } else {
                return SchemaError.InvalidDecorator;
            }
        }

        return decorators;
    }

    /// Parse decorator value: (value) or ("string")
    fn parseDecoratorValue(self: *Self) SchemaError!?[]const u8 {
        self.skipWhitespace();
        if (self.peek() != '(') {
            return null;
        }
        _ = self.advance(); // consume '('

        self.skipWhitespace();

        var value: []const u8 = undefined;

        if (self.peek() == '"' or self.peek() == '\'') {
            const quote = self.advance();
            const start = self.position;
            while (!self.isAtEnd() and self.peek() != quote) {
                _ = self.advance();
            }
            value = self.source[start..self.position];
            if (!self.isAtEnd()) _ = self.advance(); // consume closing quote
        } else {
            // Parse unquoted value (number or identifier)
            const start = self.position;
            while (!self.isAtEnd() and self.peek() != ')' and !std.ascii.isWhitespace(self.peek())) {
                _ = self.advance();
            }
            value = self.source[start..self.position];
        }

        self.skipWhitespace();
        if (self.peek() != ')') {
            return SchemaError.InvalidDecorator;
        }
        _ = self.advance(); // consume ')'

        return value;
    }

    /// Parse foreign key reference: (table.column)
    fn parseForeignKey(self: *Self) SchemaError!ForeignKey {
        self.skipWhitespace();
        if (self.peek() != '(') {
            return SchemaError.InvalidForeignKey;
        }
        _ = self.advance();

        self.skipWhitespace();
        const table = self.parseIdentifier() orelse return SchemaError.InvalidForeignKey;

        if (self.peek() != '.') {
            return SchemaError.InvalidForeignKey;
        }
        _ = self.advance();

        const column = self.parseIdentifier() orelse return SchemaError.InvalidForeignKey;

        self.skipWhitespace();
        if (self.peek() != ')') {
            return SchemaError.InvalidForeignKey;
        }
        _ = self.advance();

        return ForeignKey{
            .table = table,
            .column = column,
        };
    }

    /// Parse on_delete action: (cascade|set_null|restrict)
    fn parseOnDeleteAction(self: *Self) SchemaError!OnDeleteAction {
        self.skipWhitespace();
        if (self.peek() != '(') {
            return SchemaError.InvalidDecorator;
        }
        _ = self.advance();

        self.skipWhitespace();
        const action = self.parseIdentifier() orelse return SchemaError.InvalidDecorator;

        self.skipWhitespace();
        if (self.peek() != ')') {
            return SchemaError.InvalidDecorator;
        }
        _ = self.advance();

        if (std.mem.eql(u8, action, "cascade")) {
            return .cascade;
        } else if (std.mem.eql(u8, action, "set_null")) {
            return .set_null;
        } else if (std.mem.eql(u8, action, "restrict")) {
            return .restrict;
        } else {
            return SchemaError.InvalidDecorator;
        }
    }

    /// Parse composite key definition: @key table.keyname (field1, field2, ...) @primary @unique
    fn parseKeyDefinition(self: *Self) SchemaError!KeyDefinition {
        self.skipWhitespace();

        // Parse table.keyname
        const table_name = self.parseIdentifier() orelse return SchemaError.MissingField;

        if (self.peek() != '.') {
            return SchemaError.InvalidDecorator;
        }
        _ = self.advance(); // consume '.'

        const key_name = self.parseIdentifier() orelse return SchemaError.MissingField;

        self.skipWhitespace();

        // Parse (field1, field2, ...)
        if (self.peek() != '(') {
            return SchemaError.InvalidDecorator;
        }
        _ = self.advance(); // consume '('

        var segments: std.ArrayListAligned([]const u8, null) = .empty;
        errdefer segments.deinit(self.allocator);

        while (!self.isAtEnd() and self.peek() != ')') {
            self.skipWhitespace();

            const field_name = self.parseIdentifier() orelse break;
            segments.append(self.allocator, field_name) catch return SchemaError.OutOfMemory;

            self.skipWhitespace();
            if (self.peek() == ',') {
                _ = self.advance();
            }
        }

        if (self.peek() != ')') {
            return SchemaError.InvalidDecorator;
        }
        _ = self.advance(); // consume ')'

        // Parse optional decorators (@primary, @unique)
        var is_primary = false;
        var is_unique = false;
        var key_number: ?u8 = null;

        while (!self.isAtEnd()) {
            self.skipWhitespace();
            if (self.peek() != '@') break;
            if (self.peek() == '\n' or self.peek() == '\r') break;

            _ = self.advance(); // consume '@'
            const decorator = self.parseIdentifier() orelse break;

            if (std.mem.eql(u8, decorator, "primary")) {
                is_primary = true;
            } else if (std.mem.eql(u8, decorator, "unique")) {
                is_unique = true;
            } else if (std.mem.eql(u8, decorator, "key_number")) {
                // Parse key number: @key_number(N)
                self.skipWhitespace();
                if (self.peek() == '(') {
                    _ = self.advance();
                    self.skipWhitespace();
                    if (self.parseNumber()) |n| {
                        key_number = @truncate(n);
                    }
                    self.skipWhitespace();
                    if (self.peek() == ')') {
                        _ = self.advance();
                    }
                }
            }
        }

        return KeyDefinition{
            .name = key_name,
            .table_name = table_name,
            .segments = segments.toOwnedSlice(self.allocator) catch return SchemaError.OutOfMemory,
            .primary = is_primary,
            .unique = is_unique,
            .key_number = key_number,
        };
    }

    // ========================================================================
    // Lexer helpers
    // ========================================================================

    fn parseIdentifier(self: *Self) ?[]const u8 {
        const start = self.position;

        if (self.isAtEnd()) return null;
        if (!std.ascii.isAlphabetic(self.peek()) and self.peek() != '_') return null;

        while (!self.isAtEnd() and (std.ascii.isAlphanumeric(self.peek()) or self.peek() == '_')) {
            _ = self.advance();
        }

        if (self.position == start) return null;
        return self.source[start..self.position];
    }

    fn parseNumber(self: *Self) ?u32 {
        const start = self.position;

        while (!self.isAtEnd() and std.ascii.isDigit(self.peek())) {
            _ = self.advance();
        }

        if (self.position == start) return null;

        return std.fmt.parseInt(u32, self.source[start..self.position], 10) catch null;
    }

    fn skipWhitespace(self: *Self) void {
        while (!self.isAtEnd()) {
            const c = self.peek();
            if (c == ' ' or c == '\t') {
                _ = self.advance();
            } else {
                break;
            }
        }
    }

    fn skipWhitespaceAndComments(self: *Self) void {
        while (!self.isAtEnd()) {
            const c = self.peek();
            switch (c) {
                ' ', '\t', '\r' => {
                    _ = self.advance();
                },
                '\n' => {
                    _ = self.advance();
                    self.line += 1;
                    self.column = 1;
                },
                ';' => {
                    self.skipToEndOfLine();
                },
                else => break,
            }
        }
    }

    fn skipToEndOfLine(self: *Self) void {
        while (!self.isAtEnd() and self.peek() != '\n') {
            _ = self.advance();
        }
    }

    fn isAtEnd(self: *const Self) bool {
        return self.position >= self.source.len;
    }

    fn peek(self: *const Self) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.position];
    }

    fn advance(self: *Self) u8 {
        const c = self.source[self.position];
        self.position += 1;
        self.column += 1;
        return c;
    }
};

/// Parse a schema file from source string
pub fn parseSchema(allocator: std.mem.Allocator, source: []const u8) SchemaError!SchemaFile {
    var parser = SchemaParser.init(allocator, source);
    return parser.parse();
}

/// Parse a schema file from a file path
pub fn parseSchemaFile(allocator: std.mem.Allocator, path: []const u8) !SchemaFile {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const source = try file.readToEndAlloc(allocator, 1024 * 1024); // 1MB max
    errdefer allocator.free(source);

    var schema = try parseSchema(allocator, source);
    // Transfer ownership of source buffer to the SchemaFile
    schema.source = source;
    return schema;
}

// ============================================================================
// Tests
// ============================================================================

test "parse simple schema with modern syntax" {
    const source =
        \\; Test schema with modern types
        \\@database testdb
        \\@version 1
        \\
        \\@table customer
        \\    id          ,ulid       @primary @auto
        \\    name        ,text(30)   @not_null
        \\    email       ,text(50)   @unique
        \\@endtable
    ;

    var schema = try parseSchema(std.testing.allocator, source);
    defer schema.deinit();

    try std.testing.expectEqualStrings("testdb", schema.database_name);
    try std.testing.expectEqual(@as(u32, 1), schema.version);
    try std.testing.expectEqual(@as(usize, 1), schema.tables.len);

    const table = &schema.tables[0];
    try std.testing.expectEqualStrings("customer", table.name);
    try std.testing.expectEqual(@as(usize, 3), table.fields.len);

    // Check id field (ulid)
    try std.testing.expectEqualStrings("id", table.fields[0].name);
    try std.testing.expect(table.fields[0].data_type == .ulid);
    try std.testing.expect(table.fields[0].decorators.primary);
    try std.testing.expect(table.fields[0].decorators.auto);

    // Check name field (text(30))
    try std.testing.expectEqualStrings("name", table.fields[1].name);
    try std.testing.expect(table.fields[1].data_type == .text);
    try std.testing.expectEqual(@as(?usize, 30), table.fields[1].data_type.text.max_length);
    try std.testing.expect(table.fields[1].decorators.not_null);

    // Check email field
    try std.testing.expectEqualStrings("email", table.fields[2].name);
    try std.testing.expect(table.fields[2].decorators.unique);
}

test "parse schema with legacy DBL syntax" {
    const source =
        \\; Test schema with legacy syntax
        \\@database testdb
        \\@version 1
        \\
        \\@table customer
        \\    id          ,d6     @primary @auto
        \\    name        ,a30    @not_null
        \\    email       ,a50    @unique
        \\@endtable
    ;

    var schema = try parseSchema(std.testing.allocator, source);
    defer schema.deinit();

    try std.testing.expectEqualStrings("testdb", schema.database_name);
    try std.testing.expectEqual(@as(u32, 1), schema.version);
    try std.testing.expectEqual(@as(usize, 1), schema.tables.len);

    const table = &schema.tables[0];
    try std.testing.expectEqual(@as(usize, 3), table.fields.len);

    // Check id field (d6 -> integer)
    try std.testing.expectEqualStrings("id", table.fields[0].name);
    try std.testing.expect(table.fields[0].data_type == .integer);
    try std.testing.expect(table.fields[0].decorators.primary);
    try std.testing.expect(table.fields[0].decorators.auto);

    // Check name field (a30 -> text(30))
    try std.testing.expectEqualStrings("name", table.fields[1].name);
    try std.testing.expect(table.fields[1].data_type == .text);
    try std.testing.expectEqual(@as(?usize, 30), table.fields[1].data_type.text.max_length);
}

test "parse schema with foreign key" {
    const source =
        \\@database orders
        \\@version 1
        \\
        \\@table order_line
        \\    id          ,ulid       @primary @auto
        \\    order_id    ,ulid       @foreign(order.id) @on_delete(cascade)
        \\    product     ,text(30)   @not_null
        \\@endtable
    ;

    var schema = try parseSchema(std.testing.allocator, source);
    defer schema.deinit();

    const table = &schema.tables[0];
    const order_id_field = &table.fields[1];

    try std.testing.expect(order_id_field.decorators.foreign_key != null);
    try std.testing.expectEqualStrings("order", order_id_field.decorators.foreign_key.?.table);
    try std.testing.expectEqualStrings("id", order_id_field.decorators.foreign_key.?.column);
    try std.testing.expectEqual(OnDeleteAction.cascade, order_id_field.decorators.on_delete.?);
}

test "parse schema with default values" {
    const source =
        \\@database inventory
        \\@version 2
        \\
        \\@table product
        \\    id          ,ulid           @primary @auto
        \\    name        ,text(50)       @not_null
        \\    price       ,decimal(10,2)  @default(0)
        \\    status      ,text(1)        @default("A")
        \\@endtable
    ;

    var schema = try parseSchema(std.testing.allocator, source);
    defer schema.deinit();

    const table = &schema.tables[0];

    // Check price field default
    try std.testing.expectEqualStrings("0", table.fields[2].decorators.default_value.?);

    // Check status field default
    try std.testing.expectEqualStrings("A", table.fields[3].decorators.default_value.?);
}

test "parse schema with decimal type" {
    const source =
        \\@database finance
        \\@version 1
        \\
        \\@table transaction
        \\    id          ,ulid           @primary
        \\    amount      ,decimal(10,2)
        \\@endtable
    ;

    var schema = try parseSchema(std.testing.allocator, source);
    defer schema.deinit();

    const amount_field = &schema.tables[0].fields[1];

    switch (amount_field.data_type) {
        .decimal => |d| {
            try std.testing.expectEqual(@as(u8, 10), d.precision);
            try std.testing.expectEqual(@as(u8, 2), d.scale);
        },
        else => try std.testing.expect(false),
    }
}

test "parse schema with legacy implied decimal" {
    const source =
        \\@database finance
        \\@version 1
        \\
        \\@table transaction
        \\    id          ,d10    @primary
        \\    amount      ,d10.2
        \\@endtable
    ;

    var schema = try parseSchema(std.testing.allocator, source);
    defer schema.deinit();

    const amount_field = &schema.tables[0].fields[1];

    switch (amount_field.data_type) {
        .decimal => |d| {
            try std.testing.expectEqual(@as(u8, 10), d.precision);
            try std.testing.expectEqual(@as(u8, 2), d.scale);
        },
        else => try std.testing.expect(false),
    }
}

test "parse schema with audit fields" {
    const source =
        \\@database audit_test
        \\@version 1
        \\
        \\@table record
        \\    id          ,ulid       @primary
        \\    created_at  ,datetime   @audit_created
        \\    updated_at  ,datetime   @audit_updated
        \\@endtable
    ;

    var schema = try parseSchema(std.testing.allocator, source);
    defer schema.deinit();

    const table = &schema.tables[0];

    try std.testing.expect(table.fields[1].decorators.audit_created);
    try std.testing.expect(table.fields[2].decorators.audit_updated);
    try std.testing.expect(table.fields[1].data_type == .datetime);
}

test "missing database error" {
    const source =
        \\@version 1
        \\@table test
        \\    id ,ulid
        \\@endtable
    ;

    const result = parseSchema(std.testing.allocator, source);
    try std.testing.expectError(SchemaError.MissingDatabase, result);
}

test "missing version error" {
    const source =
        \\@database test
        \\@table test
        \\    id ,ulid
        \\@endtable
    ;

    const result = parseSchema(std.testing.allocator, source);
    try std.testing.expectError(SchemaError.MissingVersion, result);
}

test "parse composite keys" {
    const source =
        \\@database orders
        \\@version 1
        \\
        \\@table order
        \\    customer_id ,ulid
        \\    order_date  ,datetime
        \\    line_no     ,integer
        \\    product     ,text(30)
        \\@endtable
        \\
        \\@key order.pk (customer_id, order_date, line_no) @primary
        \\@key order.by_customer (customer_id, order_date) @unique
    ;

    var schema = try parseSchema(std.testing.allocator, source);
    defer schema.deinit();

    try std.testing.expectEqual(@as(usize, 1), schema.tables.len);
    try std.testing.expectEqual(@as(usize, 2), schema.keys.len);

    // Check first key (primary)
    const pk = &schema.keys[0];
    try std.testing.expectEqualStrings("order", pk.table_name);
    try std.testing.expectEqualStrings("pk", pk.name);
    try std.testing.expectEqual(@as(usize, 3), pk.segments.len);
    try std.testing.expectEqualStrings("customer_id", pk.segments[0]);
    try std.testing.expectEqualStrings("order_date", pk.segments[1]);
    try std.testing.expectEqualStrings("line_no", pk.segments[2]);
    try std.testing.expect(pk.primary);
    try std.testing.expect(!pk.unique);

    // Check second key (unique)
    const by_cust = &schema.keys[1];
    try std.testing.expectEqualStrings("order", by_cust.table_name);
    try std.testing.expectEqualStrings("by_customer", by_cust.name);
    try std.testing.expectEqual(@as(usize, 2), by_cust.segments.len);
    try std.testing.expect(!by_cust.primary);
    try std.testing.expect(by_cust.unique);
}
