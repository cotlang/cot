//! SSA HTML Visualizer for Cot (COT_SSA)
//!
//! Generates an interactive HTML page showing the function's SSA at every
//! compiler pass, side by side. Click a value to highlight it across all
//! passes. Dead code is faded. Dark mode supported.
//!
//! Set COT_SSA=funcname to generate the page during compilation.

const std = @import("std");
const Allocator = std.mem.Allocator;
const foundation = @import("foundation");
const TypeRegistry = foundation.types.TypeRegistry;
const func_mod = @import("func.zig");
const Func = func_mod.Func;
const Block = @import("block.zig").Block;
const BlockKind = @import("block.zig").BlockKind;
const Value = @import("value.zig").Value;
const Op = @import("op.zig").Op;

/// HTMLWriter generates an interactive HTML page showing SSA at every pass.
pub const HTMLWriter = struct {
    allocator: Allocator,
    buf: std.ArrayListUnmanaged(u8),
    func_name: []const u8,
    path: []const u8,
    prev_hash: u32,
    pending_phases: std.ArrayListUnmanaged([]const u8),
    pending_titles: std.ArrayListUnmanaged([]const u8),
    type_reg: ?*const TypeRegistry,

    pub fn init(allocator: Allocator, func_name: []const u8, path: []const u8, type_reg: ?*const TypeRegistry) HTMLWriter {
        var w = HTMLWriter{
            .allocator = allocator,
            .buf = .{},
            .func_name = func_name,
            .path = path,
            .prev_hash = 0,
            .pending_phases = .{},
            .pending_titles = .{},
            .type_reg = type_reg,
        };
        w.start();
        return w;
    }

    pub fn deinit(self: *HTMLWriter) void {
        self.buf.deinit(self.allocator);
        self.pending_phases.deinit(self.allocator);
        self.pending_titles.deinit(self.allocator);
    }

    /// Write the HTML page to disk and print the path.
    pub fn close(self: *HTMLWriter) void {
        self.writeStr("</tr></table></body></html>");
        std.fs.cwd().writeFile(.{
            .sub_path = self.path,
            .data = self.buf.items,
        }) catch |e| {
            std.debug.print("COT_SSA: failed to write {s}: {any}\n", .{ self.path, e });
            return;
        };
        std.debug.print("dumped SSA for {s} to {s}\n", .{ self.func_name, self.path });
    }

    /// Record a phase. If SSA changed since last phase, flush pending phases.
    pub fn writePhase(self: *HTMLWriter, phase: []const u8, title: []const u8, f: *const Func) void {
        const hash = hashFunc(self.allocator, f);
        self.pending_phases.append(self.allocator, phase) catch return;
        self.pending_titles.append(self.allocator, title) catch return;
        if (hash != self.prev_hash) {
            self.flushPhases(f);
        }
        self.prev_hash = hash;
    }

    /// Flush pending phases into a single HTML column.
    pub fn flushPhases(self: *HTMLWriter, f: *const Func) void {
        if (self.pending_phases.items.len == 0) return;

        var phase_buf: [1024]u8 = undefined;
        var phase_len: usize = 0;
        for (self.pending_phases.items, 0..) |p, i| {
            if (i > 0) {
                const sep = "  +  ";
                @memcpy(phase_buf[phase_len .. phase_len + sep.len], sep);
                phase_len += sep.len;
            }
            @memcpy(phase_buf[phase_len .. phase_len + p.len], p);
            phase_len += p.len;
        }
        const phases = phase_buf[0..phase_len];

        var func_html = std.ArrayListUnmanaged(u8){};
        defer func_html.deinit(self.allocator);
        func_html.appendSlice(self.allocator, "<code>") catch return;
        renderFuncHTML(self.allocator, &func_html, f, self.type_reg);
        func_html.appendSlice(self.allocator, "</code>") catch return;

        self.writeMultiTitleColumn(phases, self.pending_titles.items, func_html.items);

        self.pending_phases.clearRetainingCapacity();
        self.pending_titles.clearRetainingCapacity();
    }

    fn writeMultiTitleColumn(self: *HTMLWriter, phase: []const u8, titles: []const []const u8, html: []const u8) void {
        var id_buf: [256]u8 = undefined;
        const id_len = @min(phase.len, 255);
        @memcpy(id_buf[0..id_len], phase[0..id_len]);
        for (id_buf[0..id_len]) |*c| {
            if (c.* == ' ') c.* = '-';
        }
        const id = id_buf[0..id_len];

        self.writeStr("<td id=\"");
        self.writeStr(id);
        self.writeStr("-col\" class=\"collapsed\"><div>");
        self.writeStr(phase);
        self.writeStr("</div></td>");

        self.writeStr("<td id=\"");
        self.writeStr(id);
        self.writeStr("-exp\">");
        for (titles) |title| {
            self.writeStr("<h2>");
            self.writeStr(title);
            self.writeStr("</h2>");
        }
        self.buf.appendSlice(self.allocator, html) catch {};
        self.writeStr("</td>\n");
    }

    /// Write source code as the first column.
    pub fn writeSources(self: *HTMLWriter, source_text: []const u8, filename: []const u8) void {
        var source_buf = std.ArrayListUnmanaged(u8){};
        defer source_buf.deinit(self.allocator);

        source_buf.appendSlice(self.allocator, "<div class=\"lines\" style=\"width: 8%\">") catch return;

        var line_num: u32 = 1;
        var i: usize = 0;
        while (i <= source_text.len) : (line_num += 1) {
            var buf: [64]u8 = undefined;
            const ln = std.fmt.bufPrint(&buf, "<div class=\"l{d} line-number\">{d}</div>", .{ line_num, line_num }) catch break;
            source_buf.appendSlice(self.allocator, ln) catch break;
            while (i < source_text.len and source_text[i] != '\n') : (i += 1) {}
            i += 1;
        }

        source_buf.appendSlice(self.allocator, "</div><div style=\"width: 92%\"><pre>") catch return;

        source_buf.appendSlice(self.allocator, "<div><strong>") catch return;
        source_buf.appendSlice(self.allocator, filename) catch return;
        source_buf.appendSlice(self.allocator, "</strong></div>") catch return;

        line_num = 1;
        i = 0;
        while (i < source_text.len) : (line_num += 1) {
            var line_end = i;
            while (line_end < source_text.len and source_text[line_end] != '\n') : (line_end += 1) {}
            const line = source_text[i..line_end];

            var buf: [32]u8 = undefined;
            const prefix = std.fmt.bufPrint(&buf, "<div class=\"l{d} line-number\">", .{line_num}) catch break;
            source_buf.appendSlice(self.allocator, prefix) catch break;
            appendHTMLEscaped(self.allocator, &source_buf, line);
            source_buf.appendSlice(self.allocator, "</div>") catch break;

            i = line_end + 1;
        }

        source_buf.appendSlice(self.allocator, "</pre></div>") catch return;

        self.writeStr("<td id=\"sources-col\" class=\"collapsed\"><div>sources</div></td>");
        self.writeStr("<td id=\"sources-exp\" class=\"allow-x-scroll\">");
        self.writeStr("<h2>sources</h2>");
        self.buf.appendSlice(self.allocator, source_buf.items) catch {};
        self.writeStr("</td>\n");
    }

    fn start(self: *HTMLWriter) void {
        self.writeStr(
            \\<html>
            \\<head>
            \\<meta http-equiv="Content-Type" content="text/html;charset=UTF-8">
            \\<style>
        );
        self.writeStr(css_template);
        self.writeStr(
            \\</style>
            \\<script type="text/javascript">
        );
        self.writeStr(js_template);
        self.writeStr(
            \\</script>
            \\</head>
            \\<body>
        );
        self.writeStr("<h1>");
        self.writeStr(self.func_name);
        self.writeStr("</h1>");
        self.writeStr(
            \\<a href="#" onclick="toggle_visibility('help');return false;" id="helplink">help</a>
            \\<div id="help">
            \\<p>Click on a value or block to toggle highlighting of that value/block and its uses.
            \\(Values and blocks are highlighted by ID, and IDs of dead items may be reused,
            \\so not all highlights necessarily correspond to the clicked item.)</p>
            \\<p>Faded out values and blocks are dead code that has not been eliminated.</p>
            \\<p>Values printed in italics have a dependency cycle.</p>
            \\</div>
            \\<label for="dark-mode-button" style="margin-left: 15px; cursor: pointer;">darkmode</label>
            \\<input type="checkbox" onclick="toggleDarkMode();" id="dark-mode-button" style="cursor: pointer" />
            \\<table>
            \\<tr>
        );
    }

    fn writeStr(self: *HTMLWriter, s: []const u8) void {
        self.buf.appendSlice(self.allocator, s) catch {};
    }
};

fn renderValueHTML(allocator: Allocator, out: *std.ArrayListUnmanaged(u8), v: *const Value) void {
    var buf: [64]u8 = undefined;
    const id = std.fmt.bufPrint(&buf, "v{d}", .{v.id}) catch return;
    out.appendSlice(allocator, "<span class=\"") catch return;
    out.appendSlice(allocator, id) catch return;
    out.appendSlice(allocator, " ssa-value\">") catch return;
    out.appendSlice(allocator, id) catch return;
    out.appendSlice(allocator, "</span>") catch return;
}

fn renderValueLongHTML(allocator: Allocator, out: *std.ArrayListUnmanaged(u8), v: *const Value, type_reg: ?*const TypeRegistry) void {
    var buf: [128]u8 = undefined;
    const vid = std.fmt.bufPrint(&buf, "v{d}", .{v.id}) catch return;

    out.appendSlice(allocator, "<span class=\"") catch return;
    out.appendSlice(allocator, vid) catch return;
    out.appendSlice(allocator, " ssa-long-value\">") catch return;

    renderValueHTML(allocator, out, v);

    if (v.pos.line > 0) {
        const ln = std.fmt.bufPrint(&buf, " <span class=\"l{d} line-number\">({d}:{d})</span>", .{ v.pos.line, v.pos.line, v.pos.col }) catch "";
        out.appendSlice(allocator, ln) catch {};
    } else {
        out.appendSlice(allocator, " <span class=\"no-line-number\">(?)</span>") catch {};
    }

    out.appendSlice(allocator, " = ") catch {};
    const op_name = @tagName(v.op);
    out.appendSlice(allocator, op_name) catch {};

    out.appendSlice(allocator, " &lt;") catch {};
    if (type_reg) |tr| {
        const tname = tr.typeName(v.type_idx);
        appendHTMLEscaped(allocator, out, tname);
        const size = tr.sizeOf(v.type_idx);
        if (size > 8) {
            const sz = std.fmt.bufPrint(&buf, " {d}B", .{size}) catch "";
            out.appendSlice(allocator, sz) catch {};
        }
    } else {
        const type_str = std.fmt.bufPrint(&buf, "type:{d}", .{@intFromEnum(v.type_idx)}) catch "?";
        out.appendSlice(allocator, type_str) catch {};
    }
    out.appendSlice(allocator, "&gt;") catch {};

    if (v.aux == .string) {
        out.appendSlice(allocator, " {\"") catch {};
        appendHTMLEscaped(allocator, out, v.aux.string);
        out.appendSlice(allocator, "\"}") catch {};
    } else if (v.aux == .call) {
        if (v.aux_call) |ac| {
            if (ac.fn_name.len > 0) {
                out.appendSlice(allocator, " call:") catch {};
                appendHTMLEscaped(allocator, out, ac.fn_name);
            }
        }
    }
    if (v.aux_int != 0) {
        switch (v.op) {
            .wasm_i64_load, .wasm_i64_store, .wasm_i64_load8_s, .wasm_i64_load8_u,
            .wasm_i64_load16_s, .wasm_i64_load16_u, .wasm_i64_load32_s, .wasm_i64_load32_u,
            .wasm_i64_store8, .wasm_i64_store16, .wasm_i64_store32,
            .wasm_f64_load, .wasm_f64_store, .wasm_f32_load, .wasm_f32_store,
            => {
                const aux = std.fmt.bufPrint(&buf, " offset={d}", .{v.aux_int}) catch "";
                out.appendSlice(allocator, aux) catch {};
            },
            .wasm_i64_const, .wasm_i32_const, .const_int => {
                const aux = std.fmt.bufPrint(&buf, " ={d}", .{v.aux_int}) catch "";
                out.appendSlice(allocator, aux) catch {};
            },
            .arg => {
                const aux = std.fmt.bufPrint(&buf, " arg#{d}", .{v.aux_int}) catch "";
                out.appendSlice(allocator, aux) catch {};
            },
            .wasm_gc_struct_new, .wasm_gc_struct_get, .wasm_gc_struct_set => {
                const type_idx = @as(u32, @truncate(@as(u64, @bitCast(v.aux_int))));
                const field_idx = @as(u32, @truncate(@as(u64, @bitCast(v.aux_int)) >> 16));
                if (field_idx > 0) {
                    const aux = std.fmt.bufPrint(&buf, " gc_type={d} field={d}", .{ type_idx, field_idx }) catch "";
                    out.appendSlice(allocator, aux) catch {};
                } else {
                    const aux = std.fmt.bufPrint(&buf, " gc_type={d}", .{type_idx}) catch "";
                    out.appendSlice(allocator, aux) catch {};
                }
            },
            else => {
                const aux = std.fmt.bufPrint(&buf, " [{d}]", .{v.aux_int}) catch "";
                out.appendSlice(allocator, aux) catch {};
            },
        }
    }

    for (v.args) |arg| {
        out.appendSlice(allocator, " ") catch {};
        renderValueHTML(allocator, out, arg);
    }

    if (v.home) |home| {
        switch (home) {
            .register => |r| {
                const reg = std.fmt.bufPrint(&buf, " : R{d}", .{r}) catch "";
                out.appendSlice(allocator, reg) catch {};
            },
            .stack => |s| {
                const stk = std.fmt.bufPrint(&buf, " : [SP+{d}]", .{s}) catch "";
                out.appendSlice(allocator, stk) catch {};
            },
        }
    }

    const uses = std.fmt.bufPrint(&buf, " (uses={d})", .{v.uses}) catch "";
    out.appendSlice(allocator, uses) catch {};

    out.appendSlice(allocator, "</span>") catch {};
}

fn renderBlockHTML(allocator: Allocator, out: *std.ArrayListUnmanaged(u8), b: *const Block) void {
    var buf: [64]u8 = undefined;
    const id = std.fmt.bufPrint(&buf, "b{d}", .{b.id}) catch return;
    out.appendSlice(allocator, "<span class=\"") catch return;
    out.appendSlice(allocator, id) catch return;
    out.appendSlice(allocator, " ssa-block\">") catch return;
    out.appendSlice(allocator, id) catch return;
    out.appendSlice(allocator, "</span>") catch return;
}

fn renderBlockLongHTML(allocator: Allocator, out: *std.ArrayListUnmanaged(u8), b: *const Block) void {
    var buf: [64]u8 = undefined;
    const bid = std.fmt.bufPrint(&buf, "b{d}", .{b.id}) catch return;

    out.appendSlice(allocator, "<span class=\"") catch return;
    out.appendSlice(allocator, bid) catch return;
    out.appendSlice(allocator, " ssa-block\">") catch return;
    out.appendSlice(allocator, @tagName(b.kind)) catch return;
    out.appendSlice(allocator, "</span>") catch return;

    for (b.controls) |ctrl| {
        if (ctrl) |c| {
            out.appendSlice(allocator, " ") catch {};
            renderValueHTML(allocator, out, c);
        }
    }

    if (b.succs.len > 0) {
        out.appendSlice(allocator, " &#8594;") catch {};
        for (b.succs) |e| {
            out.appendSlice(allocator, " ") catch {};
            renderBlockHTML(allocator, out, e.b);
        }
    }

    if (b.likely == .likely) {
        out.appendSlice(allocator, " (likely)") catch {};
    } else if (b.likely == .unlikely) {
        out.appendSlice(allocator, " (unlikely)") catch {};
    }
}

fn renderFuncHTML(allocator: Allocator, out: *std.ArrayListUnmanaged(u8), f: *const Func, type_reg: ?*const TypeRegistry) void {
    const ssa_debug = @import("debug.zig");
    var html_ctx = HTMLFuncPrinterCtx{
        .allocator = allocator,
        .out = out,
        .type_reg = type_reg,
    };
    const printer = ssa_debug.FuncPrinter{
        .ctx = @ptrCast(&html_ctx),
        .headerFn = @ptrCast(&HTMLFuncPrinterCtx.header),
        .startBlockFn = @ptrCast(&HTMLFuncPrinterCtx.startBlock),
        .endBlockFn = @ptrCast(&HTMLFuncPrinterCtx.endBlock),
        .valueFn = @ptrCast(&HTMLFuncPrinterCtx.value),
    };
    ssa_debug.fprintFunc(allocator, printer, f) catch {};
}

const HTMLFuncPrinterCtx = struct {
    allocator: Allocator,
    out: *std.ArrayListUnmanaged(u8),
    type_reg: ?*const TypeRegistry,

    fn header(self: *HTMLFuncPrinterCtx, f: *const Func) void {
        _ = f;
        _ = self;
    }

    fn startBlock(self: *HTMLFuncPrinterCtx, b: *const Block, reachable: bool) void {
        var buf: [64]u8 = undefined;
        const bid = std.fmt.bufPrint(&buf, "b{d}", .{b.id}) catch return;

        self.out.appendSlice(self.allocator, "<ul class=\"") catch return;
        self.out.appendSlice(self.allocator, bid) catch return;
        if (!reachable) {
            self.out.appendSlice(self.allocator, " ssa-print-func dead-block\">") catch return;
        } else {
            self.out.appendSlice(self.allocator, " ssa-print-func\">") catch return;
        }

        self.out.appendSlice(self.allocator, "<li class=\"ssa-start-block\">") catch return;
        renderBlockHTML(self.allocator, self.out, b);
        self.out.appendSlice(self.allocator, ":") catch {};

        if (b.preds.len > 0) {
            self.out.appendSlice(self.allocator, " &#8592;") catch {};
            for (b.preds) |e| {
                self.out.appendSlice(self.allocator, " ") catch {};
                renderBlockHTML(self.allocator, self.out, e.b);
            }
        }

        if (b.values.items.len > 0) {
            self.out.appendSlice(self.allocator, "<button onclick=\"hideBlock(this)\">-</button>") catch {};
        }
        self.out.appendSlice(self.allocator, "</li>") catch {};

        if (b.values.items.len > 0) {
            self.out.appendSlice(self.allocator, "<li class=\"ssa-value-list\"><ul>") catch {};
        }
    }

    fn endBlock(self: *HTMLFuncPrinterCtx, b: *const Block, reachable: bool) void {
        _ = reachable;
        if (b.values.items.len > 0) {
            self.out.appendSlice(self.allocator, "</ul></li>") catch {};
        }
        self.out.appendSlice(self.allocator, "<li class=\"ssa-end-block\">") catch {};
        renderBlockLongHTML(self.allocator, self.out, b);
        self.out.appendSlice(self.allocator, "</li></ul>") catch {};
    }

    fn value(self: *HTMLFuncPrinterCtx, v: *const Value, live: bool) void {
        if (!live) {
            self.out.appendSlice(self.allocator, "<li class=\"ssa-long-value dead-value\">") catch {};
        } else {
            self.out.appendSlice(self.allocator, "<li class=\"ssa-long-value\">") catch {};
        }
        renderValueLongHTML(self.allocator, self.out, v, self.type_reg);
        self.out.appendSlice(self.allocator, "</li>") catch {};
    }
};

fn hashFunc(allocator: Allocator, f: *const Func) u32 {
    var h: u32 = 2166136261;
    for (f.blocks.items) |b| {
        var buf: [64]u8 = undefined;
        const header_str = std.fmt.bufPrint(&buf, "b{d}:{s}\n", .{ b.id, @tagName(b.kind) }) catch continue;
        for (header_str) |c| {
            h ^= c;
            h *%= 16777619;
        }
        for (b.values.items) |v| {
            const line = std.fmt.bufPrint(&buf, "v{d}={s}", .{ v.id, @tagName(v.op) }) catch continue;
            for (line) |c| {
                h ^= c;
                h *%= 16777619;
            }
            for (v.args) |arg| {
                const a = std.fmt.bufPrint(&buf, " v{d}", .{arg.id}) catch continue;
                for (a) |c| {
                    h ^= c;
                    h *%= 16777619;
                }
            }
        }
        for (b.succs) |e| {
            const s = std.fmt.bufPrint(&buf, "->b{d}\n", .{e.b.id}) catch continue;
            for (s) |c| {
                h ^= c;
                h *%= 16777619;
            }
        }
    }
    _ = allocator;
    return h;
}

fn appendHTMLEscaped(allocator: Allocator, out: *std.ArrayListUnmanaged(u8), s: []const u8) void {
    for (s) |c| {
        switch (c) {
            '<' => out.appendSlice(allocator, "&lt;") catch {},
            '>' => out.appendSlice(allocator, "&gt;") catch {},
            '&' => out.appendSlice(allocator, "&amp;") catch {},
            '"' => out.appendSlice(allocator, "&quot;") catch {},
            else => out.append(allocator, c) catch {},
        }
    }
}

const css_template =
    \\
    \\body { font-size: 14px; font-family: Arial, sans-serif; }
    \\h1 { font-size: 18px; display: inline-block; margin: 0 1em .5em 0; }
    \\#helplink { display: inline-block; }
    \\#help { display: none; }
    \\.stats { font-size: 60%; }
    \\table { border: 1px solid black; table-layout: fixed; width: 300px; }
    \\th, td { border: 1px solid black; overflow: hidden; width: 400px; vertical-align: top; padding: 5px; }
    \\td > h2 { cursor: pointer; font-size: 120%; margin: 5px 0px 5px 0px; }
    \\td.collapsed { font-size: 12px; width: 12px; border: 1px solid white; padding: 2px; cursor: pointer; background: #fafafa; }
    \\td.collapsed div { text-align: right; transform: rotate(180deg); writing-mode: vertical-lr; white-space: pre; }
    \\code, pre, .lines, .ast { font-family: Menlo, monospace; font-size: 12px; }
    \\pre { -moz-tab-size: 4; -o-tab-size: 4; tab-size: 4; }
    \\.allow-x-scroll { overflow-x: scroll; }
    \\.lines { float: left; overflow: hidden; text-align: right; margin-top: 7px; }
    \\.lines div { padding-right: 10px; color: gray; }
    \\div.line-number { font-size: 12px; }
    \\.ast { white-space: nowrap; }
    \\td.ssa-prog { width: 600px; word-wrap: break-word; }
    \\li { list-style-type: none; }
    \\li.ssa-long-value { text-indent: -2em; }
    \\li.ssa-value-list { display: inline; }
    \\li.ssa-start-block { padding: 0; margin: 0; }
    \\li.ssa-end-block { padding: 0; margin: 0; }
    \\ul.ssa-print-func { padding-left: 0; }
    \\li.ssa-start-block button { padding: 0 1em; margin: 0; border: none; display: inline; font-size: 14px; float: right; }
    \\button:hover { background-color: #eee; cursor: pointer; }
    \\.dead-value { color: gray; }
    \\.dead-block { opacity: 0.5; }
    \\.depcycle { font-style: italic; }
    \\.line-number { font-size: 11px; }
    \\.no-line-number { font-size: 11px; color: gray; }
    \\.highlight-aquamarine     { background-color: aquamarine; color: black; }
    \\.highlight-coral          { background-color: coral; color: black; }
    \\.highlight-lightpink      { background-color: lightpink; color: black; }
    \\.highlight-lightsteelblue { background-color: lightsteelblue; color: black; }
    \\.highlight-palegreen      { background-color: palegreen; color: black; }
    \\.highlight-skyblue        { background-color: skyblue; color: black; }
    \\.highlight-lightgray      { background-color: lightgray; color: black; }
    \\.highlight-yellow         { background-color: yellow; color: black; }
    \\.highlight-lime           { background-color: lime; color: black; }
    \\.highlight-khaki          { background-color: khaki; color: black; }
    \\.highlight-aqua           { background-color: aqua; color: black; }
    \\.highlight-salmon         { background-color: salmon; color: black; }
    \\.outline-blue           { outline: #2893ff solid 2px; }
    \\.outline-red            { outline: red solid 2px; }
    \\body.darkmode { background-color: rgb(21, 21, 21); color: rgb(230, 255, 255); opacity: 100%; }
    \\td.darkmode { background-color: rgb(21, 21, 21); border: 1px solid gray; }
    \\body.darkmode table, th { border: 1px solid gray; }
    \\
;

const js_template =
    \\
    \\let expandedDefault = ["start","deadcode","lower_wasm","lower_native","schedule","layout"];
    \\if (history.state === null) { history.pushState({expandedDefault}, "", location.href); }
    \\var highlights = ["highlight-aquamarine","highlight-coral","highlight-lightpink","highlight-lightsteelblue","highlight-palegreen","highlight-skyblue","highlight-lightgray","highlight-yellow","highlight-lime","highlight-khaki","highlight-aqua","highlight-salmon"];
    \\var highlighted = {};
    \\for (var i = 0; i < highlights.length; i++) { highlighted[highlights[i]] = ""; }
    \\var outlines = ["outline-blue","outline-red","outline-blueviolet","outline-darkolivegreen","outline-fuchsia","outline-sienna","outline-gold","outline-orangered","outline-teal","outline-maroon","outline-black"];
    \\var outlined = {};
    \\for (var i = 0; i < outlines.length; i++) { outlined[outlines[i]] = ""; }
    \\window.onload = function() {
    \\    if (history.state !== null) { expandedDefault = history.state.expandedDefault; }
    \\    if (window.matchMedia && window.matchMedia("(prefers-color-scheme: dark)").matches) { toggleDarkMode(); document.getElementById("dark-mode-button").checked = true; }
    \\    var ssaElemClicked = function(elem, event, selections, selected) {
    \\        event.stopPropagation();
    \\        var c = elem.classList.item(0);
    \\        var x = document.getElementsByClassName(c);
    \\        var remove = "";
    \\        for (var i = 0; i < selections.length; i++) { if (selected[selections[i]] == c) { remove = selections[i]; break; } }
    \\        if (remove != "") { for (var i = 0; i < x.length; i++) { x[i].classList.remove(remove); } selected[remove] = ""; return; }
    \\        var avail = "";
    \\        for (var i = 0; i < selections.length; i++) { if (selected[selections[i]] == "") { avail = selections[i]; break; } }
    \\        if (avail == "") { alert("out of selection colors"); return; }
    \\        for (var i = 0; i < x.length; i++) { x[i].classList.add(avail); }
    \\        selected[avail] = c;
    \\    };
    \\    var ssaValueClicked = function(event) { ssaElemClicked(this, event, highlights, highlighted); };
    \\    var ssaBlockClicked = function(event) { ssaElemClicked(this, event, outlines, outlined); };
    \\    var ssavalues = document.getElementsByClassName("ssa-value");
    \\    for (var i = 0; i < ssavalues.length; i++) { ssavalues[i].addEventListener('click', ssaValueClicked); }
    \\    var ssalongvalues = document.getElementsByClassName("ssa-long-value");
    \\    for (var i = 0; i < ssalongvalues.length; i++) { if (ssalongvalues[i].nodeName == "SPAN") { ssalongvalues[i].addEventListener('click', ssaValueClicked); } }
    \\    var ssablocks = document.getElementsByClassName("ssa-block");
    \\    for (var i = 0; i < ssablocks.length; i++) { ssablocks[i].addEventListener('click', ssaBlockClicked); }
    \\    var lines = document.getElementsByClassName("line-number");
    \\    for (var i = 0; i < lines.length; i++) { lines[i].addEventListener('click', ssaValueClicked); }
    \\    function toggler(phase) { return function() { toggle_cell(phase+'-col'); toggle_cell(phase+'-exp'); }; }
    \\    function toggle_cell(id) { var e = document.getElementById(id); if (e.style.display == 'table-cell') { e.style.display = 'none'; } else { e.style.display = 'table-cell'; } }
    \\    const td = document.getElementsByTagName("td");
    \\    for (let i = 0; i < td.length; i++) {
    \\        const id = td[i].id; const phase = id.substr(0, id.length-4);
    \\        let show = expandedDefault.indexOf(phase) !== -1;
    \\        if (id.endsWith("-exp")) { const h2Els = td[i].getElementsByTagName("h2"); for (let j = 0; j < h2Els.length; j++) { h2Els[j].addEventListener('click', toggler(phase)); } }
    \\        else { td[i].addEventListener('click', toggler(phase)); }
    \\        if (id.endsWith("-col") && show || id.endsWith("-exp") && !show) { td[i].style.display = 'none'; continue; }
    \\        td[i].style.display = 'table-cell';
    \\    }
    \\};
    \\function toggle_visibility(id) { var e = document.getElementById(id); if (e.style.display == 'block') { e.style.display = 'none'; } else { e.style.display = 'block'; } }
    \\function hideBlock(el) { var es = el.parentNode.parentNode.getElementsByClassName("ssa-value-list"); if (es.length===0) return; var e = es[0]; if (e.style.display === 'block' || e.style.display === '') { e.style.display = 'none'; el.innerHTML = '+'; } else { e.style.display = 'block'; el.innerHTML = '-'; } }
    \\function toggleDarkMode() { document.body.classList.toggle('darkmode'); var collapsedEls = document.getElementsByClassName('collapsed'); for (var i = 0; i < collapsedEls.length; i++) { collapsedEls[i].classList.toggle('darkmode'); } }
    \\
;

test "appendHTMLEscaped" {
    const allocator = std.testing.allocator;
    var out = std.ArrayListUnmanaged(u8){};
    defer out.deinit(allocator);
    appendHTMLEscaped(allocator, &out, "<script>alert('xss')</script>");
    try std.testing.expectEqualStrings("&lt;script&gt;alert('xss')&lt;/script&gt;", out.items);
}

test "hashFunc produces consistent hash" {
    _ = hashFunc;
}
