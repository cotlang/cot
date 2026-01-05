# Claude Development Guidelines for Cot

## CRITICAL: Plan Tracking

**Before starting any work:**

1. Check `~/cotlang/claude/` for authoritative plan documents (task lists, roadmaps)
2. Read `.claude-plan` in the project root if it exists - it tracks current phase/task
3. When switching tasks, update `.claude-plan` with current status

**When the user asks "what's next":**
- Look in `~/cotlang/claude/` for task documents - NOT `~/.claude/plans/` (those are stale)
- Find the incomplete tasks and continue from there
- Ask if unclear which plan is active

**Never lose track of the plan.** Context loss causes repeated work and frustration.

## Debugging Tools

### cot trace - Execution Tracing

For runtime issues, start with execution tracing:

```bash
cot compile myfile.cot -o myfile.cbo
cot trace myfile.cbo --level=verbose
```

Trace levels: `none`, `routines`, `opcodes`, `verbose`, `full`

Verbose trace shows:
- Every opcode executed with IP and source line
- Register values with type information (e.g., `r0=int:42 r1=str:"hello"`)
- Call/return flow for routines and native functions

### cot debug - Interactive Debugger

For step-by-step debugging:

```bash
cot debug myfile.cbo
```

Commands:
- `r/run` - Run/continue execution
- `s/step` - Step one instruction
- `n/next` - Step over (to next line)
- `o/out` - Step out of current function
- `b <line>` - Set breakpoint at line
- `d <line>` - Delete breakpoint
- `i/info` - Show VM state (IP, line, SP, FP)
- `reg` - Show registers
- `stack` - Show stack
- `bt` - Show backtrace

### cot validate - Bytecode Validation

For verifying bytecode integrity:

```bash
cot validate myfile.cbo [--strict]
```

Reports: code size, instruction count, routine count, validation issues, reachability.

**If verbose trace does not immediately reveal the cause:**
1. Do NOT resort to trial-and-error debugging
2. Instead, **enhance the trace infrastructure** to expose the missing information
3. Add new trace output for the specific subsystem causing issues
4. The goal is to mature `cot trace` so it can pinpoint any issue quickly

This investment in tooling pays off across all future debugging sessions.

## Critical Rule: No Shortcuts, No Workarounds

Every issue must be analyzed to find the **root cause** and the **best fix** must always be implemented.

### What is NOT acceptable:
- TODOs that defer actual fixes
- Hacks or temporary workarounds
- Stubs that bypass functionality
- Changing source code differently to avoid fixing the real problem
- Ignoring warnings or errors
- Quick wins that mask underlying issues

### What IS required:
- Analyze every issue thoroughly
- **Read and understand existing comments before modifying code** - comments often explain ownership, invariants, or why code is written a certain way
- If a comment describes intended behavior that isn't working, the bug is likely that the code doesn't match the comment - fix the code, don't delete the comment
- Identify the root cause before implementing a fix
- Implement the proper, complete solution
- All GPA memory leak warnings must be fixed at the source
- All compiler warnings must be addressed
- Test that fixes actually resolve the underlying problem

### Memory Management Rules:
- Every allocation must have a corresponding deallocation
- Use arena allocators where appropriate for bulk allocations
- Track all heap allocations and ensure proper cleanup in deinit methods
- Never ignore memory leak warnings from GeneralPurposeAllocator

### Why This Matters:
Bugs that are bypassed rather than fixed accumulate as technical debt. They become harder to find later, cause cascading issues, and erode code quality. For this project to succeed, every bug must be addressed at its source.

## Codebase Architecture

### Directory Structure
```
src/
├── main.zig              # CLI entry point, command dispatch
├── root.zig              # Main library exports (cot module)
├── lexer/                # Tokenizer
├── parser/               # Parser → AST (NodeStore)
├── ast/                  # AST node types
├── ir/                   # Intermediate representation
│   ├── ir.zig            # IR types
│   └── lower.zig         # AST → IR lowering
├── compiler/             # Type checking, diagnostics
├── runtime/
│   ├── bytecode/
│   │   ├── vm.zig        # Virtual machine
│   │   ├── vm_opcodes.zig # Opcode handlers
│   │   ├── module.zig    # Bytecode module format
│   │   └── opcodes.zig   # Opcode definitions
│   ├── trace/            # Execution tracing (cot trace)
│   │   ├── trace.zig     # Tracer, TraceLevel, TraceEntry
│   │   ├── history.zig   # Ring buffer for history
│   │   └── output.zig    # Output formatting
│   ├── debug/            # Debugging infrastructure
│   │   ├── debug.zig     # Module root, presets
│   │   ├── breakpoint.zig # BreakpointManager (CLI/LSP shared)
│   │   ├── inspector.zig # StateInspector for VM state
│   │   └── validator.zig # Bytecode validation
│   ├── native/           # Native function bindings
│   └── cot_runtime.zig   # Runtime module exports
├── framework/            # Workspace/project commands
│   └── commands/         # init, new, build, run, etc.
└── dbl/                  # DBL language frontend
```

### Key Patterns

**Compilation Pipeline** (see `compileFile` in main.zig):
```
Source → Lexer → Parser → AST → IR Lower → Type Check → Optimize → Emit Bytecode → VM
```

**Adding VM opcodes**:
1. Add to `opcodes.zig` enum
2. Add handler in `vm_opcodes.zig`
3. Add to dispatch table in `vm.zig`
4. Add emitter in `ir/emit_bytecode.zig`

**Module imports** (for main.zig):
- `cot` = src/root.zig (compiler, lexer, parser, ir, bytecode)
- `cot_runtime` = src/runtime/cot_runtime.zig (VM, native, trace)
- `cot.ir_lower` = IR lowering
- `cot.ir_emit_bytecode` = bytecode emission

### Debugging

**Execution tracing**: `cot trace <file.cbo> [--level=opcodes|routines|verbose]`

**Environment variables**:
- `COT_LOG=debug` - Enable debug logging
- `COT_LOG=info` - Default log level

## Reference Documentation

### DBL Language Manual
The legacy DBL reference manual is located at `~/cotlang/dbllang/`.

## Zig 0.15 API Notes

This project uses Zig 0.15. Be aware of these API changes from older Zig versions:

### ArrayList API Changes
- **Don't use**: `std.ArrayList(T).init(allocator)` - this is the OLD API
- **Use instead**: `std.ArrayListUnmanaged(T)` with `.empty` initialization:
  ```zig
  var list: std.ArrayListUnmanaged(T) = .empty;
  errdefer list.deinit(allocator);

  try list.append(allocator, item);
  return list.toOwnedSlice(allocator);
  ```
- ArrayListUnmanaged methods require explicit allocator parameter: `append(allocator, item)`, `toOwnedSlice(allocator)`
- Use `errdefer list.deinit(allocator)` for proper cleanup on error paths
- `ArrayList.writer()` now requires allocator: `list.writer(allocator)`

### File I/O Changes
- **Don't use**: `std.io.getStdOut()` or `std.io.getStdErr()` - these don't exist
- **Use instead**: `std.fs.File.stdout()` and `std.fs.File.stderr()`
  ```zig
  const stderr_file: std.fs.File = .stderr();
  var buf: [4096]u8 = undefined;
  var writer = stderr_file.writer(&buf);
  writer.interface.print("Hello\n", .{}) catch {};
  ```
- `File.writer()` returns a buffered writer that requires a buffer
- Use `writer.interface.print()` to access print method

### Writer API
- `std.fs.File.Writer.print()` doesn't exist directly
- Access via `writer.interface.print()` where `interface` is `std.Io.Writer`
- `std.Io.Writer.print()` takes a mutable pointer `*Writer`, not const
