# Claude Development Guidelines for Cot

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
