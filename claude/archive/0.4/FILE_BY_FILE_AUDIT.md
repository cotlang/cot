# File-by-File Audit: src/ vs compiler/

**Status: PREVIOUS AUDIT WAS UNRELIABLE. Must be redone from scratch.**

The previous agent marked 55+ files as PASS by grep'ing function names, not reading code.
Many "PASS" files have logic divergences that cause runtime failures.

## Known divergences (discovered, not exhaustive)

- lower.zig: inferExprType doesn't match reference, lowerStructInit was wrong, 44 debug.log calls missing
- checker.zig: expr_types may not be populated for all expression paths in test bodies
- parser.zig: multi-arg .call node encoding was wrong (fixed, but may have other issues)
- driver.zig: func_indices offset bugs (partially fixed)
- ssa_builder.zig: function names differ from reference, some edge cases unverified

## Test results (78 pass, 4 fail, 6 compile errors out of 22 no-import test files)

The audit must be restarted with the correct method:
1. Run same input through BOTH compilers with COT_DEBUG=all
2. Diff the output line by line
3. Fix first divergence
4. Repeat until output is identical
5. THEN mark as PASS
