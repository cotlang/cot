//! DBL Lexer Extension
//!
//! Provides DBL keyword recognition for the Cot lexer.
//! In DBL mode, these keywords are recognized as distinct tokens.
//! In Core mode, they are treated as regular identifiers.

const std = @import("std");

/// DBL-specific keywords that become tokens in DBL mode
/// In Core mode, these are valid identifiers
pub const DblKeyword = enum {
    // Program structure
    proc,
    endproc,
    subroutine,
    endsubroutine,
    function,
    endfunction,
    main,
    endmain,
    record,
    endrecord,
    structure,
    endstructure,
    group,
    endgroup,
    common,
    endcommon,
    global,
    endglobal,
    literal,
    endliteral,
    begin,
    end,
    external,
    stack,

    // Control flow
    then,
    using,
    endusing,
    of,
    endcase,
    select,
    exitloop,
    nextloop,
    repeat,

    // Loops - DBL style
    do_forever, // DO FOREVER
    do_until, // DO...UNTIL
    thru,

    // Subroutine calls
    xcall,
    call,
    xreturn,
    freturn,
    mreturn,
    stop,
    exit,

    // I/O operations
    open,
    close,
    read,
    write,
    store,
    delete,
    find,
    reads,
    writes,
    gets,
    puts,
    accept,
    display,

    // Error handling
    onerror,
    offerror,
    throw,
    trap,

    // Data operations
    clear,
    init,
    incr,
    decr,
    locase,
    upcase,
    sleep,
    forms,
    lpque,
    send,
    recv,

    // OOP (DBL style)
    class,
    endclass,
    method,
    endmethod,
    property,
    endproperty,
    namespace,
    endnamespace,
    parent,
    extends,
    implements,

    /// Look up a keyword by name
    pub fn fromString(s: []const u8) ?DblKeyword {
        return keyword_map.get(s);
    }
};

/// Map of DBL keyword strings to enum values
const keyword_map = std.StaticStringMap(DblKeyword).initComptime(.{
    // Program structure
    .{ "proc", .proc },
    .{ "endproc", .endproc },
    .{ "subroutine", .subroutine },
    .{ "endsubroutine", .endsubroutine },
    .{ "function", .function },
    .{ "endfunction", .endfunction },
    .{ "main", .main },
    .{ "endmain", .endmain },
    .{ "record", .record },
    .{ "endrecord", .endrecord },
    .{ "structure", .structure },
    .{ "endstructure", .endstructure },
    .{ "group", .group },
    .{ "endgroup", .endgroup },
    .{ "common", .common },
    .{ "endcommon", .endcommon },
    .{ "global", .global },
    .{ "endglobal", .endglobal },
    .{ "literal", .literal },
    .{ "endliteral", .endliteral },
    .{ "begin", .begin },
    .{ "end", .end },
    .{ "external", .external },
    .{ "stack", .stack },

    // Control flow
    .{ "then", .then },
    .{ "using", .using },
    .{ "endusing", .endusing },
    .{ "of", .of },
    .{ "endcase", .endcase },
    .{ "select", .select },
    .{ "exitloop", .exitloop },
    .{ "nextloop", .nextloop },
    .{ "repeat", .repeat },
    .{ "thru", .thru },

    // Subroutine calls
    .{ "xcall", .xcall },
    .{ "call", .call },
    .{ "xreturn", .xreturn },
    .{ "freturn", .freturn },
    .{ "mreturn", .mreturn },
    .{ "stop", .stop },
    .{ "exit", .exit },

    // I/O
    .{ "open", .open },
    .{ "close", .close },
    .{ "read", .read },
    .{ "write", .write },
    .{ "store", .store },
    .{ "delete", .delete },
    .{ "find", .find },
    .{ "reads", .reads },
    .{ "writes", .writes },
    .{ "gets", .gets },
    .{ "puts", .puts },
    .{ "accept", .accept },
    .{ "display", .display },

    // Error handling
    .{ "onerror", .onerror },
    .{ "offerror", .offerror },
    .{ "throw", .throw },
    .{ "trap", .trap },

    // Data operations
    .{ "clear", .clear },
    .{ "init", .init },
    .{ "incr", .incr },
    .{ "decr", .decr },
    .{ "locase", .locase },
    .{ "upcase", .upcase },
    .{ "sleep", .sleep },
    .{ "forms", .forms },
    .{ "lpque", .lpque },
    .{ "send", .send },
    .{ "recv", .recv },

    // OOP
    .{ "class", .class },
    .{ "endclass", .endclass },
    .{ "method", .method },
    .{ "endmethod", .endmethod },
    .{ "property", .property },
    .{ "endproperty", .endproperty },
    .{ "namespace", .namespace },
    .{ "endnamespace", .endnamespace },
    .{ "parent", .parent },
    .{ "extends", .extends },
    .{ "implements", .implements },
});

/// DBL operators (both dot-form and modern symbol form)
pub const DblOperator = enum {
    // Comparison operators
    eq, // .EQ. or ==
    ne, // .NE. or != or <>
    lt, // .LT. or <
    le, // .LE. or <=
    gt, // .GT. or >
    ge, // .GE. or >=

    // Logical operators
    and_, // .AND. or &&
    or_, // .OR. or ||
    not_, // .NOT. or !
    xor_, // .XOR.

    // Bitwise operators
    band, // .BAND. or &
    bor, // .BOR. or |
    bnot, // .BNOT. or ~
    bxor, // .BXOR. or ^
    bnand, // .BNAND.
    shl, // << (bit shift left)
    shr, // >> (bit shift right)

    // String comparison operators (full-length)
    eqs, // .EQS. (string equal)
    nes, // .NES. (string not equal)
    gts, // .GTS. (string greater than)
    lts, // .LTS. (string less than)
    ges, // .GES. (string greater or equal)
    les, // .LES. (string less or equal)

    // Modulo
    mod, // .MOD. or %

    // Rounding (DBL legacy)
    round, // # (truncating round)
    round_true, // ## (true round)

    /// Parse a DBL operator from a string (e.g., ".EQ.")
    pub fn fromString(s: []const u8) ?DblOperator {
        if (s.len < 4 or s[0] != '.' or s[s.len - 1] != '.') {
            return null;
        }
        const inner = s[1 .. s.len - 1];
        return operator_map.get(inner);
    }
};

const operator_map = std.StaticStringMap(DblOperator).initComptime(.{
    // Comparison
    .{ "EQ", .eq },
    .{ "NE", .ne },
    .{ "LT", .lt },
    .{ "LE", .le },
    .{ "GT", .gt },
    .{ "GE", .ge },
    // Logical
    .{ "AND", .and_ },
    .{ "OR", .or_ },
    .{ "NOT", .not_ },
    .{ "XOR", .xor_ },
    // Bitwise
    .{ "BAND", .band },
    .{ "BOR", .bor },
    .{ "BNOT", .bnot },
    .{ "BXOR", .bxor },
    .{ "BNAND", .bnand },
    // String comparison
    .{ "EQS", .eqs },
    .{ "NES", .nes },
    .{ "GTS", .gts },
    .{ "LTS", .lts },
    .{ "GES", .ges },
    .{ "LES", .les },
    // Modulo
    .{ "MOD", .mod },
});

/// Check if an identifier is a DBL keyword
pub fn isDblKeyword(s: []const u8) bool {
    return DblKeyword.fromString(s) != null;
}

test "dbl keyword lookup" {
    try std.testing.expect(DblKeyword.fromString("proc") == .proc);
    try std.testing.expect(DblKeyword.fromString("xcall") == .xcall);
    try std.testing.expect(DblKeyword.fromString("unknown") == null);
}

test "dbl operator parsing" {
    try std.testing.expect(DblOperator.fromString(".EQ.") == .eq);
    try std.testing.expect(DblOperator.fromString(".AND.") == .and_);
    try std.testing.expect(DblOperator.fromString("invalid") == null);
}
