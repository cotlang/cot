//! Formatter module
//!
//! Shared code formatting for CLI (`cot fmt`) and LSP.

pub const formatter = @import("formatter.zig");
pub const Language = formatter.Language;
pub const TextEdit = formatter.TextEdit;
pub const FormatOptions = formatter.FormatOptions;
pub const format = formatter.format;
pub const formatToEdits = formatter.formatToEdits;
