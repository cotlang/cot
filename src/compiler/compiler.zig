//! Cot Compiler Module
//!
//! This module provides compile-time analysis and diagnostics for the Cot language.
//! It includes:
//! - Diagnostic collection and formatting
//! - Type checking and validation

pub const diagnostics = @import("diagnostics.zig");
pub const DiagnosticCollector = @import("diagnostic_collector.zig");
pub const formatter = @import("diagnostic_formatter.zig");
pub const type_rules = @import("type_rules.zig");
pub const TypeChecker = @import("type_checker.zig");

// Re-export commonly used types
pub const Diagnostic = diagnostics.Diagnostic;
pub const SourceRange = diagnostics.SourceRange;
pub const Code = diagnostics.Code;
pub const Level = diagnostics.Level;
pub const Suggestion = diagnostics.Suggestion;

// Format options for output
pub const FormatOptions = formatter.FormatOptions;

/// Create a new diagnostic collector
pub fn createCollector(allocator: @import("std").mem.Allocator) DiagnosticCollector {
    return DiagnosticCollector.init(allocator);
}
