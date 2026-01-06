//! Dex: Real-time Server-Rendered UI Framework
//!
//! Dex is Cot's answer to Phoenix LiveView - a server-rendered, real-time UI
//! framework that eliminates the frontend/backend divide.
//!
//! Named after Dex, a loyal 14-year-old husky/cattle dog mix.

const std = @import("std");

// Template engine
pub const template = struct {
    // Lexer
    pub const Lexer = @import("template/lexer.zig").Lexer;
    pub const Token = @import("template/lexer.zig").Token;
    pub const TokenType = @import("template/lexer.zig").TokenType;

    // Parser
    pub const Parser = @import("template/parser.zig").Parser;
    pub const Node = @import("template/parser.zig").Node;
    pub const NodeType = @import("template/parser.zig").NodeType;
    pub const NodeData = @import("template/parser.zig").NodeData;
    pub const Attribute = @import("template/parser.zig").Attribute;
    pub const AttributeValue = @import("template/parser.zig").AttributeValue;
    pub const EventBinding = @import("template/parser.zig").EventBinding;

    // Renderer
    pub const Renderer = @import("template/renderer.zig").Renderer;
    pub const Context = @import("template/renderer.zig").Context;
    pub const Value = @import("template/renderer.zig").Value;
};

// Component runtime
pub const component = struct {
    pub const ComponentDef = @import("component.zig").ComponentDef;
    pub const ComponentInstance = @import("component.zig").ComponentInstance;
    pub const StateField = @import("component.zig").StateField;
    pub const PropDef = @import("component.zig").PropDef;
    pub const Event = @import("component.zig").Event;
    pub const Registry = @import("component.zig").Registry;
};

// DOM diffing
pub const diff = struct {
    pub const Differ = @import("diff.zig").Differ;
    pub const Patch = @import("diff.zig").Patch;
    pub const PatchOp = @import("diff.zig").PatchOp;
    pub const PatchSet = @import("diff.zig").PatchSet;
};

// Socket management
pub const socket = struct {
    pub const Manager = @import("socket.zig").Manager;
    pub const Session = @import("socket.zig").Session;
    pub const ClientMessage = @import("socket.zig").ClientMessage;
    pub const ClientMessageType = @import("socket.zig").ClientMessageType;
};

// PubSub for real-time features
pub const pubsub = struct {
    pub const Broker = @import("pubsub.zig").Broker;
    pub const Message = @import("pubsub.zig").Message;
    pub const Subscription = @import("pubsub.zig").Subscription;
};

// Component file parser (.dex files)
pub const component_parser = struct {
    pub const ComponentLexer = @import("component_parser.zig").ComponentLexer;
    pub const ComponentParser = @import("component_parser.zig").ComponentParser;
    pub const ParsedComponent = @import("component_parser.zig").ParsedComponent;
    pub const toComponentDef = @import("component_parser.zig").toComponentDef;
};

// Live route handler
pub const live = struct {
    pub const LiveContext = @import("live_handler.zig").LiveContext;
    pub const LiveRoute = @import("live_handler.zig").LiveRoute;
    pub const PendingPatch = @import("live_handler.zig").PendingPatch;
};

// Component compiler (Dex AST → executable components)
pub const compiler = struct {
    pub const Compiler = @import("compiler.zig").Compiler;
    pub const CompiledComponent = @import("compiler.zig").CompiledComponent;
    pub const ComponentInstance = @import("compiler.zig").ComponentInstance;
    pub const CompiledHandler = @import("compiler.zig").CompiledHandler;
    pub const CompiledStateField = @import("compiler.zig").CompiledStateField;
    pub const TypeTag = @import("compiler.zig").TypeTag;
};

test {
    _ = @import("template/lexer.zig");
    _ = @import("template/parser.zig");
    _ = @import("template/renderer.zig");
    _ = @import("component.zig");
    _ = @import("diff.zig");
    _ = @import("socket.zig");
    _ = @import("pubsub.zig");
    _ = @import("component_parser.zig");
    _ = @import("live_handler.zig");
    _ = @import("compiler.zig");
}
