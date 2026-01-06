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

// Page routing and SSR
pub const page = struct {
    pub const PageRouter = @import("page.zig").PageRouter;
    pub const PageMeta = @import("page.zig").PageMeta;
    pub const RequestContext = @import("page.zig").RequestContext;
    pub const ServerProps = @import("page.zig").ServerProps;
};

// HTML document shell
pub const document = struct {
    pub const DocumentConfig = @import("document.zig").DocumentConfig;
    pub const render = @import("document.zig").render;
    pub const renderError = @import("document.zig").renderError;
    pub const render404 = @import("document.zig").render404;
};

// File-based router
pub const router = struct {
    pub const Router = @import("router.zig").Router;
    pub const Route = @import("router.zig").Route;
    pub const Segment = @import("router.zig").Segment;
    pub const SegmentType = @import("router.zig").SegmentType;
    pub const MatchResult = @import("router.zig").MatchResult;
    pub const Redirect = @import("router.zig").Redirect;
};

// Layout system
pub const layout = struct {
    pub const Layout = @import("layout.zig").Layout;
    pub const LayoutChain = @import("layout.zig").LayoutChain;
    pub const LayoutResolver = @import("layout.zig").LayoutResolver;
};

// Built-in components
pub const components = struct {
    pub const link = @import("components/link.zig");
    pub const Link = link.LinkProps;
    pub const renderLink = link.render;
    pub const NavLink = link.NavLink;
};

// Response helpers
pub const response = struct {
    pub const Response = @import("response.zig").Response;
    pub const ContentType = @import("response.zig").ContentType;
    pub const redirect = @import("response.zig").redirect;
    pub const redirectTo = @import("response.zig").redirectTo;
    pub const permanentRedirect = @import("response.zig").permanentRedirect;
    pub const notFound = @import("response.zig").notFound;
    pub const json = @import("response.zig").json;
    pub const jsonValue = @import("response.zig").jsonValue;
    pub const text = @import("response.zig").text;
    pub const html = @import("response.zig").html;
    pub const error_ = @import("response.zig").error_;
    pub const forbidden = @import("response.zig").forbidden;
    pub const unauthorized = @import("response.zig").unauthorized;
    pub const badRequest = @import("response.zig").badRequest;
};

// Content system (Markdown/MDX)
pub const content = struct {
    // Markdown parser
    pub const markdown = @import("content/markdown.zig");
    pub const MarkdownParser = markdown.Parser;
    pub const MarkdownOptions = markdown.Options;
    pub const TocEntry = markdown.TocEntry;
    pub const CodeBlock = markdown.CodeBlock;

    // Frontmatter parser
    pub const frontmatter = @import("content/frontmatter.zig");
    pub const Frontmatter = frontmatter.Frontmatter;
    pub const parseFrontmatter = frontmatter.parse;
    pub const generateMetaTags = frontmatter.generateMetaTags;

    // Syntax highlighting
    pub const highlight = @import("content/highlight.zig");
    pub const Highlighter = highlight.Highlighter;
    pub const TokenType = highlight.TokenType;
    pub const Language = highlight.Language;
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
    _ = @import("page.zig");
    _ = @import("document.zig");
    _ = @import("response.zig");
    _ = @import("router.zig");
    _ = @import("layout.zig");
    _ = @import("components/link.zig");
    // Content system
    _ = @import("content/markdown.zig");
    _ = @import("content/frontmatter.zig");
    _ = @import("content/highlight.zig");
}
