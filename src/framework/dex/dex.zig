//! Dex: Full-Stack Web Framework for Cot
//!
//! Dex provides the server-side infrastructure for building web applications
//! with Cot as the backend and React/Vue/Svelte as the frontend.
//!
//! The core framework code is written in Zig and lives in the cot repo.
//! Frontend packages (@dex/react, etc.) live in the cotlang/dex repo.
//!
//! Named after Dexter, John's dog.

const std = @import("std");

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

test {
    _ = @import("page.zig");
    _ = @import("document.zig");
    _ = @import("response.zig");
    _ = @import("router.zig");
    _ = @import("layout.zig");
}
