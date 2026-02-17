# cot.land Feature Gap: Production Package Manager & Website in Cot

**Goal**: Identify every language feature, stdlib module, and infrastructure piece
needed to build cot.land — a full-stack package registry (server + website + CLI client)
— entirely in Cot.

**Contrast with self-hosting**: The self-hosting doc (`SELF_HOSTING_FEATURES.md`) focuses on
compiler-internal features (packed structs, @tagName, allocator traits). cot.land is a
**web application** — the requirements are almost entirely different: HTTP routing, database,
authentication, HTML rendering, file upload/download, TLS, deployment, concurrency.

**Scope**: Three components:
1. **Registry server** — REST API for package publish/download/search
2. **Website frontend** — browse packages, view docs, user accounts
3. **CLI client** — `cot add`, `cot remove`, `cot publish` commands

**Current state**: Empty directories at `www/land/` and `www/dev/`. No fly.io config,
no Dockerfile, no deployment infrastructure. The MCP server (`mcp/cot-mcp.cot`) is the
only real Cot application — 380 lines, single-file, stdio only.

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────┐
│                    cot.land                          │
│                                                     │
│  ┌──────────┐  ┌──────────────┐  ┌──────────────┐  │
│  │ Website  │  │ REST API     │  │ CLI Client   │  │
│  │ (SSR)    │  │ /api/v1/...  │  │ cot add/pub  │  │
│  └────┬─────┘  └──────┬───────┘  └──────┬───────┘  │
│       │               │                 │           │
│       └───────┬───────┘                 │           │
│               ▼                         │           │
│  ┌────────────────────┐                 │           │
│  │ HTTP Server        │◄────────────────┘           │
│  │ (routing, auth,    │                             │
│  │  middleware, TLS)   │                             │
│  └────────┬───────────┘                             │
│           ▼                                         │
│  ┌────────────────────┐  ┌────────────────────┐     │
│  │ SQLite Database    │  │ Package Storage    │     │
│  │ users, packages,   │  │ tarballs on disk   │     │
│  │ versions, tokens   │  │ or object store    │     │
│  └────────────────────┘  └────────────────────┘     │
└─────────────────────────────────────────────────────┘
```

---

## Component 1: Registry Server (REST API)

### Endpoints Needed

```
POST   /api/v1/packages                     # publish package
GET    /api/v1/packages/:name               # package metadata
GET    /api/v1/packages/:name/:version      # specific version
GET    /api/v1/packages/:name/download      # download tarball
GET    /api/v1/packages?q=search_term       # search
DELETE /api/v1/packages/:name/:version      # yank version
POST   /api/v1/users/register               # create account
POST   /api/v1/users/login                  # get API token
GET    /api/v1/users/me                     # current user info
POST   /api/v1/tokens                       # create API token
DELETE /api/v1/tokens/:id                   # revoke token
```

### Features Required

| Feature | Status | Gap |
|---------|--------|-----|
| TCP socket listener | HAVE | `std/http` — `tcpSocket()`, `bindSocket()`, `listenSocket()`, `acceptConnection()` |
| HTTP request parsing | **MISSING** | Need: parse method, path, headers, query params, body from raw TCP stream |
| HTTP response builder | PARTIAL | `httpResponse(status, body)` exists but only builds simple string responses. Need: headers, content-type, chunked transfer, status codes |
| URL routing | **MISSING** | Need: pattern matching on paths (`/api/v1/packages/:name`), method dispatch (GET/POST/DELETE), middleware chain |
| JSON request/response | HAVE | `std/json` — full parse/encode |
| Query string parsing | **MISSING** | `std/url` parses URL components but doesn't parse `?key=val&key2=val2` into a map |
| Request body reading | **MISSING** | Need: read Content-Length bytes from socket, or chunked transfer decoding |
| Multipart form parsing | **MISSING** | Need: parse `multipart/form-data` for file upload (package tarballs) |
| Auth tokens (Bearer) | **MISSING** | Need: parse `Authorization: Bearer <token>` header, validate against DB |
| Token generation | PARTIAL | `std/crypto` has SHA-256, `std/random` has `randomInt()`. Need: secure random token generation (hex or base64) |
| Rate limiting | **MISSING** | Need: per-IP or per-token request counting with time windows |
| Graceful shutdown | **MISSING** | Need: signal handling (`SIGTERM`/`SIGINT`) for clean deploy |
| Request logging | HAVE | `std/log` — structured logging with levels |
| Concurrent connections | **MISSING** | `std/async` has event loop but no multi-connection server pattern. Need: accept loop + per-connection async I/O |

---

## Component 2: Website (Server-Side Rendered)

### Pages Needed

```
/                                # landing page
/packages                        # browse/search packages
/packages/:name                  # package detail (readme, versions, deps)
/packages/:name/:version         # specific version detail
/packages/:name/:version/docs    # API documentation
/login                           # login form
/register                        # registration form
/dashboard                       # user's published packages
/settings                        # API tokens, profile
```

### Features Required

| Feature | Status | Gap |
|---------|--------|-----|
| HTML template rendering | **MISSING** | Need: template engine or string-based HTML builder. Even basic `<html>...</html>` construction requires string concatenation |
| HTML entity escaping | **MISSING** | Need: escape `<`, `>`, `&`, `"`, `'` in user content to prevent XSS |
| CSS serving (static files) | **MISSING** | Need: serve static files from disk (CSS, JS, images) with correct Content-Type |
| Markdown rendering | **MISSING** | Package READMEs are markdown — need parser or pass-through to client-side renderer |
| Form handling | **MISSING** | Need: parse `application/x-www-form-urlencoded` POST body |
| Session/cookie management | **MISSING** | Need: Set-Cookie header generation, cookie parsing from requests |
| CSRF protection | **MISSING** | Need: token-based CSRF mitigation for form submissions |
| Content-Type negotiation | **MISSING** | Need: serve HTML for browsers, JSON for API clients, based on Accept header |
| Pagination | **MISSING** | Need: LIMIT/OFFSET queries, next/prev links |
| Search | PARTIAL | String matching exists in `std/string`, but need full-text search in DB |

---

## Component 3: CLI Client (`cot add/publish`)

### Commands Needed

```
cot add <package>[@version]      # add dependency to cot.json + download
cot remove <package>             # remove dependency
cot publish                      # pack + upload to registry
cot login                        # authenticate with registry
cot search <query>               # search registry
cot info <package>               # show package details
```

### Features Required

| Feature | Status | Gap |
|---------|--------|-----|
| HTTP client (outbound) | **MISSING** | `std/http` only has server-side sockets. Need: `connectSocket()` exists but no HTTP client (send request, read response, follow redirects) |
| HTTPS/TLS client | **MISSING** | No TLS implementation. Production registries require HTTPS. Need: TLS 1.3 or delegate to system OpenSSL |
| Tar/gzip packing | **MISSING** | `cot publish` needs to create `.tar.gz` of package source. No `std/tar` or `std/compress` |
| Tar/gzip unpacking | **MISSING** | `cot add` needs to extract downloaded packages |
| Dependency resolution | **MISSING** | Semver constraint solving. `std/semver` has comparison but no constraint parsing (`^1.2.0`, `~1.0`, `>=2.0.0 <3.0.0`) |
| Lock file | **MISSING** | `cot.lock` — deterministic dependency graph, integrity hashes |
| Integrity verification | PARTIAL | `std/crypto` has SHA-256 for integrity hashes. Need: hash tarballs, compare on download |
| cot.json manifest reading | HAVE | `compiler/project.zig` reads `cot.json` via `std.json` |
| cot.json writing | **MISSING** | Need: update `cot.json` dependencies field (read-modify-write JSON) |
| Progress display | **MISSING** | Terminal progress bars, download percentage. Need: cursor movement, line clearing |
| Config file (~/.cotrc) | **MISSING** | Store auth tokens, registry URL. Need: file I/O to home directory |

---

## Language Feature Gaps

### Tier 1: Blocking (cannot build cot.land without these)

#### 1.1 HTTP Request Parser

**What**: Parse raw TCP byte stream into structured HTTP request (method, path, headers, body).

**Needed for**: Every single API endpoint and web page.

**Implementation**: Pure Cot stdlib module — `std/http` extension or new `std/server` module.

```cot
struct HttpRequest {
    method: string        // "GET", "POST", etc.
    path: string          // "/api/v1/packages/cot-json"
    query: string         // "q=search&page=2"
    headers: Map(string, string)
    body: string
    version: string       // "HTTP/1.1"
}

fn parseRequest(raw: string) HttpRequest { ... }
fn parseQueryString(qs: string) Map(string, string) { ... }
```

**Complexity**: Medium. HTTP/1.1 parsing is well-specified. Line-by-line header reading
with `\r\n` delimiter, Content-Length body reading. No chunked transfer needed initially.

**Depends on**: Nothing (all string ops exist).

---

#### 1.2 HTTP Response Builder (Full)

**What**: Build complete HTTP responses with status, headers, and body.

**Needed for**: Every response from server.

```cot
struct HttpResponse {
    status: i64
    headers: Map(string, string)
    body: string
}

fn respond(fd: i64, resp: HttpResponse) void { ... }
fn respondJson(fd: i64, status: i64, data: i64) void { ... }
fn respondHtml(fd: i64, status: i64, html: string) void { ... }
fn respondFile(fd: i64, path: string, content_type: string) void { ... }
fn redirect(fd: i64, url: string) void { ... }
```

**Complexity**: Low. String building with StringBuilder.

---

#### 1.3 URL Router

**What**: Match incoming request paths to handler functions, with parameter extraction.

**Needed for**: All API endpoints and web pages.

```cot
struct Router {
    routes: List(Route)
}

fn route(r: *Router, method: string, pattern: string, handler: fn(*Context) void) void
// Pattern: "/api/v1/packages/:name/:version"
// Extracts: ctx.param("name"), ctx.param("version")

struct Context {
    request: HttpRequest
    params: Map(string, string)
    writer_fd: i64
}
```

**Complexity**: Medium. Pattern matching with `:param` extraction. Linear scan of routes
is fine for <100 routes.

**Depends on**: 1.1 (HttpRequest), function pointers (HAVE).

---

#### 1.4 Database Access (SQLite)

**What**: Connect to SQLite, execute queries, read results.

**Needed for**: Users, packages, versions, tokens, downloads — all persistent data.

**This is the single biggest gap.** Without a database, cot.land cannot store anything
persistently. Options:

**Option A: FFI to libsqlite3** (recommended)
- SQLite is a single C file. Link against system libsqlite3.
- Need: `extern fn` declarations for C functions, or raw syscall-style builtins.
- Builtins: `@sqlite_open(path)`, `@sqlite_exec(db, sql)`, `@sqlite_prepare(db, sql)`,
  `@sqlite_bind_text(stmt, idx, val)`, `@sqlite_step(stmt)`, `@sqlite_column_text(stmt, idx)`,
  `@sqlite_close(db)`, `@sqlite_finalize(stmt)`.
- Implementation: Same pattern as WASI builtins — register in `wasi_runtime.zig`,
  native override in `driver.zig` calling actual libsqlite3 via dynamic linking or
  static linking.

**Option B: File-based JSON store** (stopgap)
- Store data as JSON files on disk.
- Workable for <1000 packages. Not production-grade.
- Uses existing `std/json` + `std/fs`.

**Option C: External database via TCP** (postgres)
- Connect to PostgreSQL over TCP.
- Need: PostgreSQL wire protocol implementation.
- Much more complex than SQLite.

```cot
// Option A API
import "std/db"

var db = sqliteOpen("cot.db")
defer sqliteClose(db)

sqliteExec(db, "CREATE TABLE IF NOT EXISTS packages (name TEXT, version TEXT)")

var stmt = sqlitePrepare(db, "SELECT * FROM packages WHERE name = ?")
sqliteBindText(stmt, 1, "cot-json")
while (sqliteStep(stmt) == ROW) {
    var name = sqliteColumnText(stmt, 0)
    var version = sqliteColumnText(stmt, 1)
}
sqliteFinalize(stmt)
```

**Complexity**: HIGH. Linking against C library is a new capability.

**Depends on**: C FFI or builtin registration pattern (HAVE for builtins).

---

#### 1.5 Concurrent Connection Handling

**What**: Accept and handle multiple simultaneous TCP connections.

**Needed for**: Any server that handles more than one user at a time.

**Current state**: `std/async` has `eventLoopCreate()`, `asyncAccept()`, `asyncRead()`,
`asyncWrite()` — the building blocks exist. But there's no high-level pattern for
"accept connections in a loop and handle each one concurrently."

**What's needed**: A server accept loop pattern:

```cot
import "std/async"
import "std/http"

var loop = eventLoopCreate()
var listen_fd = try tcpListen(8080)

while (true) {
    var client_fd = try asyncAccept(loop, listen_fd)
    // Need: handle client_fd without blocking the accept loop
    // Option A: spawn (0.6 feature — goroutine-style)
    // Option B: event-driven (register client_fd on event loop, handle in callbacks)
    // Option C: single-threaded event loop with async I/O (Node.js-style)
}
```

**Option C is achievable today** with `std/async` — register each client fd for read
events, process requests in the event loop callback, write responses. This is the
Node.js/Deno model. Single-threaded but handles many connections via non-blocking I/O.

**Complexity**: Medium. Pattern exists in `std/async`, needs a higher-level wrapper.

**Depends on**: Nothing new (async builtins exist).

---

#### 1.6 HTML Template Rendering

**What**: Generate HTML pages with dynamic content.

**Needed for**: Every website page.

**Options**:

**Option A: StringBuilder-based** (simplest, works today)
```cot
fn renderPackagePage(pkg: Package) string {
    var sb = StringBuilder { .buf = 0, .len = 0, .cap = 0 }
    sb.append("<!DOCTYPE html><html><head><title>")
    sb.append(htmlEscape(pkg.name))
    sb.append("</title></head><body><h1>")
    sb.append(htmlEscape(pkg.name))
    sb.append("</h1><p>")
    sb.append(htmlEscape(pkg.description))
    sb.append("</p></body></html>")
    return sb.toString()
}
```

**Option B: Template engine** (better DX, more complex)
```cot
// template: "Hello, {{name}}! You have {{count}} packages."
fn renderTemplate(template: string, vars: Map(string, string)) string { ... }
```

**Option C: Static HTML + JSON API** (SPA approach)
- Serve static HTML/CSS/JS
- Frontend fetches data via JSON API
- Requires browser-side Cot or plain JavaScript

**Recommendation**: Start with Option A (StringBuilder). It's ugly but works today
with zero new features. Add Option B later as `std/template`.

**Complexity**: Low for Option A. Medium for Option B.

---

#### 1.7 HTML Entity Escaping

**What**: Escape `<`, `>`, `&`, `"`, `'` in user-provided strings before embedding in HTML.

**Needed for**: XSS prevention on every page.

```cot
fn htmlEscape(s: string) string {
    // & → &amp;  < → &lt;  > → &gt;  " → &quot;  ' → &#x27;
}
```

**Complexity**: Trivial. 20 lines of Cot code.

**Depends on**: Nothing.

---

#### 1.8 Cookie Parsing and Generation

**What**: Parse `Cookie` header, generate `Set-Cookie` header.

**Needed for**: User sessions on the website.

```cot
fn parseCookies(header: string) Map(string, string)
fn setCookie(name: string, value: string, max_age: i64, path: string, http_only: bool) string
```

**Complexity**: Low. String parsing.

---

### Tier 2: Important (production quality requires these)

#### 2.1 TLS/HTTPS

**What**: TLS 1.2/1.3 for encrypted connections.

**Needed for**: Production deployment. Without HTTPS, browsers show warnings, API tokens
are sent in cleartext, and package integrity is compromised.

**Options**:

**Option A: Reverse proxy** (recommended for v1)
- Run cot.land behind nginx/Caddy/Cloudflare which handles TLS.
- cot.land listens on HTTP internally, proxy terminates TLS.
- Zero Cot changes needed.
- This is how most production services work (Go, Rust, Node all do this).

**Option B: System TLS** (future)
- Link against system OpenSSL/LibreSSL.
- Same pattern as SQLite FFI.
- Very complex (~5000 lines of binding code).

**Recommendation**: Option A. Use Caddy or fly.io's built-in TLS termination.

---

#### 2.2 Tar/Gzip

**What**: Create and extract `.tar.gz` archives.

**Needed for**: `cot publish` (pack source) and `cot add` (unpack downloaded package).

**Options**:

**Option A: Shell out** (works on native)
```cot
import "std/process"
run2("tar", "czf", "package.tar.gz")  // create
run2("tar", "xzf", "package.tar.gz")  // extract
```

**Option B: Pure Cot implementation**
- Tar format is simple (512-byte header blocks).
- Gzip requires DEFLATE — complex (~2000 lines).
- Or use zlib FFI.

**Recommendation**: Option A for v1. Shell out to system `tar`. Works everywhere
that has `tar` (all Unix, macOS, most Windows via Git Bash).

---

#### 2.3 HTTP Client

**What**: Make outbound HTTP requests (for CLI client).

**Needed for**: `cot add` (download packages), `cot publish` (upload), `cot search`.

```cot
struct HttpClientResponse {
    status: i64
    headers: Map(string, string)
    body: string
}

fn httpGet(url: string) HttpClientResponse { ... }
fn httpPost(url: string, body: string, headers: Map(string, string)) HttpClientResponse { ... }
```

**Implementation**: `connectSocket()` exists. Need: format HTTP request, send over socket,
read response (status line + headers + body). For HTTPS: connect via TLS or use system
`curl` as fallback.

**Complexity**: Medium for HTTP. High for HTTPS (needs TLS or curl fallback).

---

#### 2.4 Semver Constraint Parsing

**What**: Parse version constraints like `^1.2.0`, `~1.0`, `>=2.0.0 <3.0.0`.

**Needed for**: `cot add` dependency resolution. `cot.json` dependencies field.

**Current state**: `std/semver` has `parse()`, `gt()`, `lt()`, `eq()`, `compare()` —
all the comparison primitives. Missing: constraint parsing and satisfaction checking.

```cot
fn parseConstraint(s: string) Constraint { ... }
fn satisfies(version: i64, constraint: Constraint) bool { ... }
fn resolveVersion(available: List(i64), constraint: Constraint) i64 { ... }
```

**Complexity**: Medium. Well-defined semantics (npm/cargo semver rules).

---

#### 2.5 Secure Random Token Generation

**What**: Generate cryptographically secure random tokens for API auth.

**Needed for**: `POST /api/v1/tokens`, user sessions.

**Current state**: `@random(buf, len)` uses `getentropy()` — cryptographically secure.
`std/encoding` has `hexEncode()` and `base64UrlEncode()`.

```cot
fn generateToken() string {
    var buf = @alloc(32)
    @random(buf, 32)
    return hexEncode(@string(buf, 32))  // 64-char hex token
}
```

**This actually works TODAY.** The primitives exist. Just need to wire them together.

---

#### 2.6 Password Hashing

**What**: Hash passwords for storage (bcrypt, argon2, or PBKDF2).

**Needed for**: User registration and login.

**Current state**: `std/crypto` has SHA-256 and HMAC-SHA256. These are NOT suitable for
password hashing (too fast, no salt stretching).

**Options**:

**Option A: PBKDF2-HMAC-SHA256** (implementable in pure Cot)
- 100 lines of Cot code. Uses existing HMAC-SHA256 in a loop.
- OWASP-recommended with 600,000+ iterations.

**Option B: FFI to system bcrypt/argon2**
- Better security properties but requires C FFI.

**Recommendation**: Option A (PBKDF2). Implementable today with existing `std/crypto`.

---

### Tier 3: Nice to Have (polish, can defer)

| Feature | Description | Workaround |
|---------|-------------|------------|
| Markdown parser | Render package READMEs | Serve raw markdown, render client-side with JS |
| Full-text search | Search package names + descriptions | LIKE queries in SQLite |
| Email sending | Account verification, password reset | Skip email for v1, use token-only auth |
| WebSocket support | Real-time updates (download counts) | HTTP polling |
| Image/avatar upload | User profile pictures | Gravatar or initials |
| CI/CD webhooks | Trigger builds on publish | Manual for v1 |
| API pagination | Page through large result sets | LIMIT/OFFSET in SQL |
| OpenAPI spec | Auto-generated API docs | Manual documentation |
| i18n | Internationalization | English only for v1 |
| Caching layer | Redis/memcached for hot data | SQLite is fast enough for <10K packages |

---

## Infrastructure Gaps

### Deployment

| Requirement | Status | Solution |
|-------------|--------|----------|
| Dockerfile | **MISSING** | Multi-stage: Zig build → scratch + binary |
| fly.toml | **MISSING** | fly.io config for deployment |
| Health check endpoint | **MISSING** | `GET /health` → 200 OK |
| Environment variables | PARTIAL | `std/os` has `environ()` but no `getenv(name)` convenience |
| Persistent storage | **MISSING** | fly.io volumes for SQLite + package tarballs |
| DNS/domain | UNKNOWN | cot.land domain presumably owned, needs DNS config |
| TLS certificate | N/A | fly.io handles TLS termination automatically |
| CI/CD for server | PARTIAL | GitHub Actions exists for compiler releases |
| Monitoring/alerting | **MISSING** | fly.io metrics + uptime check |
| Backup strategy | **MISSING** | SQLite backup + tarball sync |

### Dockerfile Template

```dockerfile
FROM ghcr.io/nickg/zig:0.15.0 AS builder
WORKDIR /app
COPY . .
RUN zig build
RUN ./zig-out/bin/cot build www/land/main.cot -o cot-land

FROM scratch
COPY --from=builder /app/cot-land /cot-land
COPY --from=builder /app/www/land/static /static
EXPOSE 8080
ENTRYPOINT ["/cot-land"]
```

### fly.toml Template

```toml
app = "cot-land"
primary_region = "sjc"

[build]
  dockerfile = "Dockerfile"

[http_service]
  internal_port = 8080
  force_https = true
  auto_stop_machines = true
  auto_start_machines = true

[mounts]
  source = "cot_data"
  destination = "/data"
```

---

## Data Model

### SQLite Schema

```sql
CREATE TABLE users (
    id INTEGER PRIMARY KEY,
    username TEXT UNIQUE NOT NULL,
    email TEXT UNIQUE NOT NULL,
    password_hash TEXT NOT NULL,
    created_at INTEGER NOT NULL,
    updated_at INTEGER NOT NULL
);

CREATE TABLE api_tokens (
    id INTEGER PRIMARY KEY,
    user_id INTEGER NOT NULL REFERENCES users(id),
    token_hash TEXT UNIQUE NOT NULL,
    name TEXT NOT NULL,
    created_at INTEGER NOT NULL,
    last_used_at INTEGER,
    revoked INTEGER DEFAULT 0
);

CREATE TABLE packages (
    id INTEGER PRIMARY KEY,
    name TEXT UNIQUE NOT NULL,
    owner_id INTEGER NOT NULL REFERENCES users(id),
    description TEXT DEFAULT '',
    repository TEXT DEFAULT '',
    license TEXT DEFAULT '',
    created_at INTEGER NOT NULL,
    updated_at INTEGER NOT NULL,
    downloads INTEGER DEFAULT 0
);

CREATE TABLE versions (
    id INTEGER PRIMARY KEY,
    package_id INTEGER NOT NULL REFERENCES packages(id),
    version TEXT NOT NULL,
    checksum TEXT NOT NULL,
    tarball_path TEXT NOT NULL,
    readme TEXT DEFAULT '',
    dependencies TEXT DEFAULT '{}',
    cot_version TEXT DEFAULT '',
    published_at INTEGER NOT NULL,
    yanked INTEGER DEFAULT 0,
    UNIQUE(package_id, version)
);

CREATE INDEX idx_packages_name ON packages(name);
CREATE INDEX idx_versions_package ON versions(package_id);
CREATE INDEX idx_tokens_hash ON api_tokens(token_hash);
```

---

## Implementation Order

### Phase 1: Minimal Viable Server (stdlib + patterns)

Goal: HTTP server that serves JSON API endpoints. No database, no auth — in-memory data.

| Step | Feature | Effort | Module |
|------|---------|--------|--------|
| 1.1 | HTTP request parser | 2 days | `stdlib/server.cot` |
| 1.2 | HTTP response builder (full) | 1 day | `stdlib/server.cot` |
| 1.3 | URL router with params | 2 days | `stdlib/server.cot` |
| 1.4 | HTML escape function | 0.5 days | `stdlib/html.cot` |
| 1.5 | Cookie parse/generate | 1 day | `stdlib/server.cot` |
| 1.6 | Query string parser | 0.5 days | `stdlib/url.cot` (extend) |
| 1.7 | Event-loop server pattern | 2 days | `www/land/main.cot` |

**Deliverable**: `cot build www/land/main.cot -o cot-land && ./cot-land`
serves `GET /api/v1/packages` returning hardcoded JSON.

**Language features needed**: None new. All achievable with existing stdlib.

---

### Phase 2: Persistence (SQLite)

Goal: Store packages, users, versions in SQLite. Real data.

| Step | Feature | Effort | Module |
|------|---------|--------|--------|
| 2.1 | SQLite builtins (8 functions) | 3 days | `wasi_runtime.zig`, `driver.zig` |
| 2.2 | `std/db` wrapper module | 2 days | `stdlib/db.cot` |
| 2.3 | Schema creation + migration | 1 day | `www/land/db.cot` |
| 2.4 | CRUD for packages + versions | 2 days | `www/land/api.cot` |

**Deliverable**: API endpoints store and retrieve data from SQLite.

**Language features needed**: SQLite builtins (new compiler work in `wasi_runtime.zig`).

---

### Phase 3: Authentication

Goal: User registration, login, API tokens.

| Step | Feature | Effort | Module |
|------|---------|--------|--------|
| 3.1 | PBKDF2 password hashing | 1 day | `stdlib/crypto.cot` (extend) |
| 3.2 | Secure token generation | 0.5 days | `stdlib/crypto.cot` (extend) |
| 3.3 | Auth middleware | 1 day | `www/land/auth.cot` |
| 3.4 | User registration + login endpoints | 2 days | `www/land/api.cot` |
| 3.5 | Token CRUD endpoints | 1 day | `www/land/api.cot` |

**Deliverable**: `cot login` authenticates, `cot publish` requires valid token.

**Language features needed**: None new.

---

### Phase 4: Package Upload/Download

Goal: Actually publish and download packages.

| Step | Feature | Effort | Module |
|------|---------|--------|--------|
| 4.1 | Tar packing (shell out) | 1 day | CLI in `compiler/cli.zig` |
| 4.2 | Multipart upload parsing | 2 days | `stdlib/server.cot` |
| 4.3 | Tarball storage + integrity | 1 day | `www/land/storage.cot` |
| 4.4 | Download endpoint | 1 day | `www/land/api.cot` |
| 4.5 | `cot add` / `cot remove` CLI | 3 days | `compiler/cli.zig` + `compiler/package.zig` |
| 4.6 | Dependency resolution | 3 days | `compiler/resolver.zig` |
| 4.7 | Lock file generation | 2 days | `compiler/lockfile.zig` |

**Deliverable**: Full publish → add → build cycle works.

**Language features needed**: None new (tar via shell, HTTP client via `connectSocket()`).

---

### Phase 5: Website

Goal: Server-rendered HTML pages for browsing packages.

| Step | Feature | Effort | Module |
|------|---------|--------|--------|
| 5.1 | HTML page templates | 3 days | `www/land/views.cot` |
| 5.2 | Static file serving | 1 day | `www/land/static.cot` |
| 5.3 | CSS styling | 2 days | `www/land/static/style.css` |
| 5.4 | Package listing page | 1 day | `www/land/views.cot` |
| 5.5 | Package detail page | 1 day | `www/land/views.cot` |
| 5.6 | Search | 1 day | SQL LIKE queries |
| 5.7 | Login/register pages | 2 days | `www/land/views.cot` |

**Deliverable**: cot.land has a browsable website.

**Language features needed**: None new.

---

### Phase 6: Deployment

Goal: Running on fly.io.

| Step | Feature | Effort | Module |
|------|---------|--------|--------|
| 6.1 | Dockerfile | 0.5 days | `Dockerfile` |
| 6.2 | fly.toml | 0.5 days | `fly.toml` |
| 6.3 | Health check endpoint | 0.5 days | `www/land/main.cot` |
| 6.4 | Environment variable config | 0.5 days | `www/land/config.cot` |
| 6.5 | Persistent volume setup | 0.5 days | fly.io console |
| 6.6 | DNS + TLS | 0.5 days | fly.io console |
| 6.7 | CI/CD deploy | 1 day | `.github/workflows/deploy.yml` |

**Deliverable**: cot.land is live on the internet.

**Language features needed**: None new.

---

## Total Effort Estimate

| Phase | Description | Effort |
|-------|-------------|--------|
| Phase 1 | Minimal HTTP server | ~9 days |
| Phase 2 | SQLite persistence | ~8 days |
| Phase 3 | Authentication | ~5.5 days |
| Phase 4 | Package upload/download | ~13 days |
| Phase 5 | Website | ~11 days |
| Phase 6 | Deployment | ~4 days |
| **Total** | | **~50.5 days** |

---

## Critical Path Analysis

The **single blocking dependency** is SQLite (Phase 2). Everything else can be built
with existing language features and stdlib modules.

```
Phase 1 ──────► Phase 3 ──────► Phase 4
    │               │               │
    └──► Phase 2 ───┘               │
                                    ▼
                              Phase 5 ──► Phase 6
```

**If SQLite builtins are too complex**, the JSON-file-store fallback (Option B in 1.4)
enables progress without compiler changes. Replace SQLite calls with:

```cot
fn loadPackages() List(Package) {
    var data = readFile("/data/packages.json")
    return parsePackagesJson(data)
}

fn savePackages(pkgs: List(Package)) void {
    var json = encodePackagesJson(pkgs)
    writeFile("/data/packages.json", json)
}
```

This is not production-grade (no concurrent writes, no queries, no indexing) but
unblocks development of everything else.

---

## Comparison: Self-Hosting vs cot.land

| Dimension | Self-Hosting (compiler) | cot.land (web app) |
|-----------|------------------------|-------------------|
| Core need | Type system, AST, codegen | HTTP, database, auth |
| Key blocker | Packed structs, allocator trait | SQLite, HTTP parser |
| Data structures | Trees, graphs, maps | Tables, rows, queries |
| I/O pattern | File → process → file | Request → process → response |
| Concurrency | Single-threaded (compile one file) | Multi-connection (many users) |
| Memory pattern | Arena (per-compilation) | Long-lived (server uptime) |
| String handling | Token/AST manipulation | HTTP headers, HTML, JSON |
| Error handling | Compile errors (rich) | HTTP errors (status codes) |
| Stdlib deps | `std/mem`, `std/fmt`, `std/debug` | `std/http`, `std/db`, `std/json`, `std/crypto` |
| New builtins | @tagName, @errorName, @offsetOf | @sqlite_open, @sqlite_exec, etc. |
| Existing feature coverage | ~75% (needs packed structs, nested types) | ~40% (needs HTTP parser, DB, auth) |
| Estimated new Cot code | ~4900 lines compiler, ~1500 lines stdlib | ~3000 lines stdlib, ~5000 lines app |

**Key insight**: Self-hosting exercises the **type system and codegen** deeply.
cot.land exercises the **I/O, networking, and data persistence** deeply. Together,
they prove the full language — the compiler proves correctness, the web app proves utility.

---

## What Works TODAY (Honest Assessment)

A developer could build a **toy package registry** today with:
- TCP server via `std/http` (accept connections, read bytes, write bytes)
- JSON API via `std/json` (parse/encode)
- String manipulation via `std/string` (HTTP parsing by hand)
- File storage via `std/fs` (save tarballs, load metadata)
- Crypto via `std/crypto` (SHA-256 integrity hashes)
- Event loop via `std/async` (handle multiple connections)

What makes it **toy** vs **production**:
1. No database → data lost on restart (unless JSON files on disk)
2. No proper HTTP parsing → fragile, can't handle edge cases
3. No TLS → insecure without a reverse proxy
4. No tar/gzip → can't pack/unpack packages
5. No HTTP client → CLI can't talk to registry
6. Single-threaded → performance ceiling (but Node.js manages fine single-threaded)

**Bottom line**: Cot is ~60% of the way to building cot.land. The HTTP server building
blocks exist. The biggest gap is database access. The second biggest is a proper HTTP
request parser (currently would have to parse raw TCP bytes manually). Everything else
is either existing or implementable in pure Cot without compiler changes.
