# Standard Library Reference

Cot ships with 34 stdlib modules. Import them with `import "std/<module>"`.

## std/list

Dynamic array. Import: `import "std/list"`

```zig
var numbers: List(i64) = .{}
numbers.append(42)
println(numbers.get(0))    // 42
```

### List(T) Methods

| Method | Signature | Description |
|--------|-----------|-------------|
| `append` | `fn append(self, value: T) void` | Add element to end |
| `get` | `fn get(self, index: i64) T` | Get element (bounds-checked) |
| `set` | `fn set(self, index: i64, value: T) void` | Set element (bounds-checked) |
| `pop` | `fn pop(self) T` | Remove and return last element |
| `len` | `fn len(self) i64` | Element count |
| `cap` | `fn cap(self) i64` | Allocated capacity |
| `first` | `fn first(self) T` | First element |
| `last` | `fn last(self) T` | Last element |
| `isEmpty` | `fn isEmpty(self) i64` | 1 if empty |
| `insert` | `fn insert(self, index: i64, value: T) void` | Insert at index, shifting right |
| `orderedRemove` | `fn orderedRemove(self, index: i64) T` | Remove preserving order |
| `swapRemove` | `fn swapRemove(self, index: i64) T` | Remove by swapping with last |
| `indexOf` | `fn indexOf(self, value: T) i64` | First index of value, or -1 |
| `contains` | `fn contains(self, value: T) i64` | 1 if present |
| `equal` | `fn equal(self, other: List(T)) i64` | 1 if same contents |
| `reverse` | `fn reverse(self) void` | Reverse in-place |
| `clone` | `fn clone(self) List(T)` | Deep copy |
| `clear` | `fn clear(self) void` | Set count to 0, keep allocation |
| `free` | `fn free(self) void` | Deallocate memory |
| `resize` | `fn resize(self, new_len: i64) void` | Set length (grows if needed) |
| `appendNTimes` | `fn appendNTimes(self, value: T, n: i64) void` | Append same value n times |
| `appendSlice` | `fn appendSlice(self, source: i64, num: i64) void` | Append from raw pointer |
| `deleteRange` | `fn deleteRange(self, start: i64, end: i64) void` | Remove [start, end) |
| `compact` | `fn compact(self) void` | Remove consecutive duplicates |
| `sort` | `fn sort(self, cmp: fn(T, T) -> i64) void` | Sort with comparator |
| `isSorted` | `fn isSorted(self, cmp: fn(T, T) -> i64) i64` | 1 if sorted |
| `containsFunc` | `fn containsFunc(self, pred: fn(T) -> i64) i64` | 1 if any matches predicate |
| `indexOfFunc` | `fn indexOfFunc(self, pred: fn(T) -> i64) i64` | First matching index, or -1 |
| `removeIf` | `fn removeIf(self, pred: fn(T) -> i64) void` | Remove all matching elements |

---

## std/map

Hash map with linear probing. Import: `import "std/map"`

```zig
var ages: Map(i64, i64) = .{}
ages.set(1, 25)
ages.set(2, 30)
println(ages.get(1))       // 25
println(ages.has(3))       // 0
```

### Map(K, V) Methods

| Method | Signature | Description |
|--------|-----------|-------------|
| `set` | `fn set(self, key: K, value: V) void` | Insert or update |
| `get` | `fn get(self, key: K) V` | Get value (traps if missing) |
| `getOrDefault` | `fn getOrDefault(self, key: K, default: V) V` | Get or return default |
| `has` | `fn has(self, key: K) i64` | 1 if key exists |
| `delete` | `fn delete(self, key: K) void` | Remove key |
| `len` | `fn len(self) i64` | Entry count |
| `isEmpty` | `fn isEmpty(self) i64` | 1 if empty |
| `keys` | `fn keys(self) List(K)` | List of all keys |
| `values` | `fn values(self) List(V)` | List of all values |
| `clear` | `fn clear(self) void` | Remove all entries, keep allocation |
| `free` | `fn free(self) void` | Deallocate memory |

---

## std/set

Hash set (wrapper over Map). Import: `import "std/set"`

```zig
var seen: Set(i64) = .{}
seen.add(42)
println(seen.has(42))      // 1
seen.remove(42)
```

### Set(T) Methods

| Method | Signature | Description |
|--------|-----------|-------------|
| `add` | `fn add(self, value: T) void` | Add value |
| `has` | `fn has(self, value: T) i64` | 1 if present |
| `remove` | `fn remove(self, value: T) void` | Remove value |
| `len` | `fn len(self) i64` | Element count |
| `isEmpty` | `fn isEmpty(self) i64` | 1 if empty |
| `clear` | `fn clear(self) void` | Remove all, keep allocation |
| `toList` | `fn toList(self) List(T)` | Convert to List |
| `free` | `fn free(self) void` | Deallocate memory |

---

## std/string

String manipulation + StringBuilder. Import: `import "std/string"`

```zig
var s = "Hello, World!"
println(contains(s, "World"))   // true
println(toUpper(s))             // HELLO, WORLD!
println(trim("  hi  "))        // hi
```

### Free Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `charAt` | `fn charAt(s: string, index: i64) i64` | Byte at index |
| `indexOf` | `fn indexOf(s: string, needle: string) i64` | First index, or -1 |
| `lastIndexOf` | `fn lastIndexOf(s: string, needle: string) i64` | Last index, or -1 |
| `contains` | `fn contains(s: string, needle: string) bool` | True if found |
| `startsWith` | `fn startsWith(s: string, prefix: string) bool` | Prefix check |
| `endsWith` | `fn endsWith(s: string, suffix: string) bool` | Suffix check |
| `count` | `fn count(s: string, needle: string) i64` | Non-overlapping occurrences |
| `substring` | `fn substring(s: string, start: i64, end: i64) string` | Extract [start, end) |
| `trim` | `fn trim(s: string) string` | Trim whitespace both sides |
| `trimLeft` | `fn trimLeft(s: string) string` | Trim leading whitespace |
| `trimRight` | `fn trimRight(s: string) string` | Trim trailing whitespace |
| `toUpper` | `fn toUpper(s: string) string` | Uppercase (ASCII) |
| `toLower` | `fn toLower(s: string) string` | Lowercase (ASCII) |
| `replace` | `fn replace(s: string, old: string, new_s: string) string` | Replace all occurrences |
| `repeat` | `fn repeat(s: string, n: i64) string` | Repeat n times |
| `splitInto` | `fn splitInto(s: string, sep: string, result: *List(string)) void` | Split by separator |
| `parseInt` | `fn parseInt(s: string) i64` | Parse integer (0 on failure) |
| `parseIntOr` | `fn parseIntOr(s: string, default: i64) i64` | Parse with default |
| `intToString` | `fn intToString(n: i64) string` | Integer to string |
| `strEqual` | `fn strEqual(a: string, b: string) bool` | Equality check |
| `compare` | `fn compare(a: string, b: string) i64` | Lexicographic: -1, 0, 1 |
| `isDigit` | `fn isDigit(c: i64) bool` | ASCII digit? |
| `isAlpha` | `fn isAlpha(c: i64) bool` | ASCII letter? |
| `isWhitespace` | `fn isWhitespace(c: i64) bool` | Whitespace? |

### StringBuilder

```zig
var sb = StringBuilder { .buf = 0, .len = 0, .cap = 0 }
sb.append("Hello")
sb.append(", ")
sb.appendInt(42)
println(sb.toString())     // Hello, 42
sb.free()
```

| Method | Signature | Description |
|--------|-----------|-------------|
| `append` | `fn append(self: *StringBuilder, s: string) void` | Append string |
| `appendByte` | `fn appendByte(self: *StringBuilder, b: i64) void` | Append byte |
| `appendInt` | `fn appendInt(self: *StringBuilder, n: i64) void` | Append integer as string |
| `toString` | `fn toString(self: *StringBuilder) string` | Get accumulated string |
| `length` | `fn length(self: *StringBuilder) i64` | Current length |
| `clear` | `fn clear(self: *StringBuilder) void` | Reset (keep allocation) |
| `free` | `fn free(self: *StringBuilder) void` | Free buffer |

---

## std/math

Math utilities. Import: `import "std/math"`

### Constants

`PI`, `TAU`, `E`, `LN2`, `LN10`, `MAX_I64`, `MIN_I64`

### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `abs` | `fn abs(x: i64) i64` | Absolute value |
| `min` | `fn min(a: i64, b: i64) i64` | Minimum |
| `max` | `fn max(a: i64, b: i64) i64` | Maximum |
| `clamp` | `fn clamp(x: i64, lo: i64, hi: i64) i64` | Clamp to [lo, hi] |
| `fabs` | `fn fabs(x: f64) f64` | Float absolute value |
| `ceil` | `fn ceil(x: f64) f64` | Ceiling |
| `floor` | `fn floor(x: f64) f64` | Floor |
| `trunc` | `fn trunc(x: f64) f64` | Truncate toward zero |
| `round` | `fn round(x: f64) f64` | Round to nearest |
| `sqrt` | `fn sqrt(x: f64) f64` | Square root |
| `fmin` | `fn fmin(a: f64, b: f64) f64` | Float minimum |
| `fmax` | `fn fmax(a: f64, b: f64) f64` | Float maximum |
| `toInt` | `fn toInt(x: f64) i64` | Float to integer |
| `toFloat` | `fn toFloat(x: i64) f64` | Integer to float |
| `ipow` | `fn ipow(base: i64, exp: i64) i64` | Integer power |
| `fpow` | `fn fpow(base: f64, n: i64) f64` | Float power (integer exp) |

---

## std/json

JSON parser and encoder. Import: `import "std/json"`

```zig
// Parse
var root = parse("{\"name\": \"Cot\", \"version\": 3}")
println(jsonObjectGetString(root, "name"))    // Cot
println(jsonObjectGetInt(root, "version"))     // 3

// Build
var obj = jsonObject()
jsonObjectPut(obj, "hello", jsonString("world"))
jsonObjectPut(obj, "num", jsonInt(42))
println(encode(obj))    // {"hello":"world","num":42}
```

### Constructors

| Function | Signature | Description |
|----------|-----------|-------------|
| `jsonNull` | `fn jsonNull() i64` | Create null |
| `jsonBool` | `fn jsonBool(val: bool) i64` | Create boolean |
| `jsonInt` | `fn jsonInt(val: i64) i64` | Create integer |
| `jsonString` | `fn jsonString(val: string) i64` | Create string |
| `jsonArray` | `fn jsonArray() i64` | Create empty array |
| `jsonObject` | `fn jsonObject() i64` | Create empty object |

### Accessors

| Function | Signature | Description |
|----------|-----------|-------------|
| `jsonTag` | `fn jsonTag(val: i64) i64` | Type tag (0-5) |
| `jsonIsNull` | `fn jsonIsNull(val: i64) bool` | Check if null |
| `jsonGetBool` | `fn jsonGetBool(val: i64) bool` | Extract boolean |
| `jsonGetInt` | `fn jsonGetInt(val: i64) i64` | Extract integer |
| `jsonGetString` | `fn jsonGetString(val: i64) string` | Extract string |

### Array Operations

| Function | Signature | Description |
|----------|-----------|-------------|
| `jsonArrayLen` | `fn jsonArrayLen(val: i64) i64` | Array length |
| `jsonArrayGet` | `fn jsonArrayGet(val: i64, index: i64) i64` | Get element |
| `jsonArrayPush` | `fn jsonArrayPush(arr: i64, val: i64) void` | Append element |

### Object Operations

| Function | Signature | Description |
|----------|-----------|-------------|
| `jsonObjectLen` | `fn jsonObjectLen(val: i64) i64` | Key count |
| `jsonObjectPut` | `fn jsonObjectPut(obj: i64, key: string, val: i64) void` | Set key-value |
| `jsonObjectGet` | `fn jsonObjectGet(obj: i64, key: string) i64` | Get value (0 if missing) |
| `jsonObjectGetString` | `fn jsonObjectGetString(obj: i64, key: string) string` | Get string by key |
| `jsonObjectGetInt` | `fn jsonObjectGetInt(obj: i64, key: string) i64` | Get integer by key |
| `jsonObjectGetBool` | `fn jsonObjectGetBool(obj: i64, key: string) bool` | Get boolean by key |

### Parse & Encode

| Function | Signature | Description |
|----------|-----------|-------------|
| `parse` | `fn parse(input: string) i64` | Parse JSON string |
| `encode` | `fn encode(val: i64) string` | Serialize to JSON string |

---

## std/fs

File I/O. Import: `import "std/fs"`

```zig
// Read entire file
var content = readFile("config.txt")

// Write file
writeFile("output.txt", "Hello!")

// Structured file I/O
var f = openFile("data.bin", O_RDONLY)
var buf = alloc(0, 1024)
var n = try f.read(buf, 1024)
f.close()
```

### Error Type

`const FsError = error { NotFound, PermissionDenied, IoError }`

### Free Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `readFile` | `fn readFile(path: string) string` | Read entire file |
| `writeFile` | `fn writeFile(path: string, data: string) void` | Write string to file |
| `openFile` | `fn openFile(path: string, flags: i64) File` | Open with flags |
| `createFile` | `fn createFile(path: string) File` | Create/truncate for writing |
| `stdin` | `fn stdin() File` | fd 0 |
| `stdout` | `fn stdout() File` | fd 1 |
| `stderr` | `fn stderr() File` | fd 2 |

### File Methods

| Method | Signature | Description |
|--------|-----------|-------------|
| `read` | `fn read(self, buf: i64, len: i64) FsError!i64` | Read bytes |
| `write` | `fn write(self, buf: i64, len: i64) FsError!i64` | Write bytes |
| `writeAll` | `fn writeAll(self, s: string) FsError!i64` | Write string |
| `seekTo` | `fn seekTo(self, pos: i64) FsError!i64` | Seek to position |
| `seekBy` | `fn seekBy(self, delta: i64) FsError!i64` | Seek by offset |
| `getPos` | `fn getPos(self) FsError!i64` | Current position |
| `close` | `fn close(self) void` | Close file |
| `isValid` | `fn isValid(self) i64` | 1 if fd >= 0 |

### Constants

`O_RDONLY` (0), `O_WRONLY` (1), `O_RDWR` (2), `O_CREAT`, `O_TRUNC`, `O_APPEND`, `O_CREATE`, `SEEK_SET` (0), `SEEK_CUR` (1), `SEEK_END` (2)

---

## std/os

Process args and environment. Import: `import "std/os"`

```zig
// Command-line arguments
var count = argsCount()
for i in 0..count {
    println(arg(i))
}

// Exit
exit(0)
```

### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `exit` | `fn exit(code: i64) void` | Exit process |
| `argsCount` | `fn argsCount() i64` | Number of args |
| `arg` | `fn arg(n: i64) string` | Get argument n |
| `argLen` | `fn argLen(n: i64) i64` | Length of argument n |
| `argPtr` | `fn argPtr(n: i64) i64` | Raw pointer to argument n |
| `environCount` | `fn environCount() i64` | Number of env vars |
| `environ` | `fn environ(n: i64) string` | Get env entry n |
| `environLen` | `fn environLen(n: i64) i64` | Length of env entry n |
| `environPtr` | `fn environPtr(n: i64) i64` | Raw pointer to env entry n |

---

## std/time

Timestamps and timers. Import: `import "std/time"`

```zig
var start = nanoTimestamp()
// ... work ...
var elapsed_ms = (nanoTimestamp() - start) / ns_per_ms

// Or use Timer
var t = startTimer()
// ... work ...
println(t.elapsed())    // nanoseconds
```

### Constants

`ns_per_ms` (1000000), `ns_per_s` (1000000000), `ms_per_s` (1000)

### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `nanoTimestamp` | `fn nanoTimestamp() i64` | Nanoseconds since epoch |
| `milliTimestamp` | `fn milliTimestamp() i64` | Milliseconds since epoch |
| `timestamp` | `fn timestamp() i64` | Seconds since epoch |
| `startTimer` | `fn startTimer() Timer` | Create timer |

### Timer Methods

| Method | Signature | Description |
|--------|-----------|-------------|
| `elapsed` | `fn elapsed(self) i64` | Nanoseconds since start |
| `reset` | `fn reset(self) void` | Reset to now |

---

## std/random

Cryptographic random numbers. Import: `import "std/random"`

```zig
var n = randomInt()              // random i64
var dice = randomRange(6) + 1    // 1-6
```

### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `fillBytes` | `fn fillBytes(buf: i64, len: i64) i64` | Fill buffer with random bytes |
| `randomInt` | `fn randomInt() i64` | Random 64-bit integer |
| `randomRange` | `fn randomRange(max: i64) i64` | Random in [0, max) |

---

## std/io

Buffered I/O. Import: `import "std/io"`

```zig
// Buffered reading (e.g., from stdin)
var reader = newBufferedReader(0)
var line = readLine(reader)

// Buffered writing
var writer = newBufferedWriter(1)
writeString(writer, "Hello\n")
writerFlush(writer)
```

### Reader Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `newBufferedReader` | `fn newBufferedReader(fd: i64) i64` | Create (4096 buffer) |
| `newBufferedReaderSize` | `fn newBufferedReaderSize(fd: i64, size: i64) i64` | Create (custom buffer) |
| `readByte` | `fn readByte(r: i64) i64` | Read byte, -1 for EOF |
| `readLine` | `fn readLine(r: i64) string` | Read line (no newline) |

### Writer Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `newBufferedWriter` | `fn newBufferedWriter(fd: i64) i64` | Create (4096 buffer) |
| `newBufferedWriterSize` | `fn newBufferedWriterSize(fd: i64, size: i64) i64` | Create (custom buffer) |
| `writeByte` | `fn writeByte(w: i64, b: i64) void` | Write byte |
| `writeString` | `fn writeString(w: i64, s: string) void` | Write string |
| `writerFlush` | `fn writerFlush(w: i64) void` | Flush to fd |

---

## std/encoding

Base64 and hex encoding. Import: `import "std/encoding"`

```zig
// Hex
var hex = hexEncode("Hello")     // "48656c6c6f"
var raw = hexDecode(hex)          // "Hello"

// Base64
var b64 = base64Encode("Hello")  // "SGVsbG8="
var orig = base64Decode(b64)      // "Hello"
```

### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `hexEncode` | `fn hexEncode(data: string) string` | Bytes to hex |
| `hexDecode` | `fn hexDecode(s: string) string` | Hex to bytes |
| `base64Encode` | `fn base64Encode(data: string) string` | Standard Base64 |
| `base64Decode` | `fn base64Decode(encoded: string) string` | Standard Base64 decode |
| `base64UrlEncode` | `fn base64UrlEncode(data: string) string` | URL-safe Base64 |
| `base64UrlDecode` | `fn base64UrlDecode(encoded: string) string` | URL-safe decode |

---

## std/url

URL parsing. Import: `import "std/url"`

```zig
var u = parseUrl("https://example.com:8080/path?q=1#top")
println(urlScheme(u))     // https
println(urlHost(u))       // example.com
println(urlPort(u))       // 8080
println(urlPath(u))       // /path
println(urlQuery(u))      // q=1
println(urlFragment(u))   // top
```

### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `parseUrl` | `fn parseUrl(raw: string) i64` | Parse URL string |
| `urlToString` | `fn urlToString(url: i64) string` | Reconstruct URL |
| `urlScheme` | `fn urlScheme(url: i64) string` | Get scheme |
| `urlHost` | `fn urlHost(url: i64) string` | Get host |
| `urlPort` | `fn urlPort(url: i64) string` | Get port |
| `urlPath` | `fn urlPath(url: i64) string` | Get path |
| `urlQuery` | `fn urlQuery(url: i64) string` | Get query |
| `urlFragment` | `fn urlFragment(url: i64) string` | Get fragment |

---

## std/http

TCP sockets and HTTP. Import: `import "std/http"`

```zig
// Simple TCP server
var fd = try tcpListen(8080)
while (true) {
    var client = try acceptConnection(fd)
    var response = httpResponse(200, "Hello!")
    socketWriteString(client, response)
    socketClose(client)
}
```

### Error Type

`const NetError = error { SocketError, BindError, ListenError, ConnectError, AcceptError }`

### High-Level Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `tcpListen` | `fn tcpListen(port: i64) NetError!i64` | Create, bind, listen |
| `tcpConnect` | `fn tcpConnect(ip: i64, port: i64) NetError!i64` | Connect to server |
| `httpResponse` | `fn httpResponse(status: i64, body: string) string` | Build HTTP response |

### Socket Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `tcpSocket` | `fn tcpSocket() NetError!i64` | Create TCP socket |
| `setReuseAddr` | `fn setReuseAddr(fd: i64) void` | Set SO_REUSEADDR |
| `bindSocket` | `fn bindSocket(fd: i64, ip: i64, port: i64) NetError!i64` | Bind to address |
| `listenSocket` | `fn listenSocket(fd: i64, backlog: i64) NetError!i64` | Start listening |
| `acceptConnection` | `fn acceptConnection(fd: i64) NetError!i64` | Accept connection |
| `connectSocket` | `fn connectSocket(fd: i64, ip: i64, port: i64) NetError!i64` | Connect |
| `socketRead` | `fn socketRead(fd: i64, buf: i64, len: i64) i64` | Read from socket |
| `socketWrite` | `fn socketWrite(fd: i64, buf: i64, len: i64) i64` | Write raw bytes |
| `socketWriteString` | `fn socketWriteString(fd: i64, s: string) i64` | Write string |
| `socketClose` | `fn socketClose(fd: i64) void` | Close socket |

---

## std/sort

Sorting for List(T). Import: `import "std/sort"`

```zig
var nums: List(i64) = .{}
nums.append(3)
nums.append(1)
nums.append(2)
sort(i64)(nums)        // [1, 2, 3]
reverse(i64)(nums)     // [3, 2, 1]
```

### Functions (Generic)

| Function | Signature | Description |
|----------|-----------|-------------|
| `sort` | `fn sort(T)(list: List(T)) void` | Insertion sort (ascending) |
| `reverse` | `fn reverse(T)(list: List(T)) void` | Reverse in-place |

---

## std/async

Event loop and async I/O. Import: `import "std/async"`

```zig
var loop_fd = eventLoopCreate()
var sock = try tcpSocket()
setNonBlocking(sock)

// Async operations
var client = try await asyncAccept(loop_fd, sock)
var n = try await asyncRead(loop_fd, client, buf, 1024)
try await asyncWriteString(loop_fd, client, "OK")
```

### Error Type

`const IoError = error { ReadError, WriteError, AcceptError, ConnectError }`

### Event Loop Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `eventLoopCreate` | `fn eventLoopCreate() i64` | Create event loop (kqueue/epoll) |
| `watchRead` | `fn watchRead(loop_fd: i64, fd: i64) i64` | Watch for read events |
| `watchWrite` | `fn watchWrite(loop_fd: i64, fd: i64) i64` | Watch for write events |
| `unwatchRead` | `fn unwatchRead(loop_fd: i64, fd: i64) i64` | Stop watching reads |
| `unwatchWrite` | `fn unwatchWrite(loop_fd: i64, fd: i64) i64` | Stop watching writes |
| `eventLoopWait` | `fn eventLoopWait(loop_fd: i64, buf: i64, max: i64) i64` | Wait for events |
| `setNonBlocking` | `fn setNonBlocking(fd: i64) i64` | Set fd non-blocking |
| `eventFd` | `fn eventFd(buf: i64, index: i64) i64` | Extract fd from event |
| `isEagain` | `fn isEagain(result: i64) bool` | Check EAGAIN |

### Async I/O Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `asyncAccept` | `async fn asyncAccept(loop_fd: i64, listen_fd: i64) IoError!i64` | Accept with retry |
| `asyncRead` | `async fn asyncRead(loop_fd: i64, fd: i64, buf: i64, len: i64) IoError!i64` | Read with retry |
| `asyncWrite` | `async fn asyncWrite(loop_fd: i64, fd: i64, buf: i64, len: i64) IoError!i64` | Write with retry |
| `asyncWriteString` | `async fn asyncWriteString(loop_fd: i64, fd: i64, s: string) IoError!i64` | Write string with retry |
| `asyncConnect` | `async fn asyncConnect(loop_fd: i64, fd: i64, addr: i64, len: i64) IoError!i64` | Connect with retry |

---

## std/channel

Typed blocking channels for inter-thread communication. Import: `import "std/channel"`

```zig
var ch = Channel(i64).init(10)
ch.send(42)
var val = ch.recv()    // 42
ch.close()
```

### Channel(T) Methods

| Method | Signature | Description |
|--------|-----------|-------------|
| `init` | `static fn init(capacity: i64) Channel(T)` | Create with fixed capacity |
| `send` | `fn send(self, val: T) void` | Blocking send (waits if full) |
| `recv` | `fn recv(self) T` | Blocking receive (waits if empty) |
| `close` | `fn close(self) void` | Close channel, wake waiters |
| `isClosed` | `fn isClosed(self) bool` | Check if closed |
| `isEmpty` | `fn isEmpty(self) bool` | Check if empty |

---

## std/cli

Command-line argument parsing. Import: `import "std/cli"`

```zig
var args = parseArgs()
var port = getFlagInt(args, "port", 8080)
var verbose = hasFlag(args, "verbose")
var file = positional(args, 0)
```

### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `parseArgs` | `fn parseArgs() i64` | Parse process arguments |
| `getFlag` | `fn getFlag(args: i64, name: string, default: string) string` | Get flag value |
| `getFlagInt` | `fn getFlagInt(args: i64, name: string, default: i64) i64` | Get flag as integer |
| `hasFlag` | `fn hasFlag(args: i64, name: string) bool` | Check flag presence |
| `flagCount` | `fn flagCount(args: i64) i64` | Number of flags |
| `positional` | `fn positional(args: i64, index: i64) string` | Get positional arg |
| `positionalCount` | `fn positionalCount(args: i64) i64` | Number of positionals |

Supports `--name=value`, `--flag`, `-f`, and `--` delimiter.

---

## std/crypto

SHA-256 and HMAC-SHA256 hashing. Import: `import "std/crypto"`

```zig
var hash = sha256("hello")
var mac = hmacSha256("secret", "message")
```

### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `sha256` | `fn sha256(input: string) string` | SHA-256 hash (64-char hex) |
| `hmacSha256` | `fn hmacSha256(key: string, message: string) string` | HMAC-SHA256 (64-char hex) |

---

## std/debug

Debug assertions and stderr output. Import: `import "std/debug"`

### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `assert` | `fn assert(cond: bool, msg: string) void` | Trap if false |
| `assertEq` | `fn assertEq(a: i64, b: i64, msg: string) void` | Trap if not equal |
| `print` | `fn print(msg: string) void` | Write to stderr |
| `println` | `fn println(msg: string) void` | Write to stderr with newline |

---

## std/dotenv

Parse .env files. Import: `import "std/dotenv"`

```zig
var env = parseEnv(readFile(".env"))
var db = get(env, "DATABASE_URL")
```

### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `parseEnv` | `fn parseEnv(content: string) i64` | Parse .env format |
| `get` | `fn get(env: i64, key: string) string` | Look up value by key |
| `has` | `fn has(env: i64, key: string) bool` | Check if key exists |
| `entryCount` | `fn entryCount(env: i64) i64` | Number of entries |
| `entryKey` | `fn entryKey(env: i64, n: i64) string` | Key of nth entry |
| `entryValue` | `fn entryValue(env: i64, n: i64) string` | Value of nth entry |

Supports quotes, comments (#), whitespace trimming.

---

## std/fmt

String formatting, ANSI colors, and display helpers. Import: `import "std/fmt"`

```zig
println(red("Error: ") ++ bold("failed"))
println(formatBytes(1048576))        // 1.0 MB
println(formatDuration(1500000))     // 1.5ms
```

### Color Functions

`red()`, `green()`, `yellow()`, `blue()`, `magenta()`, `cyan()`, `gray()`, `white()`

### Style Functions

`bold()`, `dim()`, `italic()`, `underline()`, `strikethrough()`, `stripAnsi()`

### Format Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `formatBytes` | `fn formatBytes(bytes: i64) string` | Human-readable bytes |
| `formatDuration` | `fn formatDuration(ns: i64) string` | Human-readable duration |
| `padLeft` | `fn padLeft(s: string, width: i64, pad: i64) string` | Left-pad |
| `padRight` | `fn padRight(s: string, width: i64, pad: i64) string` | Right-pad |
| `center` | `fn center(s: string, width: i64, pad: i64) string` | Center-pad |
| `zeroPad` | `fn zeroPad(n: i64, width: i64) string` | Zero-padded integer |
| `hex` | `fn hex(n: i64) string` | Integer to hex |
| `binary` | `fn binary(n: i64) string` | Integer to binary |
| `octal` | `fn octal(n: i64) string` | Integer to octal |
| `sprintf` | `fn sprintf(template: string, args: i64, args_len: i64) string` | Format with `{}`, `{s}`, `{d}`, `{x}` |

---

## std/log

Structured logging with levels and timestamps. Import: `import "std/log"`

```zig
setLevel(LOG_DEBUG)
info("server started")
infoKv("listening", "port", "8080")
```

### Constants

`LOG_DEBUG`, `LOG_INFO` (default), `LOG_WARN`, `LOG_ERROR`, `LOG_NONE`

### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `setLevel` | `fn setLevel(level: i64) void` | Set minimum level |
| `setTimestamps` | `fn setTimestamps(enabled: bool) void` | Toggle timestamps |
| `debug` | `fn debug(msg: string) void` | Log at DEBUG |
| `info` | `fn info(msg: string) void` | Log at INFO |
| `warn` | `fn warn(msg: string) void` | Log at WARN |
| `logError` | `fn logError(msg: string) void` | Log at ERROR |
| `debugKv` | `fn debugKv(msg: string, key: string, value: string) void` | DEBUG with key=value |
| `infoKv` | `fn infoKv(msg: string, key: string, value: string) void` | INFO with key=value |
| `warnKv` | `fn warnKv(msg: string, key: string, value: string) void` | WARN with key=value |
| `logErrorKv` | `fn logErrorKv(msg: string, key: string, value: string) void` | ERROR with key=value |

---

## std/mem

Low-level byte memory operations. Import: `import "std/mem"`

### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `eql` | `fn eql(a: i64, a_len: i64, b: i64, b_len: i64) bool` | Byte equality |
| `cmp` | `fn cmp(a: i64, a_len: i64, b: i64, b_len: i64) i64` | Lexicographic compare |
| `indexOfScalar` | `fn indexOfScalar(ptr: i64, len: i64, b: i64) i64` | Find byte, or -1 |
| `lastIndexOfScalar` | `fn lastIndexOfScalar(ptr: i64, len: i64, b: i64) i64` | Find last byte |
| `startsWith` | `fn startsWith(ptr: i64, len: i64, pfx: i64, pfx_len: i64) bool` | Prefix check |
| `endsWith` | `fn endsWith(ptr: i64, len: i64, sfx: i64, sfx_len: i64) bool` | Suffix check |
| `zero` | `fn zero(ptr: i64, len: i64) void` | Zero all bytes |
| `set` | `fn set(ptr: i64, len: i64, value: i64) void` | Fill with value |
| `readU16LE` | `fn readU16LE(ptr: i64) i64` | Read little-endian u16 |
| `readU32LE` | `fn readU32LE(ptr: i64) i64` | Read little-endian u32 |
| `writeU16LE` | `fn writeU16LE(ptr: i64, val: i64) void` | Write little-endian u16 |
| `writeU32LE` | `fn writeU32LE(ptr: i64, val: i64) void` | Write little-endian u32 |

---

## std/path

POSIX path manipulation (pure string ops). Import: `import "std/path"`

```zig
println(basename("/usr/bin/cot"))    // cot
println(dirname("/usr/bin/cot"))     // /usr/bin
println(extname("app.cot"))          // .cot
println(join("src", "main.cot"))     // src/main.cot
```

### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `basename` | `fn basename(p: string) string` | Last path element |
| `dirname` | `fn dirname(p: string) string` | Parent directory |
| `extname` | `fn extname(p: string) string` | Extension with dot |
| `isAbsolute` | `fn isAbsolute(p: string) bool` | Starts with `/` |
| `join` | `fn join(a: string, b: string) string` | Join and clean |
| `join3` | `fn join3(a: string, b: string, c: string) string` | Join 3 segments |
| `join4` | `fn join4(a: string, b: string, c: string, d: string) string` | Join 4 segments |
| `clean` | `fn clean(p: string) string` | Canonicalize path |
| `relative` | `fn relative(base: string, target: string) string` | Relative path |

---

## std/process

Subprocess spawning and management. Import: `import "std/process"`

```zig
var code = run("ls", "-la")
var out = output("git", "status")
```

### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `run0` | `fn run0(cmd: string) i64` | Run, no args |
| `run` | `fn run(cmd: string, arg1: string) i64` | Run with one arg |
| `run2` | `fn run2(cmd: string, a1: string, a2: string) i64` | Run with two args |
| `runWithArgs` | `fn runWithArgs(cmd: string, args: *List(string)) i64` | Run with arg list |
| `output0` | `fn output0(cmd: string) string` | Capture stdout, no args |
| `output` | `fn output(cmd: string, arg1: string) string` | Capture stdout |
| `outputWithArgs` | `fn outputWithArgs(cmd: string, args: *List(string)) string` | Capture with args |

Returns exit code. Uses fork/execve/waitpid.

---

## std/regex

Thompson NFA regex engine (linear-time). Import: `import "std/regex"`

```zig
var found = regexMatch("[0-9]+", "abc123")    // true
var pos = regexFind("\\d+", "abc123")         // 3
var result = regexReplace("world", "hello world", "Cot")
```

### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `regexMatch` | `fn regexMatch(pattern: string, text: string) bool` | Match anywhere |
| `regexFind` | `fn regexFind(pattern: string, text: string) i64` | First match position |
| `regexFindSpan` | `fn regexFindSpan(pattern: string, text: string) i64` | Match with length |
| `regexReplace` | `fn regexReplace(pattern: string, text: string, replacement: string) string` | Replace first |
| `regexReplaceAll` | `fn regexReplaceAll(pattern: string, text: string, replacement: string) string` | Replace all |
| `regexSplit` | `fn regexSplit(pattern: string, text: string) i64` | Split by pattern |

Supports: `.`, `*`, `+`, `?`, `|`, `()`, `[]`, `[^]`, `[a-z]`, `^`, `$`, `\d`, `\w`, `\s`.

---

## std/semver

Semantic Versioning 2.0.0 parsing. Import: `import "std/semver"`

```zig
var v = parse("1.2.3-beta+build")
println(major(v))    // 1
println(cmp(parse("1.0.0"), parse("2.0.0")))    // -1
```

### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `parse` | `fn parse(s: string) i64` | Parse version string |
| `major` | `fn major(v: i64) i64` | Major version |
| `minor` | `fn minor(v: i64) i64` | Minor version |
| `patch` | `fn patch(v: i64) i64` | Patch version |
| `prerelease` | `fn prerelease(v: i64) string` | Prerelease label |
| `build` | `fn build(v: i64) string` | Build metadata |
| `cmp` | `fn cmp(a: i64, b: i64) i64` | Compare (-1/0/1) |
| `gt`, `gte`, `lt`, `lte`, `eq` | comparison functions | Version comparisons |
| `format` | `fn format(v: i64) string` | Format to string |
| `incMajor`, `incMinor`, `incPatch` | increment functions | Bump version |

---

## std/string_map

Hash map from string keys to i64 values. Import: `import "std/string_map"`

### StringMap Methods

| Method | Signature | Description |
|--------|-----------|-------------|
| `set` | `fn set(self, key: string, value: i64) void` | Insert or update |
| `get` | `fn get(self, key: string) i64` | Get value (traps on miss) |
| `getOrDefault` | `fn getOrDefault(self, key: string, default: i64) i64` | Get with fallback |
| `has` | `fn has(self, key: string) bool` | Check existence |
| `delete` | `fn delete(self, key: string) void` | Remove entry |
| `len` | `fn len(self) i64` | Entry count |
| `isEmpty` | `fn isEmpty(self) bool` | Check empty |
| `clear` | `fn clear(self) void` | Remove all entries |
| `free` | `fn free(self) void` | Deallocate |

---

## std/sqlite

SQLite3 FFI bindings. Import: `import "std/sqlite"`

Requires `"libs": ["sqlite3"]` in `cot.json`.

```zig
var db = sqliteOpen(":memory:")
sqliteExec(db, "CREATE TABLE t (id INTEGER, name TEXT)")

var stmt = sqlitePrepare(db, "INSERT INTO t VALUES (?1, ?2)")
bindInt(stmt, 1, 42)
bindText(stmt, 2, "hello")
sqlite3_step(stmt)
sqlite3_finalize(stmt)

var q = sqlitePrepare(db, "SELECT name FROM t WHERE id = ?1")
bindInt(q, 1, 42)
if (sqlite3_step(q) == SQLITE_ROW) {
    println(columnString(q, 0))    // hello
}
sqlite3_finalize(q)
sqliteClose(db)
```

### Constants

`SQLITE_OK` (0), `SQLITE_ERROR` (1), `SQLITE_ROW` (100), `SQLITE_DONE` (101), `SQLITE_TRANSIENT` (-1)

### High-Level Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `sqliteOpen` | `fn sqliteOpen(path: string) i64` | Open database (0 on failure) |
| `sqliteClose` | `fn sqliteClose(db: i64) void` | Close database |
| `sqliteExec` | `fn sqliteExec(db: i64, sql: string) i64` | Execute SQL (no results) |
| `sqlitePrepare` | `fn sqlitePrepare(db: i64, sql: string) i64` | Prepare statement |
| `bindText` | `fn bindText(stmt: i64, idx: i64, value: string) i64` | Bind string (1-indexed) |
| `bindInt` | `fn bindInt(stmt: i64, idx: i64, value: i64) i64` | Bind integer (1-indexed) |
| `columnString` | `fn columnString(stmt: i64, col: i64) string` | Read text column (0-indexed) |
| `cstr` | `fn cstr(s: string) i64` | Convert to null-terminated C string |
| `freeCstr` | `fn freeCstr(ptr: i64) void` | Free C string from cstr() |

### Low-Level Extern Functions

`sqlite3_open`, `sqlite3_close`, `sqlite3_exec`, `sqlite3_prepare_v2`, `sqlite3_step`, `sqlite3_finalize`, `sqlite3_reset`, `sqlite3_bind_text`, `sqlite3_bind_int64`, `sqlite3_bind_null`, `sqlite3_column_text`, `sqlite3_column_bytes`, `sqlite3_column_int64`, `sqlite3_column_count`, `sqlite3_errmsg`, `sqlite3_changes`

---

## std/sys

Low-level runtime function declarations. Import: `import "std/sys"`

Internal bindings to the Cot runtime. Prefer higher-level modules (fs, os, time, etc.) instead.

Categories: Memory (`alloc`, `dealloc`, `realloc`, `memcpy`), String (`string_concat`, `string_eq`), Print (`write`, `print_int`), Slice (`growslice`), ARC (`retain`, `release`), File I/O, Directory I/O, Process, Time, Networking, Event loops, Threading, Mutex, Conditions.

---

## std/testing

Enhanced test assertions. Import: `import "std/testing"`

### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `assertContains` | `fn assertContains(haystack: string, needle: string) void` | Substring check |
| `assertStartsWith` | `fn assertStartsWith(s: string, prefix: string) void` | Prefix check |
| `assertEndsWith` | `fn assertEndsWith(s: string, suffix: string) void` | Suffix check |
| `assertStrEq` | `fn assertStrEq(actual: string, expected: string, label: string) void` | String equality |
| `assertEmpty` | `fn assertEmpty(s: string) void` | Check empty |
| `assertNotEmpty` | `fn assertNotEmpty(s: string) void` | Check not empty |
| `assertLen` | `fn assertLen(s: string, expected: i64) void` | String length |
| `assertGt` | `fn assertGt(a: i64, b: i64) void` | a > b |
| `assertGte` | `fn assertGte(a: i64, b: i64) void` | a >= b |
| `assertLt` | `fn assertLt(a: i64, b: i64) void` | a < b |
| `assertLte` | `fn assertLte(a: i64, b: i64) void` | a <= b |
| `assertInRange` | `fn assertInRange(actual: i64, min: i64, max: i64) void` | Range check |
| `assertTrue` | `fn assertTrue(cond: bool, msg: string) void` | Condition true |
| `assertFalse` | `fn assertFalse(cond: bool, msg: string) void` | Condition false |

---

## std/thread

OS-level threading (pthreads). Import: `import "std/thread"`

### Thread Methods

| Method | Signature | Description |
|--------|-----------|-------------|
| `spawn` | `fn spawn(fn_ptr: i64, arg: i64) Thread` | Create thread |
| `join` | `fn join(self) void` | Wait for completion |
| `detach` | `fn detach(self) void` | Detach thread |

### Mutex Methods

| Method | Signature | Description |
|--------|-----------|-------------|
| `init` | `static fn init() Mutex` | Create mutex |
| `lock` | `fn lock(self) void` | Acquire lock |
| `unlock` | `fn unlock(self) void` | Release lock |
| `tryLock` | `fn tryLock(self) bool` | Non-blocking try |
| `destroy` | `fn destroy(self) void` | Destroy mutex |

### Condition Methods

| Method | Signature | Description |
|--------|-----------|-------------|
| `init` | `static fn init() Condition` | Create condition |
| `wait` | `fn wait(self, mutex: *Mutex) void` | Wait on condition |
| `signal` | `fn signal(self) void` | Wake one waiter |
| `broadcast` | `fn broadcast(self) void` | Wake all waiters |
| `destroy` | `fn destroy(self) void` | Destroy condition |

---

## std/uuid

UUID v4 generation (RFC 4122). Import: `import "std/uuid"`

```zig
var id = v4()    // "550e8400-e29b-41d4-a716-446655440000"
@assert(isValid(id))
```

### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `v4` | `fn v4() string` | Generate random UUID v4 |
| `isValid` | `fn isValid(s: string) bool` | Validate UUID v4 format |
| `version` | `fn version(s: string) i64` | Extract version number |
