# Dex WebSocket Protocol

This document describes the wire protocol used for real-time communication between the Dex client (browser) and server.

## Connection

The client connects via WebSocket to the server at the URL specified in the `<meta name="dex-ws-url">` tag, typically `/dex/websocket`.

## Message Format

All messages are JSON objects with the following common fields:

```json
{
  "id": <number>,      // Message ID for request/response correlation
  "type": "<string>"   // Message type
}
```

## Client → Server Messages

### join

Join a component to receive updates.

```json
{
  "id": 1,
  "type": "join",
  "component_id": "Counter"
}
```

**Response:**
```json
{
  "ref": 1,
  "type": "joined",
  "instance_id": 12345,
  "html": "<div>...</div>"
}
```

### event

Send a user interaction event to a component.

```json
{
  "id": 2,
  "type": "event",
  "component_id": "12345",
  "event": "increment",
  "value": "1"
}
```

**Response:**
```json
{
  "ref": 2,
  "type": "patch",
  "html": "<div>...</div>"
}
```

Or with patches:
```json
{
  "ref": 2,
  "patches": [
    {"target": "count", "op": "replace", "html": "<span>5</span>"}
  ]
}
```

### leave

Leave a component (stop receiving updates).

```json
{
  "id": 3,
  "type": "leave",
  "component_id": "12345"
}
```

**Response:**
```json
{
  "ref": 3,
  "type": "left"
}
```

### ping

Heartbeat to keep connection alive.

```json
{
  "id": 4,
  "type": "ping"
}
```

**Response:**
```json
{
  "ref": 4,
  "type": "pong"
}
```

### presence:track

Track presence in a topic (e.g., "room:lobby").

```json
{
  "id": 5,
  "type": "presence:track",
  "topic": "room:lobby",
  "key": "user_123",
  "meta": {
    "name": "Alice",
    "status": "online"
  }
}
```

**Response:**
```json
{
  "ref": 5,
  "type": "presence_tracked",
  "topic": "room:lobby"
}
```

### presence:untrack

Stop tracking presence in a topic.

```json
{
  "id": 6,
  "type": "presence:untrack",
  "topic": "room:lobby"
}
```

**Response:**
```json
{
  "ref": 6,
  "type": "presence_untracked",
  "topic": "room:lobby"
}
```

## Server → Client Messages (Push)

### patch

DOM update for a component.

```json
{
  "type": "patch",
  "component_id": 12345,
  "html": "<div>...</div>"
}
```

### presence_diff

Presence changes in a topic.

```json
{
  "type": "presence_diff",
  "topic": "room:lobby",
  "joins": [
    {"key": "user_456", "meta": {"name": "Bob"}}
  ],
  "leaves": [
    {"key": "user_123"}
  ]
}
```

## Patch Operations

When using granular patches instead of full HTML replacement:

| Operation | Description |
|-----------|-------------|
| `replace` | Replace element's entire HTML |
| `append`  | Append HTML to element's children |
| `prepend` | Prepend HTML to element's children |
| `remove`  | Remove the element |
| `attr`    | Set an attribute value |

```json
{
  "patches": [
    {"target": "item-1", "op": "replace", "html": "<li>Updated</li>"},
    {"target": "list", "op": "append", "html": "<li>New item</li>"},
    {"target": "old-item", "op": "remove"},
    {"target": "input", "op": "attr", "name": "value", "value": "hello"}
  ]
}
```

## Event Bindings

HTML elements can bind to events using `dex-*` attributes:

| Attribute | Event | Description |
|-----------|-------|-------------|
| `dex-click` | click | Fire event on click |
| `dex-submit` | submit | Fire event on form submit |
| `dex-change` | change | Fire event on input change |
| `dex-input` | input | Fire event on input (debounced) |

Example:
```html
<button dex-click="increment" data-dex-value="1">+1</button>
<form dex-submit="save_form">...</form>
<input dex-input="search" data-dex-debounce="300" />
```

## Heartbeat

The client should send `ping` messages every 30 seconds to keep the connection alive. If no `pong` response is received, the client should attempt to reconnect.

## Reconnection

On disconnection, the client should:

1. Wait with exponential backoff (1s, 2s, 4s, ... up to 30s)
2. Reconnect to the WebSocket
3. Rejoin all previously joined components
4. Re-track presence in all previously tracked topics

## Security

- All WebSocket messages are validated server-side
- Component IDs are verified to belong to the session
- Presence keys should be authenticated (user ID from session)
