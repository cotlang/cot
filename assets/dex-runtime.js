/**
 * Dex Runtime - Client-side JavaScript for Dex framework
 *
 * Minimal runtime (~5KB) for server-rendered real-time UI:
 * - WebSocket connection management
 * - Event capture and delegation
 * - DOM patching with morphdom
 * - Focus/scroll preservation
 *
 * Named after Dex, a loyal 14-year-old husky/cattle dog mix.
 */

(function() {
  'use strict';

  // ============================================================================
  // Configuration
  // ============================================================================

  const DEX_PREFIX = 'data-dex-';
  const EVENTS = ['click', 'input', 'change', 'submit', 'blur', 'focus', 'keydown', 'keyup'];

  // ============================================================================
  // Utility Functions
  // ============================================================================

  function getEventHandler(el, eventType) {
    return el.getAttribute(DEX_PREFIX + eventType);
  }

  function getComponentId(el) {
    return el.closest('[data-dex-id]')?.getAttribute('data-dex-id');
  }

  function serializeForm(form) {
    const data = {};
    new FormData(form).forEach((value, key) => {
      data[key] = value;
    });
    return data;
  }

  function getInputValue(el) {
    if (el.type === 'checkbox') return el.checked;
    if (el.type === 'radio') return el.checked ? el.value : null;
    if (el.tagName === 'SELECT' && el.multiple) {
      return Array.from(el.selectedOptions).map(o => o.value);
    }
    return el.value;
  }

  // ============================================================================
  // Focus/Selection Preservation
  // ============================================================================

  function saveFocus() {
    const active = document.activeElement;
    if (!active || active === document.body) return null;

    return {
      id: active.id,
      name: active.name,
      tagName: active.tagName,
      selectionStart: active.selectionStart,
      selectionEnd: active.selectionEnd,
      scrollTop: active.scrollTop,
      scrollLeft: active.scrollLeft
    };
  }

  function restoreFocus(saved) {
    if (!saved) return;

    let el = null;
    if (saved.id) {
      el = document.getElementById(saved.id);
    } else if (saved.name) {
      el = document.querySelector(`[name="${saved.name}"]`);
    }

    if (!el) return;

    el.focus();

    if (typeof saved.selectionStart === 'number' && el.setSelectionRange) {
      try {
        el.setSelectionRange(saved.selectionStart, saved.selectionEnd);
      } catch (e) {
        // Some input types don't support selection
      }
    }

    if (typeof saved.scrollTop === 'number') {
      el.scrollTop = saved.scrollTop;
      el.scrollLeft = saved.scrollLeft;
    }
  }

  // ============================================================================
  // DOM Patching (using morphdom when available, fallback to innerHTML)
  // ============================================================================

  function patchDOM(container, newHtml) {
    const focusState = saveFocus();

    if (typeof morphdom !== 'undefined') {
      // Create temporary container to parse new HTML
      const temp = document.createElement('div');
      temp.innerHTML = newHtml;

      // Use morphdom for efficient diffing
      morphdom(container, temp, {
        childrenOnly: true,
        onBeforeElUpdated: function(fromEl, toEl) {
          // Preserve focus on active input
          if (fromEl === document.activeElement) {
            return false;
          }
          return true;
        }
      });
    } else {
      // Fallback: simple innerHTML replacement
      container.innerHTML = newHtml;
    }

    restoreFocus(focusState);
  }

  // ============================================================================
  // Dex Socket - WebSocket Connection Manager
  // ============================================================================

  class DexSocket {
    constructor(url, opts = {}) {
      this.url = url;
      this.opts = opts;
      this.socket = null;
      this.connected = false;
      this.reconnectTimer = null;
      this.reconnectAttempts = 0;
      this.maxReconnectAttempts = opts.maxReconnects || 10;
      this.heartbeatInterval = opts.heartbeatInterval || 30000;
      this.heartbeatTimer = null;
      this.pendingCallbacks = new Map();
      this.messageId = 0;
      this.views = new Map();
    }

    connect() {
      if (this.socket) return;

      this.socket = new WebSocket(this.url);

      this.socket.onopen = () => {
        this.connected = true;
        this.reconnectAttempts = 0;
        this.startHeartbeat();
        this.onOpen();
      };

      this.socket.onclose = (event) => {
        this.connected = false;
        this.stopHeartbeat();
        this.onClose(event);
        this.scheduleReconnect();
      };

      this.socket.onerror = (error) => {
        this.onError(error);
      };

      this.socket.onmessage = (event) => {
        this.handleMessage(event.data);
      };
    }

    disconnect() {
      this.stopHeartbeat();
      if (this.reconnectTimer) {
        clearTimeout(this.reconnectTimer);
        this.reconnectTimer = null;
      }
      if (this.socket) {
        this.socket.close();
        this.socket = null;
      }
      this.connected = false;
    }

    send(type, payload, callback) {
      if (!this.connected) {
        console.warn('[Dex] Not connected, cannot send:', type);
        return;
      }

      const id = ++this.messageId;
      const message = JSON.stringify({ id, type, payload });

      if (callback) {
        this.pendingCallbacks.set(id, callback);
      }

      this.socket.send(message);
    }

    handleMessage(data) {
      try {
        const msg = JSON.parse(data);

        // Handle response to pending request
        if (msg.ref && this.pendingCallbacks.has(msg.ref)) {
          const callback = this.pendingCallbacks.get(msg.ref);
          this.pendingCallbacks.delete(msg.ref);
          callback(msg);
          return;
        }

        // Handle server-initiated messages
        switch (msg.type) {
          case 'patch':
            this.handlePatch(msg);
            break;
          case 'redirect':
            window.location.href = msg.url;
            break;
          case 'pong':
            // Heartbeat response
            break;
          default:
            this.onMessage(msg);
        }
      } catch (e) {
        console.error('[Dex] Failed to parse message:', e);
      }
    }

    handlePatch(msg) {
      const componentId = msg.component_id;
      const html = msg.html;

      const container = document.querySelector(`[data-dex-id="${componentId}"]`);
      if (container) {
        patchDOM(container, html);
      }
    }

    startHeartbeat() {
      this.heartbeatTimer = setInterval(() => {
        this.send('ping', {});
      }, this.heartbeatInterval);
    }

    stopHeartbeat() {
      if (this.heartbeatTimer) {
        clearInterval(this.heartbeatTimer);
        this.heartbeatTimer = null;
      }
    }

    scheduleReconnect() {
      if (this.reconnectAttempts >= this.maxReconnectAttempts) {
        console.error('[Dex] Max reconnect attempts reached');
        return;
      }

      const delay = Math.min(1000 * Math.pow(2, this.reconnectAttempts), 30000);
      this.reconnectAttempts++;

      this.reconnectTimer = setTimeout(() => {
        console.log('[Dex] Reconnecting...');
        this.socket = null;
        this.connect();
      }, delay);
    }

    // Event hooks - can be overridden
    onOpen() {}
    onClose(event) {}
    onError(error) {}
    onMessage(msg) {}
  }

  // ============================================================================
  // Dex View - Component View Manager
  // ============================================================================

  class DexView {
    constructor(socket, el) {
      this.socket = socket;
      this.el = el;
      this.id = el.getAttribute('data-dex-id');
      this.setupEventListeners();
    }

    setupEventListeners() {
      // Use event delegation on the component root
      EVENTS.forEach(eventType => {
        this.el.addEventListener(eventType, (e) => this.handleEvent(e, eventType), true);
      });
    }

    handleEvent(e, eventType) {
      const target = e.target;
      const handler = getEventHandler(target, eventType);

      if (!handler) return;

      // Prevent default for form submissions
      if (eventType === 'submit') {
        e.preventDefault();
      }

      const payload = {
        component_id: this.id,
        event: handler,
        type: eventType,
        target_id: target.id || null,
        value: getInputValue(target)
      };

      // Include form data for submit events
      if (eventType === 'submit' && target.tagName === 'FORM') {
        payload.form_data = serializeForm(target);
      }

      this.socket.send('event', payload);
    }

    destroy() {
      // Cleanup if needed
    }
  }

  // ============================================================================
  // Dex - Main Entry Point
  // ============================================================================

  class Dex {
    constructor(opts = {}) {
      this.opts = opts;
      this.socket = null;
      this.views = new Map();
    }

    connect(url) {
      this.socket = new DexSocket(url, this.opts);

      this.socket.onOpen = () => {
        console.log('[Dex] Connected');
        this.initViews();
        this.joinViews();
      };

      this.socket.onClose = () => {
        console.log('[Dex] Disconnected');
      };

      this.socket.connect();
      return this;
    }

    disconnect() {
      this.views.forEach(view => view.destroy());
      this.views.clear();
      if (this.socket) {
        this.socket.disconnect();
        this.socket = null;
      }
    }

    initViews() {
      // Find all Dex components on the page
      document.querySelectorAll('[data-dex-id]').forEach(el => {
        const id = el.getAttribute('data-dex-id');
        if (!this.views.has(id)) {
          this.views.set(id, new DexView(this.socket, el));
        }
      });
    }

    joinViews() {
      this.views.forEach((view, id) => {
        this.socket.send('join', { component_id: id });
      });
    }
  }

  // ============================================================================
  // Auto-initialization
  // ============================================================================

  function autoInit() {
    // Check for Dex components on the page
    const components = document.querySelectorAll('[data-dex-id]');
    if (components.length === 0) return;

    // Get WebSocket URL from meta tag or default
    const wsUrl = document.querySelector('meta[name="dex-ws-url"]')?.content
                  || `ws://${window.location.host}/dex/websocket`;

    // Initialize Dex
    window.dex = new Dex();
    window.dex.connect(wsUrl);
  }

  // Initialize when DOM is ready
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', autoInit);
  } else {
    autoInit();
  }

  // Export for manual initialization
  window.Dex = Dex;
  window.DexSocket = DexSocket;
  window.DexView = DexView;

})();
