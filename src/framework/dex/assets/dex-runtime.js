/**
 * Dex Runtime - Client-side WebSocket and DOM handling
 *
 * Manages real-time communication with the Dex server:
 * - WebSocket connection and reconnection
 * - Event binding and dispatch
 * - DOM patching with morphdom
 * - Presence tracking
 */

(function(window) {
  'use strict';

  // Configuration
  const DEFAULT_RECONNECT_DELAY = 1000;
  const MAX_RECONNECT_DELAY = 30000;
  const HEARTBEAT_INTERVAL = 30000;

  /**
   * Dex Socket - manages WebSocket connection
   */
  class DexSocket {
    constructor(url, options = {}) {
      this.url = url;
      this.options = options;
      this.socket = null;
      this.messageId = 0;
      this.pendingCallbacks = new Map();
      this.reconnectDelay = DEFAULT_RECONNECT_DELAY;
      this.reconnectTimer = null;
      this.heartbeatTimer = null;
      this.isConnecting = false;
      this.isConnected = false;

      // Event handlers
      this.onOpen = options.onOpen || (() => {});
      this.onClose = options.onClose || (() => {});
      this.onError = options.onError || (() => {});
      this.onMessage = options.onMessage || (() => {});
    }

    connect() {
      if (this.isConnecting || this.isConnected) return;

      this.isConnecting = true;

      try {
        this.socket = new WebSocket(this.url);

        this.socket.onopen = () => {
          this.isConnecting = false;
          this.isConnected = true;
          this.reconnectDelay = DEFAULT_RECONNECT_DELAY;
          this.startHeartbeat();
          this.onOpen();
        };

        this.socket.onclose = (event) => {
          this.isConnecting = false;
          this.isConnected = false;
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
      } catch (error) {
        this.isConnecting = false;
        this.onError(error);
        this.scheduleReconnect();
      }
    }

    disconnect() {
      this.stopHeartbeat();
      clearTimeout(this.reconnectTimer);

      if (this.socket) {
        this.socket.close();
        this.socket = null;
      }

      this.isConnected = false;
      this.isConnecting = false;
    }

    send(type, payload, callback) {
      if (!this.isConnected) {
        console.warn('DexSocket: Cannot send, not connected');
        return;
      }

      const id = ++this.messageId;
      const message = JSON.stringify({
        id: id,
        type: type,
        ...payload
      });

      if (callback) {
        this.pendingCallbacks.set(id, callback);
      }

      this.socket.send(message);
      return id;
    }

    handleMessage(data) {
      try {
        const message = JSON.parse(data);

        // Check for callback
        if (message.ref && this.pendingCallbacks.has(message.ref)) {
          const callback = this.pendingCallbacks.get(message.ref);
          this.pendingCallbacks.delete(message.ref);
          callback(message);
        }

        // General message handler
        this.onMessage(message);
      } catch (error) {
        console.error('DexSocket: Failed to parse message', error);
      }
    }

    scheduleReconnect() {
      if (this.reconnectTimer) return;

      this.reconnectTimer = setTimeout(() => {
        this.reconnectTimer = null;
        this.connect();

        // Exponential backoff
        this.reconnectDelay = Math.min(
          this.reconnectDelay * 2,
          MAX_RECONNECT_DELAY
        );
      }, this.reconnectDelay);
    }

    startHeartbeat() {
      this.heartbeatTimer = setInterval(() => {
        this.send('ping', {});
      }, HEARTBEAT_INTERVAL);
    }

    stopHeartbeat() {
      if (this.heartbeatTimer) {
        clearInterval(this.heartbeatTimer);
        this.heartbeatTimer = null;
      }
    }
  }

  /**
   * Dex View - manages a live component instance
   */
  class DexView {
    constructor(dex, element) {
      this.dex = dex;
      this.element = element;
      this.componentId = element.dataset.dexId;
      this.joined = false;
      this.eventBindings = [];

      this.bindEvents();
    }

    join() {
      if (this.joined) return;

      this.dex.socket.send('join', {
        component_id: this.componentId
      }, (response) => {
        if (response.type === 'joined') {
          this.joined = true;
          if (response.html) {
            this.patch(response.html);
          }
        } else if (response.type === 'error') {
          console.error('DexView: Join failed', response.message);
        }
      });
    }

    leave() {
      if (!this.joined) return;

      this.dex.socket.send('leave', {
        component_id: this.componentId
      });

      this.joined = false;
      this.unbindEvents();
    }

    pushEvent(event, payload = {}) {
      this.dex.socket.send('event', {
        component_id: this.componentId,
        event: event,
        ...payload
      }, (response) => {
        if (response.patches) {
          this.applyPatches(response.patches);
        } else if (response.html) {
          this.patch(response.html);
        }
      });
    }

    patch(html) {
      if (typeof morphdom !== 'undefined') {
        morphdom(this.element, html, {
          onBeforeElUpdated: (fromEl, toEl) => {
            // Preserve focus
            if (fromEl === document.activeElement) {
              return false;
            }
            return true;
          },
          childrenOnly: true
        });
      } else {
        // Fallback if morphdom isn't loaded
        this.element.innerHTML = html;
      }

      // Rebind events after DOM update
      this.bindEvents();
    }

    applyPatches(patches) {
      for (const patch of patches) {
        const target = document.querySelector(`[data-dex-id="${patch.target}"]`);
        if (!target) continue;

        switch (patch.op) {
          case 'replace':
            if (typeof morphdom !== 'undefined') {
              morphdom(target, patch.html);
            } else {
              target.outerHTML = patch.html;
            }
            break;
          case 'append':
            target.insertAdjacentHTML('beforeend', patch.html);
            break;
          case 'prepend':
            target.insertAdjacentHTML('afterbegin', patch.html);
            break;
          case 'remove':
            target.remove();
            break;
          case 'attr':
            target.setAttribute(patch.name, patch.value);
            break;
        }
      }

      this.bindEvents();
    }

    bindEvents() {
      // Unbind existing
      this.unbindEvents();

      // Find all dex event elements
      const eventElements = this.element.querySelectorAll('[dex-click], [dex-submit], [dex-change], [dex-input]');

      eventElements.forEach(el => {
        // Click events
        if (el.hasAttribute('dex-click')) {
          const handler = (e) => {
            e.preventDefault();
            const event = el.getAttribute('dex-click');
            const value = el.dataset.dexValue || el.value;
            this.pushEvent(event, { value: value });
          };
          el.addEventListener('click', handler);
          this.eventBindings.push({ el, type: 'click', handler });
        }

        // Submit events
        if (el.hasAttribute('dex-submit')) {
          const handler = (e) => {
            e.preventDefault();
            const event = el.getAttribute('dex-submit');
            const formData = new FormData(el);
            const data = Object.fromEntries(formData.entries());
            this.pushEvent(event, { form_data: data });
          };
          el.addEventListener('submit', handler);
          this.eventBindings.push({ el, type: 'submit', handler });
        }

        // Change events
        if (el.hasAttribute('dex-change')) {
          const handler = (e) => {
            const event = el.getAttribute('dex-change');
            this.pushEvent(event, { value: el.value });
          };
          el.addEventListener('change', handler);
          this.eventBindings.push({ el, type: 'change', handler });
        }

        // Input events (real-time)
        if (el.hasAttribute('dex-input')) {
          const debounceMs = parseInt(el.dataset.dexDebounce) || 300;
          let timeout;
          const handler = (e) => {
            clearTimeout(timeout);
            timeout = setTimeout(() => {
              const event = el.getAttribute('dex-input');
              this.pushEvent(event, { value: el.value });
            }, debounceMs);
          };
          el.addEventListener('input', handler);
          this.eventBindings.push({ el, type: 'input', handler });
        }
      });
    }

    unbindEvents() {
      this.eventBindings.forEach(({ el, type, handler }) => {
        el.removeEventListener(type, handler);
      });
      this.eventBindings = [];
    }
  }

  /**
   * Dex Presence - tracks who is online
   */
  class DexPresence {
    constructor(dex) {
      this.dex = dex;
      this.state = {};
      this.onJoin = () => {};
      this.onLeave = () => {};
      this.onSync = () => {};
    }

    track(topic, meta = {}) {
      this.dex.socket.send('presence:track', {
        topic: topic,
        meta: meta
      });
    }

    untrack(topic) {
      this.dex.socket.send('presence:untrack', {
        topic: topic
      });
    }

    list(topic) {
      return this.state[topic] || [];
    }

    handleDiff(message) {
      const topic = message.topic;

      if (!this.state[topic]) {
        this.state[topic] = [];
      }

      // Process joins
      if (message.joins) {
        for (const join of message.joins) {
          const existing = this.state[topic].find(p => p.key === join.key);
          if (!existing) {
            this.state[topic].push(join);
            this.onJoin(topic, join);
          }
        }
      }

      // Process leaves
      if (message.leaves) {
        for (const leave of message.leaves) {
          const index = this.state[topic].findIndex(p => p.key === leave.key);
          if (index !== -1) {
            const [removed] = this.state[topic].splice(index, 1);
            this.onLeave(topic, removed);
          }
        }
      }

      this.onSync(topic, this.state[topic]);
    }
  }

  /**
   * Main Dex class - entry point
   */
  class Dex {
    constructor() {
      this.socket = null;
      this.views = new Map();
      this.presence = null;
    }

    connect(wsUrl) {
      // Get WebSocket URL from meta tag if not provided
      if (!wsUrl) {
        const meta = document.querySelector('meta[name="dex-ws-url"]');
        if (meta) {
          const path = meta.content;
          const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
          wsUrl = `${protocol}//${window.location.host}${path}`;
        } else {
          console.error('Dex: No WebSocket URL provided');
          return;
        }
      }

      this.socket = new DexSocket(wsUrl, {
        onOpen: () => {
          console.log('Dex: Connected');
          this.joinViews();
        },
        onClose: () => {
          console.log('Dex: Disconnected');
        },
        onError: (error) => {
          console.error('Dex: Error', error);
        },
        onMessage: (message) => {
          this.handleMessage(message);
        }
      });

      this.presence = new DexPresence(this);

      this.socket.connect();
    }

    disconnect() {
      // Leave all views
      this.views.forEach(view => view.leave());
      this.views.clear();

      if (this.socket) {
        this.socket.disconnect();
        this.socket = null;
      }
    }

    mount(element) {
      if (!element) {
        element = document.querySelector('[data-dex-id]');
      }

      if (!element) {
        console.warn('Dex: No dex root element found');
        return null;
      }

      const view = new DexView(this, element);
      this.views.set(element, view);

      if (this.socket && this.socket.isConnected) {
        view.join();
      }

      return view;
    }

    unmount(element) {
      const view = this.views.get(element);
      if (view) {
        view.leave();
        this.views.delete(element);
      }
    }

    joinViews() {
      this.views.forEach(view => {
        if (!view.joined) {
          view.join();
        }
      });
    }

    handleMessage(message) {
      switch (message.type) {
        case 'patch':
          this.handlePatch(message);
          break;
        case 'presence_diff':
          this.presence.handleDiff(message);
          break;
        case 'pong':
          // Heartbeat response
          break;
        default:
          console.log('Dex: Unknown message type', message.type);
      }
    }

    handlePatch(message) {
      // Find view by component ID
      for (const view of this.views.values()) {
        if (view.componentId === String(message.component_id)) {
          view.patch(message.html);
          break;
        }
      }
    }

    // Static factory
    static init() {
      const dex = new Dex();

      // Auto-connect on DOM ready
      if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', () => {
          dex.connect();
          dex.mount();
        });
      } else {
        dex.connect();
        dex.mount();
      }

      return dex;
    }
  }

  // Export to window
  window.Dex = Dex;
  window.DexSocket = DexSocket;
  window.DexView = DexView;
  window.DexPresence = DexPresence;

  // Auto-initialize if data attribute is present
  if (document.querySelector('[data-dex-id]')) {
    window.dex = Dex.init();
  }

})(window);
