#!/bin/sh
set -e

# Cot installer — downloads pre-built binary from GitHub Releases.
# Usage: curl -fsSL https://raw.githubusercontent.com/cotlang/cot/main/install.sh | sh
#
# Environment variables:
#   COT_INSTALL_DIR  Override install location (default: ~/.cot)
#   COT_VERSION      Pin to specific version (default: latest)

COT_DIR="${COT_INSTALL_DIR:-$HOME/.cot}"
BIN_DIR="$COT_DIR/bin"

OS=$(uname -s)
ARCH=$(uname -m)

case "$OS" in
    Darwin) OS_NAME="macos" ;;
    Linux)  OS_NAME="linux" ;;
    *)      echo "Error: unsupported OS: $OS"; exit 1 ;;
esac

case "$ARCH" in
    arm64|aarch64) ARCH_NAME="aarch64" ;;
    x86_64|amd64)  ARCH_NAME="x86_64" ;;
    *)              echo "Error: unsupported architecture: $ARCH"; exit 1 ;;
esac

# Fetch latest version from GitHub API if not pinned
if [ -z "${COT_VERSION:-}" ]; then
    COT_VERSION=$(curl -fsSL https://api.github.com/repos/cotlang/cot/releases/latest \
        | grep '"tag_name"' | sed 's/.*"v\(.*\)".*/\1/')
    if [ -z "$COT_VERSION" ]; then
        echo "Error: could not determine latest version"
        exit 1
    fi
fi

FILENAME="cot-${ARCH_NAME}-${OS_NAME}.tar.gz"
URL="https://github.com/cotlang/cot/releases/download/v${COT_VERSION}/${FILENAME}"

echo "Installing cot ${COT_VERSION} (${ARCH_NAME}-${OS_NAME})..."

mkdir -p "$COT_DIR"
curl -fsSL "$URL" | tar -xz -C "$COT_DIR"
chmod +x "$BIN_DIR/cot"

echo "Cot ${COT_VERSION} installed to ${BIN_DIR}/cot"
echo ""

# Check if BIN_DIR is already in PATH
case ":$PATH:" in
    *":$BIN_DIR:"*) ;;
    *)
        SHELL_NAME=$(basename "$SHELL")
        case "$SHELL_NAME" in
            zsh)  RC_FILE="$HOME/.zshrc" ;;
            bash) RC_FILE="$HOME/.bashrc" ;;
            *)    RC_FILE="your shell profile" ;;
        esac
        echo "Add to $RC_FILE:"
        echo ""
        echo "  export PATH=\"${BIN_DIR}:\$PATH\""
        echo ""
        ;;
esac
