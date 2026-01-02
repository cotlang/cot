#!/bin/bash
#
# Bump version across all cot source files
#

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

usage() {
    echo ""
    echo -e "${CYAN}Usage:${NC} $0 <version>"
    echo ""
    echo -e "${CYAN}Examples:${NC}"
    echo "  $0 0.1.0        # Set specific version"
    echo "  $0 0.2.0        # Bump to 0.2.0"
    echo "  $0 1.0.0        # Release 1.0.0"
    echo ""
    echo -e "${CYAN}Version format:${NC} MAJOR.MINOR.PATCH (e.g., 0.1.0, 1.2.3)"
    echo ""
    echo -e "${CYAN}Files updated:${NC}"
    echo "  - build.zig.zon              (package manifest)"
    echo "  - src/root.zig               (cot.version constant)"
    echo "  - src/dbl/root.zig           (DBL frontend version)"
    echo "  - src/cotdb/cotdb.zig        (database module version)"
    echo "  - src/tui_ext/cot_tui.zig    (TUI extension version)"
    echo "  - src/comptime/builtins.zig  (@version() builtin)"
    echo "  - src/framework/config.zig   (generated cot.json default)"
    echo ""
    exit 1
}

# Check argument
if [ -z "$1" ]; then
    usage
fi

VERSION="$1"

# Validate version format
if ! [[ "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
    echo -e "${RED}Error:${NC} Invalid version format: $VERSION"
    echo "Expected format: MAJOR.MINOR.PATCH (e.g., 0.1.0)"
    exit 1
fi

echo ""
echo -e "${CYAN}Bumping version to ${GREEN}$VERSION${NC}"
echo ""

# Get current version
CURRENT=$(grep -o '"0\.[0-9]\+\.[0-9]\+"' "$ROOT_DIR/src/root.zig" | head -1 | tr -d '"')
echo -e "Current version: ${YELLOW}$CURRENT${NC}"
echo -e "New version:     ${GREEN}$VERSION${NC}"
echo ""

# Update files
update_file() {
    local file="$1"
    local pattern="$2"
    local desc="$3"

    if [ -f "$file" ]; then
        sed -i '' "$pattern" "$file"
        echo -e "  ${GREEN}✓${NC} $desc"
    else
        echo -e "  ${RED}✗${NC} $desc (file not found)"
    fi
}

echo "Updating files:"

update_file "$ROOT_DIR/build.zig.zon" \
    "s/\.version = \"[0-9]\+\.[0-9]\+\.[0-9]\+\"/.version = \"$VERSION\"/" \
    "build.zig.zon"

update_file "$ROOT_DIR/src/root.zig" \
    "s/pub const version = \"[0-9]\+\.[0-9]\+\.[0-9]\+\"/pub const version = \"$VERSION\"/" \
    "src/root.zig"

update_file "$ROOT_DIR/src/root.zig" \
    "s/expectEqualStrings(\"[0-9]\+\.[0-9]\+\.[0-9]\+\"/expectEqualStrings(\"$VERSION\"/" \
    "src/root.zig (test)"

update_file "$ROOT_DIR/src/dbl/root.zig" \
    "s/pub const version = \"[0-9]\+\.[0-9]\+\.[0-9]\+\"/pub const version = \"$VERSION\"/" \
    "src/dbl/root.zig"

update_file "$ROOT_DIR/src/cotdb/cotdb.zig" \
    "s/pub const version = \"[0-9]\+\.[0-9]\+\.[0-9]\+\"/pub const version = \"$VERSION\"/" \
    "src/cotdb/cotdb.zig"

update_file "$ROOT_DIR/src/tui_ext/cot_tui.zig" \
    "s/\.version = \"[0-9]\+\.[0-9]\+\.[0-9]\+\"/.version = \"$VERSION\"/" \
    "src/tui_ext/cot_tui.zig"

update_file "$ROOT_DIR/src/comptime/builtins.zig" \
    "s/fromString(\"[0-9]\+\.[0-9]\+\.[0-9]\+\")/fromString(\"$VERSION\")/" \
    "src/comptime/builtins.zig"

update_file "$ROOT_DIR/src/framework/config.zig" \
    "s/\"version\": \"[0-9]\+\.[0-9]\+\.[0-9]\+\"/\"version\": \"$VERSION\"/" \
    "src/framework/config.zig"

echo ""
echo -e "${GREEN}Done!${NC} Version bumped to $VERSION"
echo ""
echo "Next steps:"
echo "  1. zig build"
echo "  2. Test the changes"
echo "  3. Commit: git commit -am \"Bump version to $VERSION\""
echo ""
