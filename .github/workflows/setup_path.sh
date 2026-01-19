#!/bin/bash
# Setup paths for MSYS2 environment (replaces the Windows PATH step)
# This script should be sourced: . .github/workflows/setup_path.sh

# Add MSYS2 clang64 bin directory
export PATH="/clang64/bin:$PATH"

# Add project build bin directory
export PATH="$GITHUB_WORKSPACE/build/bin:$PATH"

# Add Windows downlevel directory for compatibility
export PATH="/c/Windows/System32/downlevel:$PATH"
