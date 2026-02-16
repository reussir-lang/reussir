# Reussir Language Support for VSCode

VSCode extension providing language support for [Reussir](https://github.com/SchrodingerZhu/reussir) via the Language Server Protocol.

## Features

- **Diagnostics**: Parse errors and elaboration diagnostics shown inline
- **Hover**: Basic hover support (placeholder, extensible)
- **Language Configuration**: Bracket matching, comment toggling, auto-closing pairs for `.reu` files

## Prerequisites

1. **Node.js** (≥ 18) and **npm** — we recommend using [nvm](https://github.com/nvm-sh/nvm) to manage Node.js versions
2. **reussir-lsp** binary — the Haskell LSP server

### Building `reussir-lsp`

From the project root:

```bash
cabal build reussir-lsp
```

Or via CMake/Ninja:

```bash
ninja reussir-lsp
```

Find the binary path with:

```bash
cabal list-bin reussir-lsp
```

## Setup

### 1. Install dependencies

```bash
cd reussir-vscode
npm install
```

### 2. Compile the extension

```bash
npm run compile
```

### 3. Run in development mode

Open the `reussir-vscode` directory in VSCode, then press **F5** to launch the Extension Development Host. Any `.reu` file opened in the new window will activate the extension.

### 4. Configure the LSP server path

By default, the extension looks for `reussir-lsp` on your `$PATH`. To use a specific binary, set `reussir.lsp.serverPath` in your VSCode settings:

```json
{
  "reussir.lsp.serverPath": "/path/to/reussir-lsp"
}
```

## Packaging as `.vsix`

```bash
npm install -g @vscode/vsce
cd reussir-vscode
vsce package
```

This produces a `.vsix` file you can install with:

```bash
code --install-extension reussir-vscode-0.1.0.vsix
```

## Extension Settings

| Setting | Default | Description |
|---------|---------|-------------|
| `reussir.lsp.serverPath` | `"reussir-lsp"` | Path to the `reussir-lsp` executable |
| `reussir.lsp.trace.server` | `"off"` | Trace communication between VSCode and the LSP server (`off` / `messages` / `verbose`) |

## Development

- `npm run watch` — recompile on file changes
- `npm run lint` — run ESLint (requires eslint dev dependency)
