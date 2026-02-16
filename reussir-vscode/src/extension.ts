import * as path from "path";
import {
    workspace,
    ExtensionContext,
    window,
} from "vscode";
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

export function activate(context: ExtensionContext): void {
    const config = workspace.getConfiguration("reussir.lsp");
    const serverPath: string = config.get<string>("serverPath", "reussir-lsp");

    const outputChannel = window.createOutputChannel("Reussir Language Server");
    outputChannel.appendLine(`Starting Reussir LSP with server path: ${serverPath}`);

    const serverOptions: ServerOptions = {
        run: {
            command: serverPath,
            transport: TransportKind.stdio,
        },
        debug: {
            command: serverPath,
            transport: TransportKind.stdio,
        },
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: "file", language: "reussir" }],
        synchronize: {
            fileEvents: workspace.createFileSystemWatcher("**/*.rr"),
        },
        outputChannel: outputChannel,
    };

    try {
        client = new LanguageClient(
            "reussir",
            "Reussir Language Server",
            serverOptions,
            clientOptions
        );

        client.start().then(() => {
            outputChannel.appendLine("Reussir LSP started successfully.");
        }).catch((error) => {
            outputChannel.appendLine(`Reussir LSP failed to start: ${error}`);
            window.showErrorMessage(`Reussir LSP failed to start: ${error}`);
        });
    } catch (error) {
        outputChannel.appendLine(`Failed to create Reussir LSP client: ${error}`);
        window.showErrorMessage(`Failed to create Reussir LSP client: ${error}`);
    }
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
