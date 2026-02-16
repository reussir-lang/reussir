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
    const outputChannel = window.createOutputChannel("Reussir Language Server");
    context.subscriptions.push(outputChannel);
    outputChannel.appendLine("[reussir-vscode] activate() called.");
    console.log("[reussir-vscode] activate() called.");

    const config = workspace.getConfiguration("reussir.lsp");
    const serverPath: string = config.get<string>("serverPath", "reussir-lsp");
    outputChannel.appendLine(`[reussir-vscode] Starting Reussir LSP with server path: ${serverPath}`);

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

        client.onDidChangeState((event) => {
            outputChannel.appendLine(
                `[reussir-vscode] client state ${event.oldState} -> ${event.newState}`
            );
        });

        client.start().then(() => {
            outputChannel.appendLine("[reussir-vscode] Reussir LSP started successfully.");
        }).catch((error: unknown) => {
            const message = `[reussir-vscode] Reussir LSP failed to start: ${String(error)}`;
            outputChannel.appendLine(message);
            outputChannel.show(true);
            window.showErrorMessage(message);
        });
    } catch (error: unknown) {
        const message = `[reussir-vscode] Failed to create Reussir LSP client: ${String(error)}`;
        outputChannel.appendLine(message);
        outputChannel.show(true);
        window.showErrorMessage(message);
    }
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
