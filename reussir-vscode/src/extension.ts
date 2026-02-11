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
            fileEvents: workspace.createFileSystemWatcher("**/*.reu"),
        },
        outputChannel: window.createOutputChannel("Reussir Language Server"),
    };

    client = new LanguageClient(
        "reussir",
        "Reussir Language Server",
        serverOptions,
        clientOptions
    );

    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
