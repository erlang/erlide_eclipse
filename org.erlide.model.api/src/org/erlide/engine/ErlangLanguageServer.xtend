package org.erlide.engine

import io.typefox.lsapi.CodeActionParams
import io.typefox.lsapi.CodeLens
import io.typefox.lsapi.CodeLensParams
import io.typefox.lsapi.CompletionItem
import io.typefox.lsapi.DidChangeConfigurationParams
import io.typefox.lsapi.DidChangeTextDocumentParams
import io.typefox.lsapi.DidChangeWatchedFilesParams
import io.typefox.lsapi.DidCloseTextDocumentParams
import io.typefox.lsapi.DidOpenTextDocumentParams
import io.typefox.lsapi.DidSaveTextDocumentParams
import io.typefox.lsapi.DocumentFormattingParams
import io.typefox.lsapi.DocumentOnTypeFormattingParams
import io.typefox.lsapi.DocumentRangeFormattingParams
import io.typefox.lsapi.DocumentSymbolParams
import io.typefox.lsapi.InitializeParams
import io.typefox.lsapi.InitializeResult
import io.typefox.lsapi.MessageParams
import io.typefox.lsapi.PublishDiagnosticsParams
import io.typefox.lsapi.ReferenceParams
import io.typefox.lsapi.RenameParams
import io.typefox.lsapi.ShowMessageRequestParams
import io.typefox.lsapi.TextDocumentPositionParams
import io.typefox.lsapi.WorkspaceSymbolParams
import io.typefox.lsapi.impl.CompletionOptionsImpl
import io.typefox.lsapi.impl.LanguageDescriptionImpl
import io.typefox.lsapi.impl.ServerCapabilitiesImpl
import io.typefox.lsapi.services.LanguageServer
import io.typefox.lsapi.services.TextDocumentService
import io.typefox.lsapi.services.WindowService
import io.typefox.lsapi.services.WorkspaceService
import java.util.concurrent.CompletableFuture
import java.util.function.Consumer

class ErlangLanguageServer implements LanguageServer, WindowService, WorkspaceService, TextDocumentService {

    val static instance = new ErlangLanguageServer

    private new() {
    }

    def static getInstance() {
        instance
    }

    override exit() {
        println('''EXIT server''')
    }

    override getTextDocumentService() {
        textDocumentService
    }

    override getWindowService() {
        windowService
    }

    override getWorkspaceService() {
        workspaceService
    }

    override initialize(InitializeParams params) {
        println('''INITIALIZE server''')
        val result = new InitializeResult() {

            override getCapabilities() {
                new ServerCapabilitiesImpl => [
                    hoverProvider = true
                    definitionProvider = true
                    referencesProvider = true
                    documentSymbolProvider = true
                    workspaceSymbolProvider = true
                    //textDocumentSync = ServerCapabilities.SYNC_INCREMENTAL
                    completionProvider = new CompletionOptionsImpl => [
                        resolveProvider = false
                        triggerCharacters = #[":", "#", "?"]
                    ]
                ]
            }

            override getSupportedLanguages() {
                val testLanguage = new LanguageDescriptionImpl => [
                    fileExtensions = #["erl", "hrl"]
                    languageId = "org.erlide.erlang"
                    mimeTypes = #[]
                ]
                #[testLanguage]
            }

        }
        CompletableFuture.completedFuture(result)
    }

    override shutdown() {
        println('''SHUTDOWN server''')
    }

    // WindowService
    override onShowMessage(Consumer<MessageParams> callback) {
        println('''onShowMessage''')
    }

    override onShowMessageRequest(Consumer<ShowMessageRequestParams> callback) {
        println('''onShowMessageRequest''')
    }

    override onLogMessage(Consumer<MessageParams> callback) {
        println('''onLogMessage''')
    }

    // WorkspaceService
    override symbol(WorkspaceSymbolParams params) {
        println('''symbol''')
        null
    }

    override didChangeConfiguraton(DidChangeConfigurationParams params) {
        println('''didChangeConfiguraton''')
    }

    override didChangeWatchedFiles(DidChangeWatchedFilesParams params) {
        println('''didChangeWatchedFiles''')
    }

    // TextDocumentService
    override completion(TextDocumentPositionParams position) {
        println('''completion''')
        null
    }

    override resolveCompletionItem(CompletionItem unresolved) {
        println('''resolveCompletionItem''')
        null
    }

    override hover(TextDocumentPositionParams position) {
        println('''hover''')
        null
    }

    override signatureHelp(TextDocumentPositionParams position) {
        println('''signatureHelp''')
        null
    }

    override definition(TextDocumentPositionParams position) {
        println('''definition''')
        null
    }

    override references(ReferenceParams params) {
        println('''references''')
        null
    }

    override documentHighlight(TextDocumentPositionParams position) {
        println('''documentHighlight''')
        null
    }

    override documentSymbol(DocumentSymbolParams params) {
        println('''documentSymbol''')
        null
    }

    override codeAction(CodeActionParams params) {
        println('''codeAction''')
        null
    }

    override codeLens(CodeLensParams params) {
        println('''codeLens''')
        null
    }

    override resolveCodeLens(CodeLens unresolved) {
        println('''resolveCodeLens''')
        null
    }

    override formatting(DocumentFormattingParams params) {
        println('''formatting''')
        null
    }

    override rangeFormatting(DocumentRangeFormattingParams params) {
        println('''rangeFormatting''')
        null
    }

    override onTypeFormatting(DocumentOnTypeFormattingParams params) {
        println('''onTypeFormatting''')
        null
    }

    override rename(RenameParams params) {
        println('''rename''')
        null
    }

    override didOpen(DidOpenTextDocumentParams params) {
        println('''didOpen''')
    }

    override didChange(DidChangeTextDocumentParams params) {
        println('''didChange''')
    }

    override didClose(DidCloseTextDocumentParams params) {
        println('''didClose''')
    }

    override didSave(DidSaveTextDocumentParams params) {
        println('''didSave''')
    }

    override onPublishDiagnostics(Consumer<PublishDiagnosticsParams> callback) {
        println('''onPublishDiagnostics''')
    }

    override onTelemetryEvent(Consumer<Object> callback) {
        throw new UnsupportedOperationException("TODO: auto-generated method stub")
    }

}
