package org.erlide.server

import java.util.concurrent.CompletableFuture
import org.eclipse.lsp4j.CompletionOptions
import org.eclipse.lsp4j.InitializeParams
import org.eclipse.lsp4j.InitializeResult
import org.eclipse.lsp4j.ServerCapabilities
import org.eclipse.lsp4j.services.LanguageServer

class ErlangLanguageServer implements LanguageServer {

    override initialize(InitializeParams params) {
        println('''INITIALIZE server  «params»''')

        val result = new InitializeResult() {

            override getCapabilities() {
                new ServerCapabilities => [
                    hoverProvider = true
                    definitionProvider = true
                    referencesProvider = true
                    documentSymbolProvider = true
                    workspaceSymbolProvider = true
                    // textDocumentSync = ServerCapabilities.SYNC_INCREMENTAL
                    completionProvider = new CompletionOptions => [
                        resolveProvider = false
                        triggerCharacters = #[":", "#", "?"]
                    ]
                ]
            }

//            override getSupportedLanguages() {
//                val erlang = new LanguageDescription => [
//                    fileExtensions = #["erl", "hrl"]
//                    languageId = "org.erlang"
//                    mimeTypes = #[]
//                ]
//                #[erlang]
//            }

            override toString() {
                '''[ «capabilities»]'''
            }

        }
        CompletableFuture.completedFuture(result)
    }

    override shutdown() {
        println('''SHUTDOWN server''')
        null
    }

    override exit() {
        println('''EXIT server''')
    }

    override getTextDocumentService() {
        new ErlangTextDocumentService(this)
    }

//    override getWindowService() {
//        new ErlangWindowService(this)
//    }

    override getWorkspaceService() {
        new ErlangWorkspaceService(this)
    }

//    override onTelemetryEvent(Consumer<Object> callback) {
//        println('''telemetry event...''')
//    }

}
