package org.erlide.server

import io.typefox.lsapi.InitializeParams
import io.typefox.lsapi.InitializeResult
import io.typefox.lsapi.impl.CompletionOptionsImpl
import io.typefox.lsapi.impl.LanguageDescriptionImpl
import io.typefox.lsapi.impl.ServerCapabilitiesImpl
import io.typefox.lsapi.services.LanguageServer
import java.util.concurrent.CompletableFuture
import java.util.function.Consumer

class ErlangLanguageServer implements LanguageServer {

    override initialize(InitializeParams params) {
        println('''INITIALIZE server  «params»''')

        val result = new InitializeResult() {

            override getCapabilities() {
                new ServerCapabilitiesImpl => [
                    hoverProvider = true
                    definitionProvider = true
                    referencesProvider = true
                    documentSymbolProvider = true
                    workspaceSymbolProvider = true
                    // textDocumentSync = ServerCapabilities.SYNC_INCREMENTAL
                    completionProvider = new CompletionOptionsImpl => [
                        resolveProvider = false
                        triggerCharacters = #[":", "#", "?"]
                    ]
                ]
            }

            override getSupportedLanguages() {
                val erlang = new LanguageDescriptionImpl => [
                    fileExtensions = #["erl", "hrl"]
                    languageId = "org.erlang"
                    mimeTypes = #[]
                ]
                #[erlang]
            }

            override toString() {
                '''[ «capabilities», «supportedLanguages» ]'''
            }

        }
        CompletableFuture.completedFuture(result)
    }

    override shutdown() {
        println('''SHUTDOWN server''')
    }

    override exit() {
        println('''EXIT server''')
    }

    override getTextDocumentService() {
        new ErlangTextDocumentService(this)
    }

    override getWindowService() {
        new ErlangWindowService(this)
    }

    override getWorkspaceService() {
        new ErlangWorkspaceService(this)
    }

    override onTelemetryEvent(Consumer<Object> callback) {
        println('''telemetry event...''')
    }

}
