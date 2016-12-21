package org.erlide.server

import org.eclipse.lsp4j.DidChangeConfigurationParams
import org.eclipse.lsp4j.DidChangeWatchedFilesParams
import org.eclipse.lsp4j.WorkspaceSymbolParams
import org.eclipse.lsp4j.services.WorkspaceService

class ErlangWorkspaceService implements WorkspaceService {

    ErlangLanguageServer server

    new(ErlangLanguageServer server) {
        this.server = server
    }

    override symbol(WorkspaceSymbolParams params) {
        println('''symbol''')
        null
    }

    override didChangeConfiguration(DidChangeConfigurationParams params) {
        println('''didChangeConfiguraton''')
    }

    override didChangeWatchedFiles(DidChangeWatchedFilesParams params) {
        println('''didChangeWatchedFiles''')
    }
}
