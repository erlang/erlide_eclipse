package org.erlide.server

import io.typefox.lsapi.DidChangeConfigurationParams
import io.typefox.lsapi.DidChangeWatchedFilesParams
import io.typefox.lsapi.WorkspaceSymbolParams
import io.typefox.lsapi.services.WorkspaceService

class ErlangWorkspaceService implements WorkspaceService {

    ErlangLanguageServer server

    new(ErlangLanguageServer server) {
        this.server = server
    }

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
}
