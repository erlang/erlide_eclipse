package org.erlide.server

import io.typefox.lsapi.MessageParams
import io.typefox.lsapi.ShowMessageRequestParams
import io.typefox.lsapi.services.WindowService
import java.util.function.Consumer

class ErlangWindowService implements WindowService {

    ErlangLanguageServer server

    new(ErlangLanguageServer server) {
        this.server = server
    }

    override onShowMessage(Consumer<MessageParams> callback) {
        println('''onShowMessage''')
    }

    override onShowMessageRequest(Consumer<ShowMessageRequestParams> callback) {
        println('''onShowMessageRequest''')
    }

    override onLogMessage(Consumer<MessageParams> callback) {
        println('''onLogMessage''')
    }
}
