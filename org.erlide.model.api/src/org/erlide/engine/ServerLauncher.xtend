package org.erlide.engine

import io.typefox.lsapi.impl.ClientCapabilitiesImpl
import io.typefox.lsapi.impl.InitializeParamsImpl
import io.typefox.lsapi.services.json.LanguageServerToJsonAdapter
import java.io.InputStream
import java.io.OutputStream

class ServerLauncher {
    ErlangLanguageServer languageServer = ErlangLanguageServer.instance;

    def void run(InputStream in, OutputStream out) throws Exception {
        val adapter = new LanguageServerToJsonAdapter(languageServer)
        adapter.connect(in, out)
        //adapter.join()
    }

    def start() {
        val server = ErlangLanguageServer.getInstance();
        val params = new InitializeParamsImpl();
        params.setProcessId(0);
        params.setRootPath("");
        params.setClientName("demo client");
        params.setCapabilities(new ClientCapabilitiesImpl());
        val result = server.initialize(params).get();
        println("INIT:: " + result.getSupportedLanguages());
    }

}
