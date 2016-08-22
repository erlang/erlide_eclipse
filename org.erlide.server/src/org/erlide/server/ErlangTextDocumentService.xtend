package org.erlide.server

import io.typefox.lsapi.CodeActionParams
import io.typefox.lsapi.CodeLens
import io.typefox.lsapi.CodeLensParams
import io.typefox.lsapi.CompletionItem
import io.typefox.lsapi.DidChangeTextDocumentParams
import io.typefox.lsapi.DidCloseTextDocumentParams
import io.typefox.lsapi.DidOpenTextDocumentParams
import io.typefox.lsapi.DidSaveTextDocumentParams
import io.typefox.lsapi.DocumentFormattingParams
import io.typefox.lsapi.DocumentOnTypeFormattingParams
import io.typefox.lsapi.DocumentRangeFormattingParams
import io.typefox.lsapi.DocumentSymbolParams
import io.typefox.lsapi.PublishDiagnosticsParams
import io.typefox.lsapi.ReferenceParams
import io.typefox.lsapi.RenameParams
import io.typefox.lsapi.TextDocumentPositionParams
import io.typefox.lsapi.services.TextDocumentService
import java.util.function.Consumer

class ErlangTextDocumentService implements TextDocumentService {

    ErlangLanguageServer server

    new(ErlangLanguageServer server) {
        this.server = server
    }

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

}
