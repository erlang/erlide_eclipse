package org.erlide.server

import org.eclipse.lsp4j.CodeActionParams
import org.eclipse.lsp4j.CodeLens
import org.eclipse.lsp4j.CodeLensParams
import org.eclipse.lsp4j.CompletionItem
import org.eclipse.lsp4j.DidChangeTextDocumentParams
import org.eclipse.lsp4j.DidCloseTextDocumentParams
import org.eclipse.lsp4j.DidOpenTextDocumentParams
import org.eclipse.lsp4j.DidSaveTextDocumentParams
import org.eclipse.lsp4j.DocumentFormattingParams
import org.eclipse.lsp4j.DocumentOnTypeFormattingParams
import org.eclipse.lsp4j.DocumentRangeFormattingParams
import org.eclipse.lsp4j.DocumentSymbolParams
import org.eclipse.lsp4j.ReferenceParams
import org.eclipse.lsp4j.RenameParams
import org.eclipse.lsp4j.TextDocumentPositionParams
import org.eclipse.lsp4j.services.TextDocumentService
import org.eclipse.lsp4j.CompletionParams

class ErlangTextDocumentService implements TextDocumentService {

	ErlangLanguageServer server

	new(ErlangLanguageServer server) {
		this.server = server
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

	override completion(CompletionParams position) {
		throw new UnsupportedOperationException("TODO: auto-generated method stub")
	}

//    override onPublishDiagnostics(Consumer<PublishDiagnosticsParams> callback) {
//        println('''onPublishDiagnostics''')
//    }
}
