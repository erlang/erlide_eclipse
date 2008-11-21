/**
 *
 */
package org.erlide.ui.editors.erl.test;

import java.util.ResourceBundle;

import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.TextEditorAction;
import org.erlide.core.erlang.ErlScanner;
import org.erlide.core.erlang.IErlModule;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.exceptions.BackendException;

import erlang.ErlideScanner2;

/**
 * @author jakob
 * 
 */
public class TestAction extends TextEditorAction {

	private final IErlModule module;

	public TestAction(ResourceBundle bundle, String prefix, ITextEditor editor,
			IErlModule module) {
		super(bundle, prefix, editor);
		this.module = module;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.action.Action#run()
	 */
	@Override
	public void run() {
		super.run();
		final ITextEditor textEditor = getTextEditor();
		final IDocument document = textEditor.getDocumentProvider()
				.getDocument(textEditor.getEditorInput());
		final String text = document.get();
		try {
			final String s = ErlideScanner2.checkAll(ErlScanner
					.createScannerModuleName(module), text);
			ErlLogger.debug("%s", s);
		} catch (final BackendException e) {
			ErlLogger.warn(e);
		}
	}

}
