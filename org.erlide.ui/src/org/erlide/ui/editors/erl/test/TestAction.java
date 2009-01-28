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
import org.erlide.ui.editors.erl.ErlangEditor;

import erlang.ErlideScanner;

/**
 * @author jakob
 * 
 */
public class TestAction extends TextEditorAction {

	private final IErlModule module;

	public TestAction(final ResourceBundle bundle, final String prefix,
			final ITextEditor editor, final IErlModule module) {
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
		if (false) {
			final IDocument document = textEditor.getDocumentProvider()
					.getDocument(textEditor.getEditorInput());
			final String text = document.get();
			try {
				final String s = ErlideScanner.checkAll(ErlScanner
						.createScannerModuleName(module), text);
				ErlLogger.debug("%s", s);
			} catch (final BackendException e) {
				ErlLogger.warn(e);
			}
		} else {
			final ErlangEditor ee = (ErlangEditor) textEditor;
			ee.reconcileNow();
		}
	}

}
