package org.erlide.ui.views.console;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.jface.text.rules.FastPartitioner;
import org.eclipse.jface.text.rules.IPartitionTokenScanner;
import org.erlide.runtime.backend.console.ErlConsoleModel;

//public class ErlConsoleDocument {
public class ErlConsoleDocument extends Document {

	private final ErlConsoleModel model;

	public ErlConsoleDocument(final ErlConsoleModel model) {
		super();
		this.model = model;

		@SuppressWarnings("unused")
		final IDocumentPartitioner partitioner = new FastPartitioner(
				createScanner(), new String[] { "header", "prompt", "input",
						"comment", "text" });
		// partitioner.connect(document);
		// document.setDocumentPartitioner(partitioner);
	}

	private IPartitionTokenScanner createScanner() {
		// TODO Auto-generated method stub
		return null;
	}

	public ErlConsoleModel getModel() {
		return model;
	}

	@Override
	public String get() {
		return super.get();
	}

	@Override
	public void replace(final int pos, final int length, final String text)
			throws BadLocationException {
		super.replace(pos, length, text);
	}
}
