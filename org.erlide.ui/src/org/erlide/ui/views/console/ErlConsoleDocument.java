package org.erlide.ui.views.console;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.jface.text.rules.DefaultPartitioner;
import org.eclipse.jface.text.rules.IPartitionTokenScanner;
import org.erlide.runtime.backend.console.ErlConsoleModel;

//public class ErlConsoleDocument {
public class ErlConsoleDocument extends Document {

	private ErlConsoleModel model;

	public ErlConsoleDocument(ErlConsoleModel model) {
		super();
		this.model = model;

		IDocumentPartitioner partitioner = new DefaultPartitioner(
				createScanner(), new String[] { "header", "prompt", "input",
						"comment" });
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
	public void replace(int pos, int length, String text)
			throws BadLocationException {
		super.replace(pos, length, text);
	}
}
