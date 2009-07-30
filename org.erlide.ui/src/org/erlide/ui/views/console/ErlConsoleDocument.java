package org.erlide.ui.views.console;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.jface.text.rules.FastPartitioner;
import org.eclipse.jface.text.rules.IPartitionTokenScanner;
import org.erlide.runtime.backend.console.ErlConsoleModel;

public class ErlConsoleDocument extends Document {

	public static final String OUTPUT_TYPE = "output";
	public static final String INPUT_TYPE = "input";

	private static final String[] LEGAL_CONTENT_TYPES = new String[] {
			INPUT_TYPE, OUTPUT_TYPE };

	private final ErlConsoleModel model;

	public ErlConsoleDocument(final ErlConsoleModel model) {
		super();
		Assert.isNotNull(model);
		this.model = model;

		setTextStore(new IoRequestStore(model));

		final IDocumentPartitioner partitioner = new FastPartitioner(
				createScanner(), LEGAL_CONTENT_TYPES);
		partitioner.connect(this);
		setDocumentPartitioner(partitioner);
	}

	private IPartitionTokenScanner createScanner() {
		return new IoRequestScanner(model);
	}

	@Override
	public void replace(final int pos, final int length, final String text)
			throws BadLocationException {
	}
}
