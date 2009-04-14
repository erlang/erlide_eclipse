package org.erlide.ui.views.console;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Document;
import org.erlide.runtime.backend.console.ErlConsoleModel;

//public class ErlConsoleDocument {
public class ErlConsoleDocument extends Document {

	private ErlConsoleModel model;

	public ErlConsoleDocument(ErlConsoleModel model) {
		super();
		this.model = model;
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
