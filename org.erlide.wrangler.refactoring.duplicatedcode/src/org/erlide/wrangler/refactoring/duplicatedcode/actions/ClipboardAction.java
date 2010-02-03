package org.erlide.wrangler.refactoring.duplicatedcode.actions;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.widgets.Display;

public class ClipboardAction extends Action {
	private final Display display;
	private String text;

	public ClipboardAction(Display display) {
		this.display = display;
	}

	@Override
	public void run() {
		Clipboard cb = new Clipboard(display);
		TextTransfer textTransfer = TextTransfer.getInstance();
		cb.setContents(new Object[] { text },
				new TextTransfer[] { textTransfer });
	}

	@Override
	public void setText(String text) {
		this.text = text;
	}

	private void showMessage(String message) {
		MessageDialog.openInformation(display.getActiveShell(),
				"Duplicated codes", message);
	}
}
