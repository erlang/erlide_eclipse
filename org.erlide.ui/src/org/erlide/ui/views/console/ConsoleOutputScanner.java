package org.erlide.ui.views.console;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.ITokenScanner;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.erlide.ui.editors.erl.ColorManager;
import org.erlide.ui.util.IColorManager;

public class ConsoleOutputScanner implements ITokenScanner {

	protected final IColorManager fColorManager;
	private boolean gotten;
	private String text;
	private int offset;

	public ConsoleOutputScanner(ColorManager colorManager) {
		fColorManager = colorManager;
	}

	public int getTokenLength() {
		return text.length();
	}

	public int getTokenOffset() {
		return offset;
	}

	public IToken nextToken() {
		if (gotten) {
			return Token.EOF;
		} else {
			gotten = true;
			if (text == null) {
				return Token.EOF;
			}
			return new Token(getAttribute(text));
		}
	}

	private TextAttribute getAttribute(String string) {
		int style = SWT.NORMAL;
		Color bg = null;
		Color fg = null;
		if (string.startsWith("Eshell V")) {
			style = SWT.BOLD;
		} else if (string.matches("^\\([^)]+\\)[0-9]+> $")) {
			fg = fColorManager.getColor(new RGB(0, 0, 100));
			bg = fColorManager.getColor(new RGB(255, 255, 230));
			style = SWT.BOLD;
		}
		return new TextAttribute(fg, bg, style);
	}

	public void setRange(IDocument document, int offset, int length) {
		this.offset = offset;
		try {
			text = document.get(offset, length);
		} catch (BadLocationException e) {
			text = null;
		}
		gotten = false;
	}

}
