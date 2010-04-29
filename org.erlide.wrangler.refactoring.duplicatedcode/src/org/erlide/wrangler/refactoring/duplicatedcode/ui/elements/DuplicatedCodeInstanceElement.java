package org.erlide.wrangler.refactoring.duplicatedcode.ui.elements;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.erlide.wrangler.refactoring.util.WranglerUtils;

public class DuplicatedCodeInstanceElement extends AbstractResultTreeObject {

	private int startOffset;
	private int endOffset;
	private String codePartString;
	private final IFile containingFile;
	private final int startLine;

	// private ITextEditor containingEditor;

	public DuplicatedCodeInstanceElement(IFile containingFile, int startLine,
			int startColumn, int endLine, int endColumn) {

		this.containingFile = containingFile;
		this.startLine = startLine;
		IDocument doc = WranglerUtils.getDocument(containingFile);
		try {
			this.startOffset = WranglerUtils.calculateOffsetFromPosition(
					startLine, startColumn, doc);
			this.endOffset = WranglerUtils.calculateOffsetFromPosition(endLine,
					endColumn, doc);
			this.codePartString = WranglerUtils.getTextSegment(startOffset,
					endOffset, doc);
		} catch (BadLocationException e) {
			e.printStackTrace();
		}

	}

	public int getStartOffset() {
		return startOffset;
	}

	public int getEndOffset() {
		return endOffset;
	}

	/*
	 * public ITextEditor getEditor() { return containingEditor; }
	 */

	public IFile getContainingFile() {
		return containingFile;
	}

	public String getCodePartString() {
		return simplifyCodePartString(codePartString);
	}

	@Override
	public String getName() {
		return startLine + ": \"" + getCodePartString() + "\"";
	}

	protected String simplifyCodePartString(String input) {
		return input;// input.replace("\n", " ").replace("\t", "").replace("\r",
						// "");

	}
}
