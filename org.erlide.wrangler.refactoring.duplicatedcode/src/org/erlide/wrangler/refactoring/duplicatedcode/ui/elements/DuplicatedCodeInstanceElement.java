package org.erlide.wrangler.refactoring.duplicatedcode.ui.elements;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.erlide.wrangler.refactoring.util.EditorUtil;

public class DuplicatedCodeInstanceElement extends AbstractResultTreeObject {

	private int startOffset;
	private int endOffset;
	private String codePartString;
	private IFile containingFile;
	private int startLine;

	// private ITextEditor containingEditor;

	public DuplicatedCodeInstanceElement(IFile containingFile, int startLine,
			int startColumn, int endLine, int endColumn) {

		this.containingFile = containingFile;
		this.startLine = startLine;
		IDocument doc = EditorUtil.getDocument(containingFile);
		try {
			this.startOffset = EditorUtil.calculateOffset(startLine,
					startColumn, doc) - 1;
			this.endOffset = EditorUtil
					.calculateOffset(endLine, endColumn, doc) - 2;
			this.codePartString = EditorUtil.getTextSegment(startOffset,
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
		return codePartString;
	}

	@Override
	public String getName() {
		return startLine + ": \"" + getCodePartString() + "\"";
	}
}
