package org.erlide.wrangler.refactoring.util;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.wrangler.refactoring.core.exception.WranglerRefactoringException;

import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class EditorUtil {

	/**
	 * Calculates the column number from offset.
	 * 
	 * @param offset
	 *            offset of the selected text
	 * @return the column number of the selected text
	 */
	static public int calculateColumn(int offset, IDocument doc) {
		int sumpos = 0;
		int i = 0;

		try {
			while (sumpos + doc.getLineLength(i) - 1 < offset) {
				sumpos += doc.getLineLength(i);
				++i;
			}
		} catch (BadLocationException e) {
			// TODO: what the hell do I need here???
			e.printStackTrace();
		}

		return offset - sumpos + 1;
	}

	/**
	 * Converts the given position to Eclipse's offset.
	 * 
	 * @param line
	 *            line of the position
	 * @param column
	 *            column of the positions
	 * @return calculated offset
	 * @throws BadLocationException
	 */
	static public int calculateOffset(int line, int column, IDocument doc)
			throws BadLocationException {
		return doc.getLineOffset(line - 1) + column;
	}

	/**
	 * Converts the given tuple to offset information
	 * 
	 * @param pos
	 *            tuple with 2 element: line:int(), col:int()
	 * @return
	 * @throws OtpErlangRangeException
	 * @throws BadLocationException
	 */
	static public int calculateOffset(OtpErlangTuple pos, IDocument doc)
			throws OtpErlangRangeException, BadLocationException {
		int line = ((OtpErlangLong) pos.elementAt(0)).intValue();
		int column = ((OtpErlangLong) pos.elementAt(1)).intValue();
		int offset = calculateOffset(line, column, doc);
		return offset - 1;
	}

	/**
	 * Returns the selection's text from the active editor's text file.
	 * 
	 * @param start
	 *            tuple with 2 element: line:int(), col:int()
	 * @param end
	 *            tuple with 2 element: line:int(), col:int()
	 * @return
	 * @throws OtpErlangRangeException
	 * @throws BadLocationException
	 */
	static public String getTextSegment(OtpErlangTuple start,
			OtpErlangTuple end, IDocument doc) throws OtpErlangRangeException,
			BadLocationException {
		int startOffset = calculateOffset(start, doc);
		int endOffset = calculateOffset(end, doc);
		return getTextSegment(startOffset, endOffset, doc);
	}

	static public String getTextSegment(int startOffset, int endOffset,
			IDocument doc) throws BadLocationException {
		String s = doc.get(startOffset, endOffset - startOffset + 1);
		return s;
	}

	/*
	 * static public String getTextSegment(int startLine, int startPos, int
	 * endLine, int endPos, IFile file) throws BadLocationException { IDocument
	 * doc = getDocument(file); return getTextSegment(startLine, startPos,
	 * endLine, endPos, doc); }
	 */

	public static String getTextSegment(int startLine, int startPos,
			int endLine, int endPos, IDocument doc) throws BadLocationException {
		int startOffset = calculateOffset(startLine, startPos, doc);
		int endOffset = calculateOffset(endLine, endPos, doc);
		return getTextSegment(startOffset, endOffset, doc);
	}

	static public void highlightSelection(int startOffset, int endOffset,
			ITextEditor editor) {
		editor
				.setHighlightRange(startOffset, endOffset - startOffset + 1,
						true);
		editor.selectAndReveal(startOffset, endOffset - startOffset + 1);
	}

	static public IEditorPart openFile(IFile file) {
		IWorkbenchPage page = PlatformUI.getWorkbench()
				.getActiveWorkbenchWindow().getActivePage();
		IEditorDescriptor desc = PlatformUI.getWorkbench().getEditorRegistry()
				.getDefaultEditor(file.getName());

		try {
			return page.openEditor(new FileEditorInput(file), desc.getId());
		} catch (PartInitException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;

	}

	static public IDocument getDocument(IFile file) {
		return new Document(getFileContent(file));
	}

	/**
	 * Returns the corresponding IDocument object
	 * 
	 * @param editor
	 * @return
	 */
	static public IDocument getDocument(ITextEditor editor) {
		IFileEditorInput input = (IFileEditorInput) (editor.getEditorInput());
		IDocument doc = editor.getDocumentProvider().getDocument(input);

		return doc;
	}

	static private String getFileContent(IFile file) {
		try {
			InputStream in = file.getContents();
			ByteArrayOutputStream out = new ByteArrayOutputStream();
			byte[] buf = new byte[1024];
			int read = in.read(buf);
			while (read > 0) {
				out.write(buf, 0, read);
				read = in.read(buf);
			}
			return out.toString();
		} catch (CoreException e) {
		} catch (IOException e) {
		}
		return "";
	}

	static public IFile geFileFromPath(String pathString)
			throws WranglerRefactoringException {
		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		Path path = new Path(pathString);
		IFile[] files = root.findFilesForLocation(path);
		if (files.length > 0)
			return files[0];
		else
			throw new WranglerRefactoringException("File not found!");
	}
}
