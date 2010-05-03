package org.erlide.wrangler.refactoring.util;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
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
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFunctionClause;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.wrangler.refactoring.backend.ChangedFile;
import org.erlide.wrangler.refactoring.exception.WranglerException;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;

import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class WranglerUtils {
	static public int calculateColumnFromOffset(int offset, int line,
			IDocument doc) {
		int lineOffset;
		try {
			lineOffset = doc.getLineOffset(line);
			int ret = ((offset - lineOffset) < 0 ? 0 : offset - lineOffset);
			return ret + 1;
		} catch (BadLocationException e) {
			e.printStackTrace();
			return 0;

		}
	}

	static public int calculateOffsetFromPosition(int line, int column,
			IDocument doc) throws BadLocationException {
		return doc.getLineOffset(line - 1) + column - 1;
	}

	static public int calculateOffsetFromErlangPos(OtpErlangTuple pos,
			IDocument doc) throws OtpErlangRangeException, BadLocationException {
		int line = ((OtpErlangLong) pos.elementAt(0)).intValue();
		int column = ((OtpErlangLong) pos.elementAt(1)).intValue();
		int offset = calculateOffsetFromPosition(line, column, doc);
		return offset - 1;
	}

	static public String getTextFromEditor(IErlRange range, IDocument doc) {
		try {
			String s = doc.get(range.getOffset(), range.getLength());
			return s;
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
		return "";

	}

	/**
	 * Returns a list of the given project Erlang files.
	 * 
	 * @param project
	 * @return
	 */
	static public ArrayList<String> getModuleNames(IProject project) {
		ArrayList<IFile> erlangFiles = getModules(project);

		ArrayList<String> moduleNames = new ArrayList<String>();
		for (IFile f : erlangFiles) {
			moduleNames.add(removeExtension(f.getName()));
		}
		Collections.sort(moduleNames);
		return moduleNames;
	}

	static public ArrayList<IFile> getModules(IProject project) {
		ArrayList<IFile> erlangFiles = new ArrayList<IFile>();
		try {
			findModulesRecursively(project, erlangFiles);
		} catch (CoreException e) {
			e.printStackTrace();
			return new ArrayList<IFile>();
		}
		return erlangFiles;
	}

	static private void findModulesRecursively(IResource res,
			ArrayList<IFile> files) throws CoreException {
		if (res instanceof IContainer) {
			IContainer c = (IContainer) res;
			for (IResource r : c.members()) {
				findModulesRecursively(r, files);
			}
		} else if (res instanceof IFile) {
			IFile file = (IFile) res;
			if (isErlangFile(file)) {
				files.add(file);
			}
		}
	}

	static private boolean isErlangFile(IFile file) {
		return "erl".equals(file.getFileExtension());
	}

	static public String removeExtension(String fileName) {
		return fileName.substring(0, fileName.lastIndexOf("."));
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
		int startOffset = calculateOffsetFromErlangPos(start, doc);
		int endOffset = calculateOffsetFromErlangPos(end, doc);
		return getTextSegment(startOffset, endOffset, doc);
	}

	static public String getTextSegment(int startOffset, int endOffset,
			IDocument doc) throws BadLocationException {
		String s = doc.get(startOffset, endOffset - startOffset);
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
		int startOffset = calculateOffsetFromPosition(startLine, startPos, doc);
		int endOffset = calculateOffsetFromPosition(endLine, endPos, doc);
		return getTextSegment(startOffset, endOffset, doc);
	}

	static public void highlightOffsetSelection(int startOffset, int endOffset,
			ITextEditor editor) {
		highlightSelection(startOffset, endOffset - startOffset, editor);
	}

	static public void highlightSelection(int offset, int length,
			ITextEditor editor) {
		editor.setHighlightRange(offset, length, true);
		editor.selectAndReveal(offset, length);

	}

	public static void highlightSelection(int offset, int length,
			IErlMemberSelection selection) {
		ITextEditor editor = (ITextEditor) openFile(selection.getFile());
		highlightSelection(offset, length, editor);
	}

	public static void highlightSelection(IErlFunctionClause clause) {
		int offset, length;
		offset = clause.getNameRange().getOffset();
		length = clause.getNameRange().getLength();
		IErlModule module = clause.getModule();
		IEditorPart editor = openFile((IFile) module.getResource());
		highlightSelection(offset, length, (ITextEditor) editor);

	}

	static public IEditorPart openFile(IFile file) {
		IWorkbenchPage page = PlatformUI.getWorkbench()
				.getActiveWorkbenchWindow().getActivePage();
		IEditorDescriptor desc = PlatformUI.getWorkbench().getEditorRegistry()
				.getDefaultEditor(file.getName());

		try {
			return IDE.openEditor(page, file);
			// return page.openEditor(new FileEditorInput(file), desc.getId());
		} catch (PartInitException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;

	}

	static public IDocument getDocument() {
		IWorkbenchPage page = PlatformUI.getWorkbench()
				.getActiveWorkbenchWindow().getActivePage();
		IEditorPart part = page.getActiveEditor();
		ITextEditor editor = (ITextEditor) part;
		IDocumentProvider dp = editor.getDocumentProvider();
		IDocument doc = dp.getDocument(editor.getEditorInput());
		return doc;
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
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return "";
	}

	static public IFile getFileFromPath(String pathString)
			throws WranglerException {
		Path path = new Path(pathString);
		return getFileFromPath(path);
	}

	public static IFile getFileFromPath(IPath path) throws WranglerException {
		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		IFile[] files = // root.findFilesForLocation(path);
		root.findFilesForLocationURI(org.eclipse.core.filesystem.URIUtil
				.toURI(path));
		if (files.length > 0)
			return files[0];// else

		/*
		 * if (file != null) return file;
		 */
		else
			throw new WranglerException("File not found!");
	}

	public static void notifyErlide(ArrayList<ChangedFile> changedFiles) {

		IErlModel model = ErlangCore.getModel();
		for (ChangedFile f : changedFiles) {
			IFile file;
			try {
				file = getFileFromPath(f.getIPath());
				IErlElement element = model.findElement(file);
				IErlModule m = (IErlModule) element;
				m.resourceChanged();
				IEditorPart editor = GlobalParameters.getEditor();
				if (editor instanceof ErlangEditor)
					((ErlangEditor) editor).resetAndCacheScannerAndParser();
				model.notifyChange(m);

			} catch (Exception e) {
				e.printStackTrace();
			}
		}

	}
}
