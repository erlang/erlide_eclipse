/*******************************************************************************
 * Copyright (c) 2010 György Orosz.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     György Orosz - initial API and implementation
 ******************************************************************************/
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
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.core.model.erlang.IErlFunctionClause;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlModel;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.wrangler.refactoring.backend.ChangedFile;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;

import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Utility class, which aims to collect common frequently used functions.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public final class WranglerUtils {
    private WranglerUtils() {
    }

    /**
     * Gets the column number from offset value
     * 
     * @param offset
     *            offset in the current editor
     * @param line
     *            line number in the current editor
     * @param doc
     *            current document, in which the offset is calculated
     * @return column number
     */
    static public int calculateColumnFromOffset(final int offset,
            final int line, final IDocument doc) {
        int lineOffset;
        try {
            lineOffset = doc.getLineOffset(line);
            final int ret = offset - lineOffset < 0 ? 0 : offset - lineOffset;
            return ret + 1;
        } catch (final BadLocationException e) {
            e.printStackTrace();
            return 0;

        }
    }

    /**
     * Calculates offset from the given line and column number.
     * 
     * @param line
     *            line number
     * @param column
     *            column number
     * @param doc
     *            the document in which the position is given
     * @return offset in the document
     * @throws BadLocationException
     *             if the given position doesn't exist
     */
    static public int calculateOffsetFromPosition(final int line,
            final int column, final IDocument doc) throws BadLocationException {
        return doc.getLineOffset(line - 1) + column - 1;
    }

    /**
     * Calculate offset from a position.
     * 
     * @param pos
     *            Position wrapped in an Erlang Tuple: {Line, Column}
     * @param doc
     *            the current document
     * @return offset in the document
     * @throws OtpErlangRangeException
     *             if the tuple is not well formed
     * @throws BadLocationException
     *             if the given position doesn't exist
     */
    static public int calculateOffsetFromErlangPos(final OtpErlangTuple pos,
            final IDocument doc) throws OtpErlangRangeException,
            BadLocationException {
        final int line = ((OtpErlangLong) pos.elementAt(0)).intValue();
        final int column = ((OtpErlangLong) pos.elementAt(1)).intValue();
        final int offset = calculateOffsetFromPosition(line, column, doc);
        return offset - 1;
    }

    /**
     * Gets a string from a document.
     * 
     * @param range
     *            Position range
     * @param doc
     *            source document
     * @return requested string
     */
    static public String getTextFromEditor(final IErlRange range,
            final IDocument doc) {
        try {
            final String s = doc.get(range.getOffset(), range.getLength());
            return s;
        } catch (final BadLocationException e) {
            e.printStackTrace();
        }
        return "";

    }

    /**
     * Returns a list of the given project Erlang files.
     * 
     * @param project
     *            the containing project in which the scan is run
     * @return module names' list
     */
    static public ArrayList<String> getModuleNames(final IProject project) {
        final ArrayList<IFile> erlangFiles = getModules(project);

        final ArrayList<String> moduleNames = new ArrayList<String>();
        for (final IFile f : erlangFiles) {
            moduleNames.add(removeExtension(f.getName()));
        }
        Collections.sort(moduleNames);
        return moduleNames;
    }

    /**
     * Get modules in an Erlang project.
     * 
     * @param project
     *            the project in which the scan is run
     * @return module list
     */
    static public ArrayList<IFile> getModules(final IProject project) {
        final ArrayList<IFile> erlangFiles = new ArrayList<IFile>();
        try {
            findModulesRecursively(project, erlangFiles);
        } catch (final CoreException e) {
            e.printStackTrace();
            return new ArrayList<IFile>();
        }
        return erlangFiles;
    }

    static private void findModulesRecursively(final IResource res,
            final ArrayList<IFile> files) throws CoreException {
        if (res instanceof IContainer) {
            final IContainer c = (IContainer) res;
            for (final IResource r : c.members()) {
                findModulesRecursively(r, files);
            }
        } else if (res instanceof IFile) {
            final IFile file = (IFile) res;
            if (isErlangFile(file)) {
                files.add(file);
            }
        }
    }

    static public boolean isErlangFile(final IFile file) {
        return "erl".equals(file.getFileExtension())
                || "hrl".equals(file.getFileExtension());
    }

    /**
     * Removes the extension from a filename
     * 
     * @param fileName
     *            the file's name
     * @return file name without the extension
     */
    static public String removeExtension(final String fileName) {
        return fileName.substring(0, fileName.lastIndexOf("."));
    }

    /**
     * Returns the selection's text from the active editor's text file.
     * 
     * @param start
     *            tuple with 2 element: line:int(), col:int()
     * @param end
     *            tuple with 2 element: line:int(), col:int()
     * @param doc
     *            the document in which the scan is run
     * @return selected text
     * @throws OtpErlangRangeException
     *             if the position tuple is not well formed
     * @throws BadLocationException
     *             if the given position does not exist
     */
    static public String getTextSegment(final OtpErlangTuple start,
            final OtpErlangTuple end, final IDocument doc)
            throws OtpErlangRangeException, BadLocationException {
        final int startOffset = calculateOffsetFromErlangPos(start, doc);
        final int endOffset = calculateOffsetFromErlangPos(end, doc);
        return getTextSegment(startOffset, endOffset, doc);
    }

    /**
     * Returns the selection's text from the active editor's text file.
     * 
     * @param startOffset
     *            start offset
     * @param endOffset
     *            end offset
     * @param doc
     *            selected document
     * @return selected text
     * @throws BadLocationException
     *             if the positions does not name a corrsssect place
     */
    public static String getTextSegment(final int startOffset,
            final int endOffset, final IDocument doc)
            throws BadLocationException {
        final String s = doc.get(startOffset, endOffset - startOffset);
        return s;
    }

    /*
     * static public String getTextSegment(int startLine, int startPos, int
     * endLine, int endPos, IFile file) throws BadLocationException { IDocument
     * doc = getDocument(file); return getTextSegment(startLine, startPos,
     * endLine, endPos, doc); }
     */

    protected static String getTextSegment(final int startLine,
            final int startPos, final int endLine, final int endPos,
            final IDocument doc) throws BadLocationException {
        final int startOffset = calculateOffsetFromPosition(startLine,
                startPos, doc);
        final int endOffset = calculateOffsetFromPosition(endLine, endPos, doc);
        return getTextSegment(startOffset, endOffset, doc);
    }

    /**
     * Highlight the given selection in the given editor
     * 
     * @param startOffset
     *            selection's starting offset
     * @param endOffset
     *            selection's ending offset
     * @param editor
     *            editor in which the highlightion should be done
     */
    static public void highlightOffsetSelection(final int startOffset,
            final int endOffset, final ITextEditor editor) {
        highlightSelection(startOffset, endOffset - startOffset, editor);
    }

    /**
     * Highlight the given selection in the given editor
     * 
     * @param offset
     *            selection's starting offset
     * @param length
     *            selection's length
     * @param editor
     *            editor in which the highlightion should be done
     */
    static protected void highlightSelection(final int offset,
            final int length, final ITextEditor editor) {
        editor.setHighlightRange(offset, length, true);
        editor.selectAndReveal(offset, length);

    }

    /**
     * Highlight the given selection in the editor which contains the given
     * IErlMemberSelection
     * 
     * @param offset
     *            selection's starting offset
     * @param length
     *            selection's length
     * @param selection
     *            Selected Erlang member
     */
    public static void highlightSelection(final int offset, final int length,
            final IErlMemberSelection selection) {
        final ITextEditor editor = (ITextEditor) openFile(selection.getFile());
        highlightSelection(offset, length, editor);
    }

    /**
     * Highlights the given function clause
     * 
     * @param clause
     *            erlang function clause
     */
    public static void highlightSelection(final IErlFunctionClause clause) {
        int offset, length;
        offset = clause.getNameRange().getOffset();
        length = clause.getNameRange().getLength();
        final IErlModule module = clause.getModule();
        final IEditorPart editor = openFile((IFile) module.getResource());
        highlightSelection(offset, length, (ITextEditor) editor);

    }

    /**
     * Opens the given file with the Eclipse default editor
     * 
     * @param file
     *            file
     * @return the opened editor
     */
    static public IEditorPart openFile(final IFile file) {
        final IWorkbenchPage page = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage();
        /*
         * IEditorDescriptor desc =
         * PlatformUI.getWorkbench().getEditorRegistry()
         * .getDefaultEditor(file.getName());
         */

        try {
            return IDE.openEditor(page, file);
            // return page.openEditor(new FileEditorInput(file), desc.getId());
        } catch (final PartInitException e) {
            e.printStackTrace();
        }
        return null;

    }

    /**
     * Returns the actual editor's document
     * 
     * @return document in the current editor
     */
    static public IDocument getDocument() {
        final IWorkbenchPage page = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage();
        final IEditorPart part = page.getActiveEditor();
        final ITextEditor editor = (ITextEditor) part;
        final IDocumentProvider dp = editor.getDocumentProvider();
        final IDocument doc = dp.getDocument(editor.getEditorInput());
        return doc;
    }

    /**
     * Gets the document which belongs to a file
     * 
     * @param file
     *            corresponding file
     * @return document
     */
    static public IDocument getDocument(final IFile file) {
        return new Document(getFileContent(file));
    }

    /**
     * Returns the corresponding IDocument object
     * 
     * @param editor
     *            editor which has the document
     * @return document is conatained by the editor
     */
    static public IDocument getDocument(final ITextEditor editor) {
        final IFileEditorInput input = (IFileEditorInput) editor
                .getEditorInput();
        final IDocument doc = editor.getDocumentProvider().getDocument(input);

        return doc;
    }

    static private String getFileContent(final IFile file) {
        try {
            final InputStream in = file.getContents();
            final ByteArrayOutputStream out = new ByteArrayOutputStream();
            try {
                final byte[] buf = new byte[1024];
                int read = in.read(buf);
                while (read > 0) {
                    out.write(buf, 0, read);
                    read = in.read(buf);
                }
                return out.toString();
            } finally {
                in.close();
                out.close();
            }
        } catch (final CoreException e) {
            e.printStackTrace();
        } catch (final IOException e) {
            e.printStackTrace();
        }
        return "";
    }

    /**
     * Gets IFile object from path string
     * 
     * @param pathString
     *            file path
     * @return IFile object
     */
    static public IFile getFileFromPath(final String pathString) {
        final Path path = new Path(pathString);
        return getFileFromPath(path);
    }

    /**
     * Gets an IFile object from Path
     * 
     * @param path
     *            file path
     * @return IFile object
     */
    public static IFile getFileFromPath(final IPath path) {
        final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
        final IFile[] files = // root.findFilesForLocation(path);
        root.findFilesForLocationURI(org.eclipse.core.filesystem.URIUtil
                .toURI(path));

        if (files.length > 0) {
            return files[0];// else
        } else {
            return root.getFile(path);
            /*
             * if (file != null) return file;
             */
            /*
             * else throw new WranglerException("File not found!");
             */
        }
    }

    /**
     * Notifies Erlide about the changed files.
     * 
     * @param changedFiles
     *            changed files
     */
    public static void notifyErlide(final ArrayList<ChangedFile> changedFiles) {

        final IErlModel model = ErlModelManager.getErlangModel();
        for (final ChangedFile f : changedFiles) {
            IFile file;
            try {
                file = getFileFromPath(f.getNewPath());
                final IErlElement element = model.findElement(file);
                final IErlModule m = (IErlModule) element;
                m.resourceChanged(null);
                final IEditorPart editor = GlobalParameters.getEditor();
                if (editor instanceof ErlangEditor) {
                    ((ErlangEditor) editor).resetAndCacheScannerAndParser();
                }
                model.notifyChange(m);

            } catch (final Exception e) {
                e.printStackTrace();
            }
        }

    }
}
