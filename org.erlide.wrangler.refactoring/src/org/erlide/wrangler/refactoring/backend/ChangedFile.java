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
package org.erlide.wrangler.refactoring.backend;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.TextFileChange;
import org.eclipse.text.edits.MultiTextEdit;
import org.eclipse.text.edits.TextEdit;
import org.erlide.wrangler.refactoring.util.ChangesetMaker;

/**
 * Represents an Erlang file, and a string which contains the modified source.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class ChangedFile {

    /**
     * Represented Erlang file OS dependent path before the refactoring.
     */
    String oldPath;

    /**
     * Represented Erlang file OS dependent path after the refactoring.
     */
    String newPath;

    /**
     * Modified source of <code>oldPath</code>.
     */
    String newFileContent;

    /**
     * Constructs a new object.
     * 
     * @param oldPath
     *            Old path of the file.
     * @param newPath
     *            New path of the file.
     * @param newFileContent
     *            New content of the source file.
     */
    public ChangedFile(final String oldPath, final String newPath,
            final String newFileContent) {
        this.oldPath = oldPath;
        this.newPath = newPath;
        this.newFileContent = newFileContent;
    }

    /**
     * Creates <code>Change</code> objects from the original and the modified
     * source.
     * 
     * @return the created <code>Change</code> object which typically instance
     *         of <code>TextFileChange</code>
     * @throws IOException
     *             if an exception occurs while accessing the source file
     */
    public Change createChanges(final IFile file) throws IOException {
        IFile eclipseRep;
        if (file == null) {
            eclipseRep = findEclipseRepresentation(oldPath);
        } else {
            eclipseRep = file;
        }

        final TextFileChange change = new TextFileChange(oldPath, eclipseRep);
        // change.setSaveMode(TextFileChange.FORCE_SAVE);
        final File tf = new File(oldPath);
        final ArrayList<TextEdit> edits = ChangesetMaker.createEdits(tf,
                newFileContent);
        final MultiTextEdit multiEdit = new MultiTextEdit();
        if (edits.size() != 0) {
            for (final TextEdit edit : edits) {
                multiEdit.addChild(edit);
            }
            change.setEdit(multiEdit);
            return change;
        } else {
            return null;
        }
    }

    public Change createChanges() throws IOException {
        return createChanges(null);

    }

    /**
     * Finds the Eclipse representation of the given path.
     * 
     * @param anOldPath
     *            OS dependent path of the refactored file
     * @return an <code>IFile</code> object of the given path
     * @throws IOException
     *             if the given path could not be found on the workspace
     */
    private IFile findEclipseRepresentation(final String anOldPath)
            throws IOException {
        final IWorkspace workspace = ResourcesPlugin.getWorkspace();
        final IWorkspaceRoot root = workspace.getRoot();
        final Path p = new Path(anOldPath);
        @SuppressWarnings("deprecation")
        final IFile[] files = root.findFilesForLocation(p);
        if (files == null || files.length != 1) {
            throw new IOException("File not found");
        }

        return files[0];

    }

    /**
     * True if the file name is changed during the refactoring.
     * 
     * @return true if the file name has changed
     */
    public boolean isNameChanged() {
        return !newPath.equals(oldPath);
    }

    /**
     * Returns the IPath object of the old path.
     * 
     * @return IPath object
     */
    public IPath getPath() {
        IFile f;
        try {
            f = findEclipseRepresentation(oldPath);
        } catch (final IOException e) {
            return null;
        }
        return f.getFullPath();
    }

    /**
     * Returns the IPath object of the new paths
     * 
     * @return IPath object
     */
    public IPath getNewPath() {
        IFile f;
        try {
            f = findEclipseRepresentation(newPath);
        } catch (final IOException e) {
            return null;
        }
        return f.getFullPath();
    }

    /**
     * If the refactoring changes the file name, it returns back the new one.
     * 
     * @return new file name
     */
    public String getNewName() {
        return Path.fromOSString(newPath).toFile().getName();
        // return newPath;
    }
}
