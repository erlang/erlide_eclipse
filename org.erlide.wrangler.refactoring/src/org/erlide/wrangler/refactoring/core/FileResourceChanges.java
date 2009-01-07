package org.erlide.wrangler.refactoring.core;

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
import org.erlide.wrangler.refactoring.util.ComparerTool;

/**
 * Represents an Erlang file, and a string which contains the modified source.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class FileResourceChanges {

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
	public FileResourceChanges(String oldPath, String newPath,
			String newFileContent) {
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
	public Change createChanges() throws IOException {
		IFile eclipseRep = findEclipseRepresentation(oldPath);

		TextFileChange change = new TextFileChange(newPath, eclipseRep);
		File tf = new File(oldPath);
		ArrayList<TextEdit> edits = ComparerTool
				.createEdits(tf, newFileContent);
		MultiTextEdit multiEdit = new MultiTextEdit();
		if (edits.size() != 0) {
			for (TextEdit edit : edits) {
				multiEdit.addChild(edit);
			}
			change.setEdit(multiEdit);
			return change;
		} else {
			return null;
		}
	}

	/**
	 * Finds the Eclipse representation of the given path.
	 * 
	 * @param oldPath
	 *            OS dependent path of the refactored file
	 * @return an <code>IFile</code> object of the given path
	 * @throws IOException
	 *             if the given path could not be found on the workspace
	 */
	private IFile findEclipseRepresentation(String oldPath) throws IOException {
		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		IWorkspaceRoot root = workspace.getRoot();
		Path p = new Path(oldPath);
		IFile[] files = root.findFilesForLocation(p);
		if (files == null || files.length != 1) {
			throw new IOException("File not found");
		}

		return files[0];

		/*
		 * for (IResource actRes : root.members()) { if (actRes.getType() ==
		 * IResource.FILE && actRes.getLocation().toOSString() == oldPath) {
		 * return (IFile) actRes; } }
		 * 
		 * return null;
		 */

	}

	/**
	 * True if the file name is changed during the refacrtoring.
	 * 
	 * @return
	 */
	public boolean isNameChanged() {
		return newPath != oldPath;
	}

	/**
	 * Returns the IPath object of the old path.
	 * 
	 * @return
	 */
	public IPath getIPath() {
		IFile f;
		try {
			f = findEclipseRepresentation(oldPath);
		} catch (IOException e) {
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
