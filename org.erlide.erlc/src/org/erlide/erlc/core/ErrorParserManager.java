/*******************************************************************************
 * Copyright (c) 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.erlc.core;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceProxy;
import org.eclipse.core.resources.IResourceProxyVisitor;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.IMarkerGenerator;
import org.erlide.erlc.ErlideErlcPlugin;
import org.erlide.erlc.builder.ErlcMakeBuilder;
import org.erlide.erlc.errorparsing.IErrorParser;

public class ErrorParserManager extends OutputStream {

	public final static String PREF_ERROR_PARSER = ErlangPlugin.PLUGIN_ID
			+ ".errorOutputParser"; //$NON-NLS-1$

	private int nOpens;

	private IProject fProject;

	private IMarkerGenerator fMarkerGenerator;

	private Map<String, IFile> fFilesInProject;

	private List<String> fNameConflicts;

	private Map<String, IErrorParser[]> fErrorParsers;

	private ArrayList<Problem> fErrors;

	private Vector<IPath> fDirectoryStack;

	private IPath fBaseDirectory;

	// private String previousLine;
	private OutputStream outputStream;

	private final StringBuffer currentLines = new StringBuffer();

	private final StringBuffer scratchBuffer = new StringBuffer();

	private boolean hasErrors = false;

	public ErrorParserManager(ErlcMakeBuilder builder) {
		this(builder.getProject(), builder);
	}

	public ErrorParserManager(IProject project, IMarkerGenerator markerGenerator) {
		this(project, markerGenerator, null);
	}

	public ErrorParserManager(IProject project,
			IMarkerGenerator markerGenerator, String[] parsersIDs) {
		this(project, project.getLocation(), markerGenerator, parsersIDs);
	}

	public ErrorParserManager(IProject project, IPath workingDirectory,
			IMarkerGenerator markerGenerator, String[] parsersIDs) {
		fProject = project;
		if (parsersIDs == null) {
			enableAllParsers();
		} else {
			fErrorParsers = new LinkedHashMap<String, IErrorParser[]>(
					parsersIDs.length);
			for (int i = 0; i < parsersIDs.length; i++) {
				final IErrorParser[] parsers = ErlideErlcPlugin.getDefault()
						.getErrorParser(parsersIDs[i]);
				fErrorParsers.put(parsersIDs[i], parsers);
			}
		}
		fMarkerGenerator = markerGenerator;
		initErrorParserManager(workingDirectory);
	}

	private void initErrorParserManager(IPath workingDirectory) {
		fFilesInProject = new HashMap<String, IFile>();
		fNameConflicts = new ArrayList<String>();
		fDirectoryStack = new Vector<IPath>();
		fErrors = new ArrayList<Problem>();

		final List<IFile> collectedFiles = new ArrayList<IFile>();
		fBaseDirectory = (workingDirectory == null || workingDirectory
				.isEmpty()) ? fProject.getLocation() : workingDirectory;
		collectFiles(fProject, collectedFiles);

		for (int i = 0; i < collectedFiles.size(); i++) {
			final IFile file = collectedFiles.get(i);
			final Object existing = fFilesInProject.put(file.getName(), file);
			if (existing != null) {
				fNameConflicts.add(file.getName());
			}
		}
	}

	public IPath getWorkingDirectory() {
		if (fDirectoryStack.size() != 0) {
			return (IPath) fDirectoryStack.lastElement();
		}
		// Fallback to the Project Location
		return fBaseDirectory;
	}

	public void pushDirectory(IPath dir) {
		if (dir != null) {
			IPath pwd = null;
			if (fBaseDirectory.isPrefixOf(dir)) {
				final int segments = fBaseDirectory.matchingFirstSegments(dir);
				pwd = dir.removeFirstSegments(segments);
			} else {
				pwd = dir;
			}
			fDirectoryStack.addElement(pwd);
		}
	}

	public IPath popDirectory() {
		final int i = fDirectoryStack.size();
		if (i != 0) {
			final IPath dir = (IPath) fDirectoryStack.lastElement();
			fDirectoryStack.removeElementAt(i - 1);
			return dir;
		}
		return new Path(""); //$NON-NLS-1$
	}

	public int getDirectoryLevel() {
		return fDirectoryStack.size();
	}

	private void enableAllParsers() {
		fErrorParsers = new LinkedHashMap<String, IErrorParser[]>();
		final String[] parserIDs = ErlideErlcPlugin.getDefault()
				.getAllErrorParsersIDs();
		for (int i = 0; i < parserIDs.length; i++) {
			final IErrorParser[] parsers = ErlideErlcPlugin.getDefault()
					.getErrorParser(parserIDs[i]);
			fErrorParsers.put(parserIDs[i], parsers);
		}
		if (fErrorParsers.size() == 0) {
			initErrorParsersMap();
		}
	}

	private void initErrorParsersMap() {
		final String[] parserIDs = ErlideErlcPlugin.getDefault()
				.getAllErrorParsersIDs();
		for (int i = 0; i < parserIDs.length; i++) {
			final IErrorParser[] parsers = ErlideErlcPlugin.getDefault()
					.getErrorParser(parserIDs[i]);
			fErrorParsers.put(parserIDs[i], parsers);
		}
	}

	protected void collectFiles(IProject parent, final List<IFile> result) {
		try {
			parent.accept(new IResourceProxyVisitor() {

				public boolean visit(IResourceProxy proxy) throws CoreException {
					if (proxy.getType() == IResource.FILE) {
						result.add((IFile) proxy.requestResource());
						return false;
					}
					return true;
				}
			}, IResource.NONE);
		} catch (final CoreException e) {
			ErlangPlugin.log(e.getStatus());
		}
	}

	/**
	 * Parses the input and try to generate error or warning markers
	 */
	private void processLines(String line) {
		if (fErrorParsers.size() == 0) {
			return;
		}

		final String[] parserIDs = new String[fErrorParsers.size()];
		final Iterator items = fErrorParsers.keySet().iterator();
		for (int i = 0; items.hasNext(); i++) {
			parserIDs[i] = (String) items.next();
		}

		for (int i = 0; i < parserIDs.length; ++i) {
			final IErrorParser[] parsers = (IErrorParser[]) fErrorParsers
					.get(parserIDs[i]);
			for (int j = 0; j < parsers.length; j++) {
				final IErrorParser curr = parsers[j];
				if (curr.processLines(line, this)) {
					return;
				}
			}
		}

		// This old way of doing was trouble because it did not
		// respect the ordering provide by the users.
		//
		// int top = parserIDs.length - 1;
		// int i = top;
		// do {
		// IErrorParser[] parsers = (IErrorParser[])
		// fErrorParsers.get(parserIDs[i]);
		// for (int j = 0; j < parsers.length; j++) {
		// IErrorParser curr = parsers[j];
		// if (curr.processLine(line, this)) {
		// if (i != top) {
		// // move to top
		// Object used = fErrorParsers.remove(parserIDs[i]);
		// fErrorParsers.put(parserIDs[i], used);
		// //savePreferences();
		// }
		// return;
		// }
		// }
		// i--;
		// } while (i >= 0);
	}

	/**
	 * Called by the error parsers.
	 */
	public IFile findFileName(String fileName) {
		final IPath path = new Path(fileName);
		return (IFile) fFilesInProject.get(path.lastSegment());
	}

	protected IFile findFileInWorkspace(IPath path) {
		IFile file = null;
		if (path.isAbsolute()) {
			final IWorkspaceRoot root = fProject.getWorkspace().getRoot();
			file = root.getFileForLocation(path);
			// It may be a link resource so we must check it also.
			if (file == null) {
				final IFile[] files = root.findFilesForLocation(path);
				for (int i = 0; i < files.length; i++) {
					if (files[i].getProject().equals(fProject)) {
						file = files[i];
						break;
					}
				}
			}

		} else {
			file = fProject.getFile(path);
		}
		return file;
	}

	/**
	 * Called by the error parsers.
	 */
	public boolean isConflictingName(String fileName) {
		final IPath path = new Path(fileName);
		return fNameConflicts.contains(path.lastSegment());
	}

	/**
	 * Called by the error parsers.
	 */
	public IFile findFilePath(String filePath) {
		IPath path = null;
		final IPath fp = new Path(filePath);
		if (fp.isAbsolute()) {
			if (fBaseDirectory.isPrefixOf(fp)) {
				final int segments = fBaseDirectory.matchingFirstSegments(fp);
				path = fp.removeFirstSegments(segments);
			} else {
				path = fp;
			}
		} else {
			path = getWorkingDirectory().append(filePath);
		}

		IFile file = null;
		// The workspace may throw an IllegalArgumentException
		// Catch it and the parser should fallback to scan the entire project.
		try {
			file = findFileInWorkspace(path);
		} catch (final Exception e) {
		}

		// We have to do another try, on Windows for cases like "TEST.C" vs
		// "test.c"
		// We use the java.io.File canonical path.
		if (file == null || !file.exists()) {
			final File f = path.toFile();
			try {
				final String canon = f.getCanonicalPath();
				path = new Path(canon);
				file = findFileInWorkspace(path);
			} catch (final IOException e1) {
			}
		}
		return (file != null && file.exists()) ? file : null;
	}

	protected class Problem {

		protected IResource file;

		protected int lineNumber;

		protected String description;

		protected int severity;

		protected String variableName;

		public Problem(IResource file, int lineNumber, String desciption,
				int severity, String variableName) {
			this.file = file;
			this.lineNumber = lineNumber;
			this.description = desciption;
			this.severity = severity;
			this.variableName = variableName;
		}
	}

	/**
	 * Called by the error parsers.
	 */
	public void generateMarker(IResource file, int lineNumber, String desc,
			int severity, String varName) {
		final Problem problem = new Problem(file, lineNumber, desc, severity,
				varName);
		fErrors.add(problem);

		if (severity == IMarker.SEVERITY_ERROR) {
			hasErrors = true;
		}
	}

	static class MarkerGenerator implements IMarkerGenerator {

		ErrorParserManager fErrorParserManager;

		MarkerGenerator(ErrorParserManager epm) {
			fErrorParserManager = epm;
		}

		public void addMarker(IResource file, String errorDesc, int lineNumber,
				int severity, String errorVar) {
			fErrorParserManager.generateMarker(file, lineNumber, errorDesc,
					severity, errorVar);
		}

	}

	public IMarkerGenerator getMarkerGenerator() {
		return new MarkerGenerator(this);
	}

	// /**
	// * Called by the error parsers. Return the previous line, save in the
	// working
	// buffer.
	// */
	// public String getPreviousLine() {
	// return new String((previousLine) == null ? "" : previousLine);
	// //$NON-NLS-1$
	// }

	/**
	 * Method setOutputStream.
	 * 
	 * @param cos
	 */
	public void setOutputStream(OutputStream os) {
		outputStream = os;
	}

	/**
	 * Method getOutputStream. It has a reference count the stream must be close
	 * the same number of time this method was call.
	 * 
	 * @return OutputStream
	 */
	public OutputStream getOutputStream() {
		nOpens++;
		return this;
	}

	/**
	 * @see java.io.OutputStream#close()
	 */
	@Override
	public void close() throws IOException {
		if (nOpens > 0 && --nOpens == 0) {
			checkLines(true);
			fDirectoryStack.removeAllElements();
			fBaseDirectory = null;
			if (outputStream != null) {
				outputStream.close();
			}
		}
	}

	/**
	 * @see java.io.OutputStream#flush()
	 */
	@Override
	public void flush() throws IOException {
		if (outputStream != null) {
			outputStream.flush();
		}
	}

	/**
	 * @see java.io.OutputStream#write(int)
	 */
	@Override
	public synchronized void write(int b) throws IOException {
		currentLines.append((char) b);
		checkLines(false);
		if (outputStream != null) {
			outputStream.write(b);
		}
	}

	@Override
	public synchronized void write(byte[] b, int off, int len)
			throws IOException {
		if (b == null) {
			throw new NullPointerException();
		} else if (off != 0 || (len < 0) || (len > b.length)) {
			throw new IndexOutOfBoundsException();
		} else if (len == 0) {
			return;
		}
		currentLines.append(new String(b, 0, len));
		checkLines(false);
		if (outputStream != null) {
			outputStream.write(b, off, len);
		}
	}

	private void checkLines(boolean flush) {
		if (!flush) {
			return;
		}
		// TODO JC we don't care about background processing, just wait for
		// erlc to do it's job to an end... maybe we should be smarter: allowing
		// for chunks w several lines to be processed, but, what the heck
		processLines(currentLines.toString());
		currentLines.setLength(0);
		// while ((i = buffer.indexOf('\n')) != -1) {
		// String line = buffer.substring(0, i).trim(); // get rid of any
		// trailing \r
		// processLine(line);
		// previousLine = line;
		// buffer = buffer.substring(i + 1); // skip the \n and advance
		// }
		// currentLine.setLength(0);
		// if (flush) {
		// if (buffer.length() > 0) {
		// processLine(buffer);
		// previousLine = buffer;
		// }
		// } else {
		// currentLine.append(buffer);
		// }
	}

	public boolean reportProblems() {
		boolean reset = false;
		if (nOpens == 0) {
			final Iterator iter = fErrors.iterator();
			while (iter.hasNext()) {
				final Problem problem = (Problem) iter.next();
				if (problem.severity == IMarker.SEVERITY_ERROR) {
					reset = true;
				}
				if (problem.file == null) {
					fMarkerGenerator.addMarker(fProject, problem.description,
							problem.lineNumber, problem.severity,
							problem.variableName);
				} else {
					fMarkerGenerator.addMarker(problem.file,
							problem.description, problem.lineNumber,
							problem.severity, problem.variableName);
				}
			}
			fErrors.clear();
		}
		return reset;
	}

	/**
	 * 
	 */
	public String getScratchBuffer() {
		return scratchBuffer.toString();
	}

	/**
	 * @param line
	 */
	public void appendToScratchBuffer(String line) {
		scratchBuffer.append(line);
	}

	/**
	 * 
	 */
	public void clearScratchBuffer() {
		scratchBuffer.setLength(0);
	}

	public boolean hasErrors() {
		return hasErrors;
	}
}
