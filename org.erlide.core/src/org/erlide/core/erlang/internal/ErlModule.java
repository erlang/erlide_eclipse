/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.erlang.internal;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.basiccore.ErlLogger;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlScanner;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlAttribute;
import org.erlide.core.erlang.IErlComment;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlFunctionClause;
import org.erlide.core.erlang.IErlImport;
import org.erlide.core.erlang.IErlMember;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlPreprocessorDef;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IErlScanner;
import org.erlide.core.erlang.ISourceRange;
import org.erlide.core.erlang.ISourceReference;
import org.erlide.core.erlang.util.Util;
import org.erlide.core.util.ErlangFunction;
import org.erlide.core.util.ErlangIncludeFile;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

public class ErlModule extends Openable implements IErlModule {

	private long timestamp;

	private OtpErlangObject parseTree;

	private final boolean isModule;

	// the document last reconciled with
	// private IDocument fDoc;
	private final List<IErlComment> comments;

	private final IErlScanner fScanner;

	private final IFile fFile;

	// These are needed to ignore the initial INSERT of all text and final
	// DELETE of all text
	private boolean fIgnoreNextReconcile = false;

	protected ErlModule(IErlElement parent, String name, boolean isErl,
			IFile file) {
		super(parent, name);
		fFile = file;
		if (ErlModelManager.verbose) {
			ErlLogger.debug("...creating " + parent.getElementName() + "/"
					+ name + " " + isErl);
		}
		isModule = isErl;
		comments = new ArrayList<IErlComment>(0);
		String initialText;
		try {
			initialText = new String(Util.getResourceContentsAsCharArray(file));
		} catch (final ErlModelException e) {
			initialText = "";
		}
		fScanner = new ErlScanner(this, initialText);
		try {
			open(null);
		} catch (final ErlModelException e) {
			// not much to do
		}
	}

	protected ErlModule(IErlElement parent, String name, String text,
			boolean isErl, IFile file) {
		this(parent, name, isErl, file);
	}

	@Override
	protected boolean buildStructure(IProgressMonitor pm,
			IResource underlyingResource) throws ErlModelException {

		// generate structure and compute syntax problems if needed
		final IErlProject project = getErlProject();
		boolean computeProblems = ErlangCore.hasErlangNature(project
				.getProject());

		final Map<String, String> options = project.getOptions(true);
		if (!computeProblems) {
			// disable task tags checking to speed up parsing
			options.put(ErlangCore.COMPILER_TASK_TAGS, ""); //$NON-NLS-1$
		}

		// ErlLogger.debug("* build structure " + this.fName);
		// PUT SOMEWHERE ELSE! getScanner().modifyText(doc, dirtyRegion);

		final ErlParser parser = new ErlParser();
		isStructureKnown = parser.parse(this);
		final IErlModel model = getModel();
		if (model != null) {
			model.notifyChange(this);
		}

		// update timestamp (might be IResource.NULL_STAMP if original does not
		// exist)
		if (underlyingResource == null) {
			underlyingResource = getResource();
		}
		if (underlyingResource != null) {
			timestamp = ((IFile) underlyingResource).getLocalTimeStamp();
		} else {
			timestamp = IResource.NULL_STAMP;
		}

		return isStructureKnown();
	}

	public IErlElement getElementAt(int position) throws ErlModelException {
		for (final IErlElement child : fChildren) {
			if (child instanceof IErlFunction) {
				final IErlFunction f = (IErlFunction) child;
				final IErlFunctionClause[] clauses = f.getClauses();
				if (clauses.length == 1
						&& f.getSourceRange().hasPosition(position)) {
					return f;
				}
				for (final IErlFunctionClause c : clauses) {
					if (c.getSourceRange().hasPosition(position)) {
						return c;
					}
				}
			} else {
				final ISourceReference ch = (ISourceReference) child;
				final ISourceRange r = ch.getSourceRange();
				if (r != null && r.hasPosition(position)) {
					return child;
				}
			}
		}
		return null;
	}

	public boolean hasResourceChanged() {
		// TODO Auto-generated method stub
		return false;
	}

	public ErlElementType getElementType() {
		return ErlElementType.MODULE;
	}

	public IResource getResource() {
		// final IResource parentRes = this.getParent().getResource();
		// if (parentRes == null || !(parentRes instanceof IContainer)) {
		// return null;
		// }
		// return ((IContainer) parentRes).getFile(new Path(getElementName()));
		return fFile;
	}

	public String getSource() throws ErlModelException {
		return ""; // return getBuffer().getContents();
	}

	public ISourceRange getSourceRange() throws ErlModelException {
		return new SourceRange(0, 0);
	}

	public void copy(IErlElement container, IErlElement sibling, String rename,
			boolean replace, IProgressMonitor monitor) throws ErlModelException {
		// TODO Auto-generated method stub

	}

	public void delete(boolean force, IProgressMonitor monitor)
			throws ErlModelException {
		// TODO Auto-generated method stub

	}

	public void move(IErlElement container, IErlElement sibling, String rename,
			boolean replace, IProgressMonitor monitor) throws ErlModelException {
		// TODO Auto-generated method stub

	}

	public void rename(String aname, boolean replace, IProgressMonitor monitor)
			throws ErlModelException {
		// TODO Auto-generated method stub

	}

	@Override
	protected boolean hasBuffer() {
		return true;
	}

	// @Override
	// protected IBuffer openBuffer(IProgressMonitor pm, Object info) {
	// final IBuffer b = BufferManager.getDefaultBufferManager().createBuffer(
	// this);
	// try {
	// final IFile f = (IFile) getUnderlyingResource();
	// b.setContents(Util.getResourceContentsAsCharArray(f));
	// b.addBufferChangedListener(this);
	// } catch (final ErlModelException e) {
	// // e.printStackTrace();
	// }
	// return b;
	// }

	public void addMember(IErlMember elem) {
		addChild(elem);
	}

	public void addComment(IErlComment c) {
		comments.add(c);
	}

	public void reset() {
		fChildren.clear();
		comments.clear();
		isStructureKnown = false;
	}

	public OtpErlangObject getParseTree() {
		return parseTree;
	}

	public void setParseTree(OtpErlangList forms) {
		parseTree = forms;
	}

	public boolean isModule() {
		return isModule;
	}

	public long getTimestamp() {
		return timestamp;
	}

	public IErlComment[] getComments() {
		return comments.toArray(new IErlComment[comments.size()]);
	}

	public IErlImport findImport(ErlangFunction function) {
		for (final IErlElement m : fChildren) {
			if (m instanceof IErlImport) {
				final IErlImport ei = (IErlImport) m;
				if (ei.hasImported(function)) {
					return ei;
				}
			}
		}
		return null;
	}

	public IErlPreprocessorDef findPreprocessorDef(String definedName,
			ErlElementType type) {
		for (final IErlElement m : fChildren) {
			if (m instanceof IErlPreprocessorDef) {
				final IErlPreprocessorDef pd = (IErlPreprocessorDef) m;
				if (pd.getElementType().equals(type)
						&& pd.getDefinedName().equals(definedName)) {
					return pd;
				}
			}
		}
		return null;
	}

	public ErlangIncludeFile[] getIncludedFiles() {
		final List<ErlangIncludeFile> r = new ArrayList<ErlangIncludeFile>(0);
		for (final IErlElement m : fChildren) {
			if (m instanceof IErlAttribute) {
				final IErlAttribute a = (IErlAttribute) m;
				final OtpErlangObject v = a.getValue();
				if (v instanceof OtpErlangString) {
					final String s = ((OtpErlangString) v).stringValue();
					if ("include".equals(a.getElementName())) {
						r.add(new ErlangIncludeFile(false, s));
					} else if ("include_lib".equals(a.getElementName())) {
						r.add(new ErlangIncludeFile(true, s));
					}
				}
			}
		}
		return r.toArray(new ErlangIncludeFile[r.size()]);
	}

	public IErlImport[] getImports() {
		final List<IErlImport> r = new ArrayList<IErlImport>();
		for (final IErlElement m : fChildren) {
			if (m instanceof IErlImport) {
				r.add((IErlImport) m);
			}
		}
		return r.toArray(new IErlImport[r.size()]);
	}

	public IErlScanner getScanner() {
		// if (fScanner == null) {
		// fScanner = new ErlScanner(this);
		// // FIXxcME: få tag på texten på annat sätt! fScanner.insertText(0,
		// // getBuffer().getContents());
		// }
		return fScanner;
	}

	public void fixExportedFunctions() {
		// final List<ErlangFunction> exports = new
		// ArrayList<ErlangFunction>(10);
		// for (final IErlElement m : fChildren) {
		// if (m instanceof IErlExport) {
		// // final OtpErlangList l = (OtpErlangList) ((IErlExport) m)
		// // .getParseTree();
		// // for (int j = 0; j < l.arity(); ++j) {
		// // TODO removed temporarily
		// // try {
		// // exports.add(new ErlangFunction((OtpErlangTuple)
		// // l.elementAt(j)));
		// // } catch (OtpErlangRangeException e) {
		// // e.printStackTrace();
		// // }
		// // }
		// }
		// }
		// for (final IErlElement m : fChildren) {
		// if (m instanceof ErlFunction) {
		// final ErlFunction f = (ErlFunction) m;
		// f.setExported(exports.contains(f.getFunction()));
		// }
		// }
	}

	public int getLineEnd() {
		// TODO Auto-generated method stub
		return 0;
	}

	public int getLineStart() {
		// TODO Auto-generated method stub
		return 0;
	}

	public void reconcileText(int offset, int removeLength, String newText,
			IProgressMonitor mon) {
		// getScanner();
		ErlLogger.debug("reconcileText " + offset + ":" + removeLength + ":"
				+ newText.length() + " ign " + fIgnoreNextReconcile);
		if (fIgnoreNextReconcile) {
			fIgnoreNextReconcile = false;
			return;
		}
		if (removeLength != 0) {
			fScanner.removeText(offset, removeLength);
		}
		if (newText.length() != 0) {
			fScanner.insertText(offset, newText);
		}
		setIsStructureKnown(false);
		try {
			open(mon);
		} catch (final ErlModelException e) {
			// not much to do
		}
	}

	@Override
	protected void closing(Object info) throws ErlModelException {
		// TODO Auto-generated method stub

	}

	public void finalReconcile() {
		fIgnoreNextReconcile = true;
	}

	public void initialReconcile() {
		fIgnoreNextReconcile = true;
	}

}
