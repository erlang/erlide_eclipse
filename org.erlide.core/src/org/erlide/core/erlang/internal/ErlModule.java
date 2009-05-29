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
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlScanner;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlAttribute;
import org.erlide.core.erlang.IErlComment;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlExport;
import org.erlide.core.erlang.IErlImport;
import org.erlide.core.erlang.IErlMember;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlPreprocessorDef;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IErlScanner;
import org.erlide.core.erlang.IErlangFirstThat;
import org.erlide.core.erlang.ISourceRange;
import org.erlide.core.erlang.ISourceReference;
import org.erlide.core.erlang.util.ErlangFunction;
import org.erlide.core.erlang.util.ErlangIncludeFile;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

import erlang.ErlideNoparse;

public class ErlModule extends Openable implements IErlModule {

	private long timestamp;
	private final List<IErlComment> comments;
	private String initialText;
	private IErlScanner scanner;
	private final IFile fFile;
	private boolean parsed = false;

	// These are needed to ignore the initial INSERT of all text and final
	// DELETE of all text
	private boolean fIgnoreNextReconcile = false;
	private boolean fIgnoreNextPostReconcile = false;
	private boolean scannerDisposed = false;
	private final ModuleKind moduleKind;

	protected ErlModule(final IErlElement parent, final String name,
			final String initialText, final IFile file) {
		super(parent, name);
		fFile = file;
		moduleKind = ErlideUtil.nameToModuleKind(name);
		comments = new ArrayList<IErlComment>(0);
		scanner = null;
		this.initialText = initialText;
		if (ErlModelManager.verbose) {
			ErlLogger.debug("...creating " + parent.getName() + "/" + getName()
					+ " " + moduleKind);
		}
	}

	@Override
	protected synchronized boolean buildStructure(final IProgressMonitor pm,
			IResource underlyingResource) throws ErlModelException {
		logBuildStructure(underlyingResource);
		final String path = getFilePath();
		final String erlidePath = getErlidePath();
		if (scanner == null) {
			parsed = false;
		}
		final boolean initialParse = !parsed;
		final String text = initialParse ? initialText : "";
		parsed = ErlParser.parse(this, text, initialParse, path, erlidePath);
		final IErlModel model = getModel();
		if (model != null) {
			model.notifyChange(this);
		}

		// update timestamp (might be IResource.NULL_STAMP if original does not
		// exist)
		if (underlyingResource == null) {
			underlyingResource = getResource();
		}
		if (underlyingResource != null && underlyingResource instanceof IFile) {
			timestamp = ((IFile) underlyingResource).getLocalTimeStamp();
		} else {
			timestamp = IResource.NULL_STAMP;
		}
		return parsed;
	}

	protected String getErlidePath() {
		return fFile.getFullPath().toString();
	}

	/**
	 * @param underlyingResource
	 * @return
	 */
	protected String getFilePath() {
		final IPath location = fFile.getLocation();
		if (location == null) {
			return null;
		}
		return location.toString();
	}

	// public IErlElement getElementAt(final int position)
	// throws ErlModelException {
	// for (final IErlElement child : fChildren) {
	// if (child instanceof IErlFunction) {
	// final IErlFunction f = (IErlFunction) child;
	// final List<IErlFunctionClause> clauses = f.getClauses();
	// if (clauses.size() <= 1
	// && f.getSourceRange().hasPosition(position)) {
	// return f;
	// }
	// for (final IErlFunctionClause c : clauses) {
	// if (c.getSourceRange().hasPosition(position)) {
	// return c;
	// }
	// }
	// } else {
	// final ISourceReference ch = (ISourceReference) child;
	// final ISourceRange r = ch.getSourceRange();
	// if (r != null && r.hasPosition(position)) {
	// return child;
	// }
	// }
	// }
	// return null;
	// }

	public IErlElement getElementAt(final int position)
			throws ErlModelException {
		return getModel().innermostThat(this, new IErlangFirstThat() {
			public boolean firstThat(final IErlElement e) {
				if (e instanceof ISourceReference) {
					final ISourceReference ch = (ISourceReference) e;
					ISourceRange r;
					try {
						r = ch.getSourceRange();
						if (r != null && r.hasPosition(position)) {
							return true;
						}
					} catch (final ErlModelException e1) {
					}
				}
				return false;
			}
		});
	}

	public IErlElement getElementAtLine(final int lineNumber) {
		return getModel().innermostThat(this, new IErlangFirstThat() {
			public boolean firstThat(final IErlElement e) {
				if (e instanceof ISourceReference) {
					final ISourceReference sr = (ISourceReference) e;
					if (sr.getLineStart() <= lineNumber
							&& sr.getLineEnd() >= lineNumber) {
						return true;
					}
				}
				return false;
			}
		});
	}

	// public IErlElement getElementAtLine(final int lineNumber) {
	// for (final IErlElement child : fChildren) {
	// if (child instanceof ISourceReference) {
	// final ISourceReference sr = (ISourceReference) child;
	// if (sr.getLineStart() <= lineNumber
	// && sr.getLineEnd() >= lineNumber) {
	// return child;
	// }
	// }
	// if (child instanceof IParent) {
	// final IParent p = (IParent) child;
	// try {
	// for (final IErlElement e : p.getChildren()) {
	// if (e instanceof ISourceReference) {
	// final ISourceReference sr = (ISourceReference) e;
	// if (sr.getLineStart() <= lineNumber
	// && sr.getLineEnd() >= lineNumber) {
	// return e;
	// }
	// }
	// }
	// } catch (final ErlModelException e) {
	// }
	// }
	// }
	// return null;
	// }

	public ModuleKind getModuleKind() {
		return moduleKind;
	}

	public IResource getResource() {
		// final IResource parentRes = this.getParent().getResource();
		// if (parentRes == null || !(parentRes instanceof IContainer)) {
		// return null;
		// }
		// return ((IContainer) parentRes).getFile(new Path(getElementName()));
		return fFile;
	}

	// public String getSource() throws ErlModelException {
	// return ""; // return getBuffer().getContents();
	// }

	public ISourceRange getSourceRange() throws ErlModelException {
		return new SourceRange(0, 0);
	}

	public void copy(final IErlElement container, final IErlElement sibling,
			final String rename, final boolean replace,
			final IProgressMonitor monitor) throws ErlModelException {
		// TODO Auto-generated method stub

	}

	public void delete(final boolean force, final IProgressMonitor monitor)
			throws ErlModelException {
		// TODO Auto-generated method stub

	}

	public void move(final IErlElement container, final IErlElement sibling,
			final String rename, final boolean replace,
			final IProgressMonitor monitor) throws ErlModelException {
		// TODO Auto-generated method stub

	}

	public void rename(final String aname, final boolean replace,
			final IProgressMonitor monitor) throws ErlModelException {
		// TODO Auto-generated method stub

	}

	@Override
	protected boolean hasBuffer() {
		return true;
	}

	public void addMember(final IErlMember elem) {
		addChild(elem);
	}

	public void addComment(final IErlComment c) {
		comments.add(c);
	}

	public void removeChildren() {
		fChildren.clear();
		comments.clear();
	}

	public synchronized long getTimestamp() {
		return timestamp;
	}

	public Collection<IErlComment> getComments() {
		return comments;
	}

	public IErlImport findImport(final ErlangFunction function) {
		for (final IErlElement m : fChildren) {
			if (m instanceof IErlImport) {
				final IErlImport ei = (IErlImport) m;
				if (ei.hasFunction(function)) {
					return ei;
				}
			}
		}
		return null;
	}

	public IErlExport findExport(final ErlangFunction function) {
		for (final IErlElement m : fChildren) {
			if (m instanceof IErlExport) {
				final IErlExport ei = (IErlExport) m;
				if (ei.hasFunction(function)) {
					return ei;
				}
			}
		}
		return null;
	}

	public IErlPreprocessorDef findPreprocessorDef(final String definedName,
			final Kind type) {
		for (final IErlElement m : fChildren) {
			if (m instanceof IErlPreprocessorDef) {
				final IErlPreprocessorDef pd = (IErlPreprocessorDef) m;
				if (pd.getKind().equals(type)
						&& pd.getDefinedName().equals(definedName)) {
					return pd;
				}
			}
		}
		return null;
	}

	public Collection<IErlPreprocessorDef> getPreprocessorDefs(final Kind type) {
		final List<IErlPreprocessorDef> res = new ArrayList<IErlPreprocessorDef>();
		for (final IErlElement m : fChildren) {
			if (m instanceof IErlPreprocessorDef) {
				final IErlPreprocessorDef pd = (IErlPreprocessorDef) m;
				if (pd.getKind().equals(type) || type.equals(Kind.ERROR)) {
					res.add(pd);
				}
			}
		}
		return res;
	}

	public Collection<ErlangIncludeFile> getIncludedFiles()
			throws ErlModelException {
		if (!isStructureKnown()) {
			open(null);
		}
		final List<ErlangIncludeFile> r = new ArrayList<ErlangIncludeFile>(0);
		for (final IErlElement m : fChildren) {
			if (m instanceof IErlAttribute) {
				final IErlAttribute a = (IErlAttribute) m;
				final OtpErlangObject v = a.getValue();
				if (v instanceof OtpErlangString) {
					final String s = ((OtpErlangString) v).stringValue();
					if ("include".equals(a.getName())) {
						r.add(new ErlangIncludeFile(false, s));
					} else if ("include_lib".equals(a.getName())) {
						r.add(new ErlangIncludeFile(true, s));
					}
				}
			}
		}
		return r;
	}

	public Collection<IErlImport> getImports() {
		final List<IErlImport> r = new ArrayList<IErlImport>();
		for (final IErlElement m : fChildren) {
			if (m instanceof IErlImport) {
				r.add((IErlImport) m);
			}
		}
		return r;
	}

	public IErlScanner getScanner() {
		if (scanner == null) {
			scanner = getNewScanner();
		}
		return scanner;
	}

	private IErlScanner getNewScanner() {
		final String path = getFilePath();
		final String erlidePath = getErlidePath();
		if (path == null || erlidePath == null) {
			return null;
		}
		return new ErlScanner(this, initialText, path, erlidePath);
	}

	public void fixExportedFunctions() {
		try {
			final List<? extends IErlElement> functions = getChildrenOfType(IErlElement.Kind.FUNCTION);
			for (final IErlElement erlElement : functions) {
				final ErlFunction f = (ErlFunction) erlElement;
				final boolean exported = findExport(f.getFunction()) != null;
				f.setExported(exported);
			}
		} catch (final ErlModelException e) {
		}
	}

	public void reconcileText(final int offset, final int removeLength,
			final String newText, final IProgressMonitor mon) {
		if (scannerDisposed) {
			return;
		}
		if (!fIgnoreNextReconcile) {
			getScanner();
			if (scanner != null) {
				scanner.replaceText(offset, removeLength, newText);
			}
			if (mon != null) {
				mon.worked(1);
			}
			setStructureKnown(false);
		}
		fIgnoreNextReconcile = false;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.erlide.core.erlang.IErlModule#postReconcile(org.eclipse.core.runtime
	 * .IProgressMonitor)
	 */
	public void postReconcile(final IProgressMonitor mon) {
		if (!fIgnoreNextPostReconcile) {
			try {
				open(mon);
			} catch (final ErlModelException e) {
				ErlLogger.warn(e);
			}
			if (mon != null) {
				mon.worked(1);
			}
		}
		fIgnoreNextPostReconcile = false;
	}

	@Override
	protected void closing(final Object info) throws ErlModelException {
		// TODO Auto-generated method stub

	}

	public void finalReconcile() {
		fIgnoreNextReconcile = true;
		fIgnoreNextPostReconcile = true;
	}

	public void initialReconcile() {
		fIgnoreNextReconcile = true;
		fIgnoreNextPostReconcile = true;
	}

	public String getModuleName() {
		return ErlideUtil.withoutExtension(getName());
	}

	public void disposeScanner() {
		// TODO use reference counting to know if there are any editors on the
		// module
		if (scanner == null) {
			return;
		}
		scanner.dispose();
		scanner = null;
		scannerDisposed = true;
		setStructureKnown(false);
	}

	public void disposeParser() {
		final Backend b = ErlangCore.getBackendManager().getIdeBackend();
		ErlideNoparse.destroy(b, getModuleName());
		setStructureKnown(false);
		parsed = false;
	}

	public void reenableScanner() {
		scannerDisposed = false;
		setStructureKnown(false);
	}

	public IErlProject getProject() {
		for (IErlElement p = this; p != null; p = p.getParent()) {
			if (p instanceof IErlProject) {
				return (IErlProject) p;
			}
		}
		return null;
	}

	public Kind getKind() {
		return Kind.MODULE;
	}

	public void dispose() {
	}

	public Set<IErlModule> getDirectDependents() throws ErlModelException {
		final Set<IErlModule> result = new HashSet<IErlModule>();
		final IErlProject project = getProject();
		for (final IErlModule m : project.getModules()) {
			System.out.println(m);
			final boolean wasOpen = m.isOpen();
			if (!wasOpen) {
				m.open(null);
			}
			final Collection<ErlangIncludeFile> incs = m.getIncludedFiles();
			for (final ErlangIncludeFile inc : incs) {
				if (inc.getFilename().equals(getName())) {
					result.add(m);
					break;
				}
			}
			if (!wasOpen) {
				m.close();
			}
		}
		return result;
	}

	public Set<IErlModule> getAllDependents() throws ErlModelException {
		final Set<IErlModule> mod = getDirectDependents();
		return getAllDependents(mod, new HashSet<IErlModule>());
	}

	private Set<IErlModule> getAllDependents(final Set<IErlModule> current,
			final Set<IErlModule> result) throws ErlModelException {
		final Set<IErlModule> next = new HashSet<IErlModule>();
		for (final IErlModule mod : current) {
			final Collection<IErlModule> dep = mod.getDirectDependents();
			result.add(mod);
			next.addAll(dep);
		}
		if (next.size() == 0) {
			return result;

		} else {
			return getAllDependents(next, result);
		}
	}

	public synchronized void resetParser(final String newText) {
		scanner = null;
		initialText = newText;
		parsed = false;
		setStructureKnown(false);
	}

	@Override
	public IErlModule getModule() {
		return this;
	}
}
