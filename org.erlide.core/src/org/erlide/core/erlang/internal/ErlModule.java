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

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.reconciler.DirtyRegion;
import org.erlide.basiccore.ErlLogger;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlScanner;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlAttribute;
import org.erlide.core.erlang.IErlComment;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlExport;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlFunctionClause;
import org.erlide.core.erlang.IErlImport;
import org.erlide.core.erlang.IErlMacroDef;
import org.erlide.core.erlang.IErlMember;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlPreprocessorDef;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IErlRecordDef;
import org.erlide.core.erlang.IErlScanner;
import org.erlide.core.erlang.ISourceRange;
import org.erlide.core.erlang.ISourceReference;
import org.erlide.core.erlang.util.BufferManager;
import org.erlide.core.erlang.util.IBuffer;
import org.erlide.core.erlang.util.Util;
import org.erlide.core.util.ErlangFunction;
import org.erlide.core.util.ErlangIncludeFile;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

public class ErlModule extends Openable implements IErlModule {

	private long timestamp;

	private OtpErlangObject parseTree;

	private boolean isModule;

	// the document last reconciled with
	// private IDocument fDoc;
	private List<IErlComment> comments;

	private IErlScanner fScanner;

	protected ErlModule(IErlElement parent, String name, boolean isErl) {
		super(parent, name);
		if (ErlModelManager.verbose) {
			ErlLogger.debug("...creating " + parent.getElementName() + "/"
					+ name + " " + isErl);
		}
		isModule = isErl;
		comments = new ArrayList<IErlComment>(0);
	}

	protected ErlModule(IErlElement parent, String name, String text,
			boolean isErl) {
		this(parent, name, isErl);
		final IBuffer b = BufferManager.getDefaultBufferManager().createBuffer(
				this);
		b.setContents(text);
		b.addBufferChangedListener(this);
	}

	@Override
	protected boolean buildStructure(IProgressMonitor pm,
			IResource underlyingResource, IDocument doc, DirtyRegion dirtyRegion)
			throws ErlModelException {
		// get buffer contents
		final IBuffer buffer = getBufferManager().getBuffer(this);
		if (buffer == null) {
			openBuffer(pm, this);
		}

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
		if (doc == null) {
			doc = new Document();
			doc.set(getBuffer().getContents());
		}
		getScanner().modifyText(doc, dirtyRegion);

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
		for (IErlElement element : fChildren) {
			if (element instanceof IErlFunction) {
				final IErlFunction f = (IErlFunction) element;
				final IErlElement[] cls = f.getChildren();
				for (IErlElement element0 : cls) {
					final ISourceReference ch = (ISourceReference) element0;
					final ISourceRange r = ch.getSourceRange();
					if (r.hasPosition(position)) {
						return element0;
					}
				}
			} else {
				final ISourceReference ch = (ISourceReference) element;
				final ISourceRange r = ch.getSourceRange();
				if (r != null && r.hasPosition(position)) {
					return element;
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
		final IResource parentRes = this.getParent().getResource();
		if (parentRes == null || !(parentRes instanceof IContainer)) {
			return null;
		}
		return ((IContainer) parentRes).getFile(new Path(getElementName()));
	}

	public String getSource() throws ErlModelException {
		return getBuffer().getContents();
	}

	public ISourceRange getSourceRange() throws ErlModelException {
		return new SourceRange(0, getBuffer().getLength());
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

	@Override
	protected IBuffer openBuffer(IProgressMonitor pm, Object info)
			throws ErlModelException {
		final IBuffer b = BufferManager.getDefaultBufferManager().createBuffer(
				this);
		try {
			final IFile f = (IFile) getUnderlyingResource();
			b.setContents(Util.getResourceContentsAsCharArray(f));
			b.addBufferChangedListener(this);
		} catch (final ErlModelException e) {
			// e.printStackTrace();
		}
		return b;
	}

	public void addMember(IErlMember elem) {
		final IErlElement[] newch = new IErlElement[fChildren.length + 1];
		System.arraycopy(fChildren, 0, newch, 0, fChildren.length);
		newch[fChildren.length] = elem;
		fChildren = newch;
	}

	public void addComment(IErlComment c) {
		comments.add(c);
	}

	public void reset() {
		fChildren = ErlElement.NO_ELEMENTS;
		comments = new ArrayList<IErlComment>(0);
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

	public void getContentProposals(String prefix, String indent, int offset,
			ArrayList<ICompletionProposal> result) {
		// ErlLogger.debug("> completing: " + prefix + "," + offset);
		CompletionProposal cp = null;

		class PredefMacros {
			String fMacroName;

			String fDescription;

			PredefMacros(String name, String desc) {
				fMacroName = name;
				fDescription = desc;
			}

			String getName() {
				return fMacroName;
			}

			String getDesc() {
				return fDescription;
			}
		}

		/* Pre-defined macroses */
		PredefMacros[] Macroses = {
				new PredefMacros("?MODULE", "The name of the current module."),
				new PredefMacros("?MODULE_STRING",
						"The name of the current module, as a string."),
				new PredefMacros("?FILE",
						"The file name of the current module."),
				new PredefMacros("?LINE", "The current line number."),
				new PredefMacros("?MACHINE", "The machine name, 'BEAM'.") };
		for (PredefMacros m : Macroses) {
			final String mname = m.getName();
			if (mname.startsWith(prefix)) {
				cp = new CompletionProposal(mname, /* replacementString */
				offset - prefix.length(), /* replacementOffset */
				prefix.length(), /* replacementLength */
				mname.length(), /* cursorPosition */
				null, /* image */
				mname, /* displayString */
				null, /* contextInformation */
				m.getDesc()); /* String additionalProposalInfo */
			}
			if (cp != null) {
				result.add(cp);
				cp = null;
			}
		}

		/* Local module completition: records, defines, functions */
		for (IErlElement el : fChildren) {
			if (el instanceof IErlFunction) {
				final IErlFunction f = (IErlFunction) el;
				String FuncName = f.getElementName();
				if (FuncName.startsWith(prefix)) {
					for (IErlFunctionClause fc : f.getClauses()) {
						cp = new CompletionProposal(FuncName, /* replacementString */
						offset - prefix.length(), /* replacementOffset */
						prefix.length(), /* replacementLength */
						FuncName.length(), /* cursorPosition */
						null, /* image */
						FuncName + fc.toString(), /* displayString */
						null, /* contextInformation */
						null); /* String additionalProposalInfo */
					}
				}
			} else if (el instanceof IErlRecordDef) {
				final IErlRecordDef rec = (IErlRecordDef) el;
				String RecName = rec.getDefinedName();
				if (prefix.length() == 0
						|| (prefix.charAt(0) == '#' && RecName
								.startsWith(prefix.substring(1)))) {
					cp = new CompletionProposal("#" + RecName, /* replacementString */
					offset - prefix.length(), /* replacementOffset */
					prefix.length(), /* replacementLength */
					RecName.length() + 1, /* cursorPosition */
					null, /* image */
					"#" + rec.getDefinedName() + "{...}", /*
					 * FIXME:
					 * displayString
					 */
					null, /* contextInformation */
					null); /* String additionalProposalInfo */
				}
			} else if (el instanceof IErlMacroDef) {
				final IErlMacroDef mac = (IErlMacroDef) el;
				String MacName = mac.getDefinedName();
				if (prefix.length() == 0
						|| (prefix.charAt(0) == '?' && MacName
								.startsWith(prefix.substring(1)))) {
					cp = new CompletionProposal("?" + MacName, /* replacementString */
					offset - prefix.length(), /* replacementOffset */
					prefix.length(), /* replacementLength */
					MacName.length() + 1, /* cursorPosition */
					null, /* image */
					"?" + mac.getDefinedName(), /* displayString */
					null, /* contextInformation */
					null); /* String additionalProposalInfo */
				}
			}
			if (cp != null) {
				result.add(cp);
				cp = null;
			}
		}
	}

	public long getTimestamp() {
		return timestamp;
	}

	public void reconcile(IDocument doc, DirtyRegion dirtyRegion) {
		if (doc == null) {
			return;
		}
		final IBuffer buffer = getBufferManager().getBuffer(this);
		if (buffer == null) {
			return;
		}
		buffer.setContents(doc.get());
		try {
			buildStructure(null, this.getResource(), doc, dirtyRegion);
		} catch (final ErlModelException e) {
			e.printStackTrace();
		}
	}

	public void reconcile(IDocument document) {
		// fDoc = document;
		reconcile(document, new DirtyRegion(0, document.getLength(),
				DirtyRegion.INSERT, document.get()));
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
		final List<IErlImport> r = new ArrayList<IErlImport>(0);
		for (final IErlElement m : fChildren) {
			if (m instanceof IErlImport) {
				r.add((IErlImport) m);
			}
		}
		return r.toArray(new IErlImport[r.size()]);
	}

	public IErlScanner getScanner() {
		if (fScanner == null) {
			fScanner = new ErlScanner(this);
		}
		return fScanner;
	}

	public void fixExportedFunctions() {
		final List<ErlangFunction> exports = new ArrayList<ErlangFunction>(10);
		for (final IErlElement m : fChildren) {
			if (m instanceof IErlExport) {
				final OtpErlangList l = (OtpErlangList) ((IErlExport) m)
						.getParseTree();
				for (int j = 0; j < l.arity(); ++j) {
					// TODO removed temporarily
					// try {
					// exports.add(new ErlangFunction((OtpErlangTuple)
					// l.elementAt(j)));
					// } catch (OtpErlangRangeException e) {
					// e.printStackTrace();
					// }
				}
			}
		}
		for (final IErlElement m : fChildren) {
			if (m instanceof ErlFunction) {
				final ErlFunction f = (ErlFunction) m;
				f.setExported(exports.contains(f.getFunction()));
			}
		}
	}

}
