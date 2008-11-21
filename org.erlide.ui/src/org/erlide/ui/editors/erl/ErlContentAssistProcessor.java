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
package org.erlide.ui.editors.erl;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.swt.graphics.Point;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlImport;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlPreprocessorDef;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IErlRecordDef;
import org.erlide.core.erlang.IErlElement.Kind;
import org.erlide.core.util.ErlangFunction;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.IdeBackend;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.util.ErlModelUtils;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlang.ErlideDoc;

public class ErlContentAssistProcessor implements IContentAssistProcessor {
	private final ISourceViewer sourceViewer;
	private final String prefix;
	private final IErlModule module;

	private static final ICompletionProposal[] NO_COMPLETIONS = new ICompletionProposal[0];

	public ErlContentAssistProcessor(final ISourceViewer sourceViewer,
			final String prefix, final IErlModule module) {
		this.sourceViewer = sourceViewer;
		this.prefix = prefix;
		this.module = module;
	}

	public ICompletionProposal[] computeCompletionProposals(
			final ITextViewer viewer, final int offset) {
		final IdeBackend b = BackendManager.getDefault().getIdeBackend();
		try {
			final IDocument doc = viewer.getDocument();
			final String aPrefix = lastText(doc, offset);
			// final String indent = lastIndent(doc, offset);

			final int colonPos = aPrefix.indexOf(':');
			final int hashMarkPos = aPrefix.indexOf('#');
			final int dotPos = aPrefix.indexOf('.');
			final int interrogationMarkPos = aPrefix.indexOf('?');
			List<ICompletionProposal> result;
			if (colonPos >= 0) {
				final IErlProject project = module.getProject();
				final String moduleName = aPrefix.substring(0, colonPos);
				result = externalCallCompletions(b, project, moduleName,
						offset, aPrefix.substring(colonPos + 1), colonPos);
			} else if (hashMarkPos >= 0) {
				if (dotPos >= 0) {
					final String recordName = aPrefix.substring(
							hashMarkPos + 1, dotPos);
					result = recordFieldCompletions(b, recordName, offset,
							aPrefix.substring(dotPos + 1), hashMarkPos);
				} else {
					result = macroOrRecordCompletions(b, offset, aPrefix
							.substring(hashMarkPos + 1),
							IErlElement.Kind.RECORD_DEF);
				}
			} else if (interrogationMarkPos >= 0) {
				result = macroOrRecordCompletions(b, offset, aPrefix
						.substring(interrogationMarkPos + 1),
						IErlElement.Kind.MACRO_DEF);
			} else {
				result = moduleOrLocalCallCompletions(b, offset, aPrefix,
						colonPos);
			}
			if (result == null) {
				return NO_COMPLETIONS;
			}
			return result.toArray(new ICompletionProposal[result.size()]);
		} catch (final Exception e) {
			ErlLogger.warn(e);
			return NO_COMPLETIONS;
		}
	}

	private List<ICompletionProposal> recordFieldCompletions(
			final IdeBackend b, final String recordName, final int offset,
			final String aprefix, final int hashMarkPos) {
		if (module == null) {
			return null;
		}
		final IProject project = (IProject) module.getProject().getResource();
		final IErlPreprocessorDef p = ErlModelUtils.findPreprocessorDef(b,
				project, module, recordName, IErlElement.Kind.RECORD_DEF);
		if (p == null || !(p instanceof IErlRecordDef)) {
			return null;
		}
		final IErlRecordDef recordDef = (IErlRecordDef) p;
		final List<String> fields = recordDef.getFields();
		final List<ICompletionProposal> result = new ArrayList<ICompletionProposal>(
				fields.size());
		for (final String field : fields) {
			if (field.startsWith(aprefix)) {
				result.add(new CompletionProposal(field, offset
						- aprefix.length(), aprefix.length(), field.length()));
			}
		}
		return result;
	}

	private List<ICompletionProposal> moduleOrLocalCallCompletions(
			final IdeBackend b, final int offset, final String aprefix,
			final int k) {
		final List<ICompletionProposal> result = new ArrayList<ICompletionProposal>();
		// add declared functions in module
		try {
			for (final IErlElement e : module.getChildren()) {
				if (e instanceof IErlFunction) {
					final IErlFunction f = (IErlFunction) e;
					addFunctionCompletion(offset, aprefix, result, f, false);
				}
			}
		} catch (final ErlModelException e) {
			e.printStackTrace();
		}
		// add imported functions in module
		for (final IErlImport imp : module.getImports()) {
			for (final ErlangFunction ef : imp.getFunctions()) {
				addFunctionCompletion(offset, aprefix, result, ef);
			}
		}
		// add modules
		final List<IErlModule> modules = ErlModelUtils
				.getModulesWithReferencedProjects(module.getProject());
		final List<String> allErlangFiles = new ArrayList<String>();
		for (final IErlModule m : modules) {
			final String name = m.getName();
			if (!allErlangFiles.contains(name)) {
				allErlangFiles.add(name);
			}
		}
		OtpErlangObject res = null;
		res = ErlideDoc.getModules(b, aprefix, allErlangFiles);
		if (res instanceof OtpErlangList) {
			final OtpErlangList resList = (OtpErlangList) res;
			for (int i = 0; i < resList.arity(); ++i) {
				final OtpErlangObject o = resList.elementAt(i);
				if (o instanceof OtpErlangString) {
					final OtpErlangString s = (OtpErlangString) o;
					final String cpl = s.stringValue() + ":";
					result
							.add(new CompletionProposal(cpl, offset
									- aprefix.length(), aprefix.length(), cpl
									.length()));
				}
			}
		}
		return result;
	}

	/**
	 * @param b
	 * @param offset
	 * @param aPrefix
	 * @param kind
	 * @return
	 */
	private List<ICompletionProposal> macroOrRecordCompletions(
			final IdeBackend b, final int offset, final String aPrefix,
			final Kind kind) {
		if (module == null) {
			return null;
		}
		final IProject project = (IProject) module.getProject().getResource();
		final List<IErlPreprocessorDef> defs = ErlModelUtils
				.getPreprocessorDefs(b, project, module, kind);
		final List<ICompletionProposal> result = new ArrayList<ICompletionProposal>();
		for (final IErlPreprocessorDef pd : defs) {
			final String name = pd.getDefinedName();
			if (name.startsWith(aPrefix)) {
				result.add(new CompletionProposal(name, offset
						- aPrefix.length(), aPrefix.length(), name.length()));
			}
		}
		return result;
	}

	private List<ICompletionProposal> externalCallCompletions(
			final IdeBackend b, final IErlProject project,
			final String moduleName, final int offset, final String aprefix,
			final int k) throws ErlangRpcException, BackendException,
			RpcException, OtpErlangRangeException {
		final List<ICompletionProposal> result = new ArrayList<ICompletionProposal>();
		// we have an external call
		final String stateDir = ErlideUIPlugin.getDefault().getStateLocation()
				.toString();
		// first check in project and refs
		final IErlModule m = ErlModelUtils.getModule(moduleName);
		if (m != null) {
			try {
				m.open(null);
				for (final IErlElement e : m.getChildren()) {
					if (e instanceof IErlFunction) {
						final IErlFunction f = (IErlFunction) e;
						addFunctionCompletion(offset, aprefix, result, f, true);
					}
				}
			} catch (final ErlModelException e) {
				e.printStackTrace();
			}
		} else {
			// then check built stuff (and otp)
			final OtpErlangObject res = ErlideDoc.getProposalsWithDoc(b,
					moduleName, aprefix, stateDir);
			// final OtpErlangObject res = ErlideDoc
			// .getExported(b, prefix, moduleName);
			if (res instanceof OtpErlangList) {
				final OtpErlangList resl = (OtpErlangList) res;
				for (int i = 0; i < resl.arity(); i++) {
					// {FunWithArity, FunWithParameters, [{Offset, Length}],
					// Doc}
					final OtpErlangTuple f = (OtpErlangTuple) resl.elementAt(i);
					final String funWithArity = ((OtpErlangString) f
							.elementAt(0)).stringValue();
					final String funWithParameters = ((OtpErlangString) f
							.elementAt(1)).stringValue();
					final OtpErlangList parOffsets = (OtpErlangList) f
							.elementAt(2);
					final int nPars = parOffsets.arity();
					String docStr = null;
					if (f.arity() > 3) {
						final OtpErlangObject elt = f.elementAt(3);
						if (elt instanceof OtpErlangString) {
							docStr = ((OtpErlangString) elt).stringValue();
						}
					}
					final String cpl = funWithParameters.substring(aprefix
							.length());
					final List<Point> offsetsAndLengths = getOffsetsAndLengths(
							parOffsets, offset);
					int offs = cpl.length();
					if (nPars > 0) {
						offs = offsetsAndLengths.get(0).x;
					}

					final ICompletionProposal c = new ErlCompletionProposal(
							offsetsAndLengths, funWithArity, cpl, offset, 0,
							offs, null, null, docStr, sourceViewer);

					result.add(c);
				}
			}
		}
		return result;
	}

	/**
	 * @param offset
	 * @param aprefix
	 * @param result
	 * @param function
	 * @param externalsOnly
	 *            TODO
	 */
	private void addFunctionCompletion(final int offset, final String aprefix,
			final List<ICompletionProposal> result,
			final IErlFunction function, boolean externalsOnly) {
		if ((!externalsOnly || function.isExported())
				&& function.getName().startsWith(aprefix)) {
			int offs = function.getName().length() - aprefix.length();
			final List<Point> offsetsAndLengths = getOffsetsAndLengths(function
					.getArity(), offset + offs + 1);
			if (offsetsAndLengths.size() > 0) {
				offs = offsetsAndLengths.get(0).x;
			}
			final String funWithArity = function.getNameWithArity();
			final String funWithParameters = function.getNameWithParameters();
			final String cpl = funWithParameters.substring(aprefix.length());
			final ICompletionProposal c = new ErlCompletionProposal(
					offsetsAndLengths, funWithArity, cpl, offset, 0, offs,
					null, null, null, sourceViewer);
			result.add(c);
		}
	}

	private void addFunctionCompletion(final int offset, final String aprefix,
			final List<ICompletionProposal> result,
			final ErlangFunction function) {
		if (function.name.startsWith(aprefix)) {
			int offs = function.name.length() - aprefix.length();
			final List<Point> offsetsAndLengths = getOffsetsAndLengths(
					function.arity, offset + offs + 1);
			if (offsetsAndLengths.size() > 0) {
				offs = offsetsAndLengths.get(0).x;
			}
			final String funWithArity = function.getNameWithArity();
			final String funWithParameters = function.getNameWithParameters();
			final String cpl = funWithParameters.substring(aprefix.length());
			final ICompletionProposal c = new ErlCompletionProposal(
					offsetsAndLengths, funWithArity, cpl, offset, 0, offs,
					null, null, null, sourceViewer);
			result.add(c);
		}
	}

	private List<Point> getOffsetsAndLengths(final int arity,
			final int replacementOffset) {
		final ArrayList<Point> result = new ArrayList<Point>(arity);
		for (int i = 0; i < arity; i++) {
			result.add(new Point(replacementOffset + i * 3, 1));
		}
		return result;
	}

	private List<Point> getOffsetsAndLengths(final OtpErlangList parOffsets,
			final int replacementOffset) {
		final int arity = parOffsets.arity();
		final List<Point> result = new ArrayList<Point>(arity);
		for (int i = 0; i < arity; i++) {
			final OtpErlangTuple t = (OtpErlangTuple) parOffsets.elementAt(i);
			final OtpErlangLong offset = (OtpErlangLong) t.elementAt(0);
			final OtpErlangLong length = (OtpErlangLong) t.elementAt(1);
			try {
				result.add(new Point(offset.intValue() + replacementOffset,
						length.intValue()));
			} catch (final OtpErlangRangeException e) {
			}
		}
		return result;
	}

	private String lastText(final IDocument doc, final int offset) {
		try {
			for (int n = offset - 1; n >= 0; n--) {
				final char c = doc.getChar(n);
				if (!isErlangIdentifierChar(c) && c != ':' && c != '.'
						&& c != '#' && c != '?') {
					return doc.get(n + 1, offset - n - 1);
				}
			}
			return doc.get(0, offset);
		} catch (final BadLocationException e) {
		}
		return "";
	}

	static private boolean isErlangIdentifierChar(final char char1) {
		return Character.isJavaIdentifierPart(char1);
	}

	public IContextInformation[] computeContextInformation(
			final ITextViewer viewer, final int offset) {
		return null;
	}

	public char[] getCompletionProposalAutoActivationCharacters() {
		return new char[] { ':', '?', '#' };
	}

	public char[] getContextInformationAutoActivationCharacters() {
		return null;
	}

	public String getErrorMessage() {
		return null;
	}

	public IContextInformationValidator getContextInformationValidator() {
		return null;
	}
}
