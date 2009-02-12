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
import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IPathVariableChangeEvent;
import org.eclipse.core.resources.IPathVariableChangeListener;
import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
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
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlFunctionClause;
import org.erlide.core.erlang.IErlImport;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlPreprocessorDef;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IErlRecordDef;
import org.erlide.core.erlang.ISourceRange;
import org.erlide.core.erlang.ISourceReference;
import org.erlide.core.erlang.IErlElement.Kind;
import org.erlide.core.util.ErlangFunction;
import org.erlide.core.util.ErlideUtil;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.jinterface.rpc.Tuple;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.Backend;
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

import erlang.ErlideContextAssist;
import erlang.ErlideDoc;

public class ErlContentAssistProcessor implements IContentAssistProcessor,
		IPathVariableChangeListener {
	private final ISourceViewer sourceViewer;
	private final IErlModule module;
	private final String externalModules;
	private final String externalIncludes;
	private ArrayList<Tuple> pathVars;

	private static final ICompletionProposal[] NO_COMPLETIONS = new ICompletionProposal[0];

	public ErlContentAssistProcessor(final ISourceViewer sourceViewer,
			final IErlModule module, final String externalModules,
			final String externalIncludes) {
		this.sourceViewer = sourceViewer;
		this.module = module;
		this.externalModules = externalModules;
		this.externalIncludes = externalIncludes;
		initPathVars();
	}

	public ICompletionProposal[] computeCompletionProposals(
			final ITextViewer viewer, final int offset) {
		final Backend b = ErlangCore.getBackendManager().getIdeBackend();
		try {
			final IDocument doc = viewer.getDocument();
			final String aPrefix = lastText(doc, offset);
			// final String indent = lastIndent(doc, offset);

			final int colonPos = aPrefix.indexOf(':');
			final int hashMarkPos = aPrefix.indexOf('#');
			final int dotPos = aPrefix.indexOf('.');
			final int leftBracketPos = aPrefix.indexOf('{');
			final int interrogationMarkPos = aPrefix.indexOf('?');
			List<ICompletionProposal> result;
			if (colonPos >= 0) {
				final IErlProject project = module == null ? null : module
						.getProject();
				final String moduleName = aPrefix.substring(0, colonPos);
				result = externalCallCompletions(b, project,
						unquote(moduleName), offset, aPrefix
								.substring(colonPos + 1), colonPos);
			} else if (hashMarkPos >= 0) {
				if (dotPos >= 0) {
					final String recordName = aPrefix.substring(
							hashMarkPos + 1, dotPos);
					result = recordFieldCompletions(b, recordName, offset,
							aPrefix.substring(dotPos + 1), hashMarkPos);
				} else if (leftBracketPos > hashMarkPos) {
					final String recordName = aPrefix.substring(
							hashMarkPos + 1, leftBracketPos);
					final int n = atomPrefixLength(doc, offset);
					result = recordFieldCompletions(b, recordName, offset,
							aPrefix.substring(aPrefix.length() - n),
							hashMarkPos);
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

	private List<ICompletionProposal> recordFieldCompletions(final Backend b,
			final String recordName, final int offset, final String aprefix,
			final int hashMarkPos) {
		if (module == null) {
			return null;
		}
		final IProject project = (IProject) module.getProject().getResource();
		final IErlPreprocessorDef p = ErlModelUtils.findPreprocessorDef(b,
				project, module, recordName, IErlElement.Kind.RECORD_DEF,
				externalIncludes, pathVars);
		if (p == null || !(p instanceof IErlRecordDef)) {
			return null;
		}
		final IErlRecordDef recordDef = (IErlRecordDef) p;
		final List<String> fields = recordDef.getFields();
		final List<ICompletionProposal> result = new ArrayList<ICompletionProposal>(
				fields.size());
		for (final String field : fields) {
			if (field.startsWith(aprefix)) {
				final int alength = aprefix.length();
				result.add(new CompletionProposal(field, offset - alength,
						alength, field.length()));
			}
		}
		return result;
	}

	private List<ICompletionProposal> moduleOrLocalCallCompletions(
			final Backend b, final int offset, final String aprefix, final int k) {
		final List<ICompletionProposal> result = new ArrayList<ICompletionProposal>();
		final List<String> allErlangFiles = new ArrayList<String>();
		final int alength = aprefix.length();
		if (module != null) {
			// add variables
			final IErlElement el = getElementAt(offset);
			if (el instanceof ISourceReference) {
				try {
					final ISourceRange r = ((ISourceReference) el)
							.getSourceRange();
					final int o = r.getOffset();
					final IDocument doc = sourceViewer.getDocument();
					final String src = doc.get(o, offset - o - alength);
					final Collection<String> vars = ErlideContextAssist
							.getVariables(b, src, aprefix);
					for (final String var : vars) {
						result.add(new CompletionProposal(var,
								offset - alength, alength, var.length()));
					}
				} catch (final ErlModelException e) {
					e.printStackTrace();
				} catch (final BadLocationException e) {
					e.printStackTrace();
				}
			}
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
					addFunctionCompletion(offset, aprefix, result, ef,
							getParameterNames(ef));
				}
			}
			// add modules
			final List<IErlModule> modules = ErlModelUtils
					.getModulesWithReferencedProjects(module.getProject());
			for (final IErlModule m : modules) {
				if (m.getModuleKind() == IErlModule.ModuleKind.ERL) {
					final String name = ErlideUtil
							.withoutExtension(m.getName());
					if (!allErlangFiles.contains(name)) {
						allErlangFiles.add(name);
					}
				}
			}
			// add external modules
			final List<String> mods = ErlModelUtils.getExternalModules(b,
					aprefix, externalModules, pathVars);
			for (final String m : mods) {
				final String name = ErlideUtil.basenameWithoutExtension(m);
				if (!allErlangFiles.contains(name)) {
					allErlangFiles.add(name);
				}
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
					result.add(new CompletionProposal(cpl, offset - alength,
							alength, cpl.length()));
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
	private List<ICompletionProposal> macroOrRecordCompletions(final Backend b,
			final int offset, final String aPrefix, final Kind kind) {
		if (module == null) {
			return null;
		}
		final IProject project = (IProject) module.getProject().getResource();
		final List<IErlPreprocessorDef> defs = ErlModelUtils
				.getPreprocessorDefs(b, project, module, kind,
						externalIncludes, pathVars);
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

	private List<ICompletionProposal> externalCallCompletions(final Backend b,
			final IErlProject project, final String moduleName,
			final int offset, final String aprefix, final int k)
			throws ErlangRpcException, BackendException, RpcException,
			OtpErlangRangeException {
		final List<ICompletionProposal> result = new ArrayList<ICompletionProposal>();
		// we have an external call
		final String stateDir = ErlideUIPlugin.getDefault().getStateLocation()
				.toString();
		// first check in project, refs and external modules
		final List<IErlModule> modules = ErlModelUtils
				.getModulesWithReferencedProjects(project);
		try {
			final IErlModule external = ErlModelUtils.getExternalModule(
					moduleName, externalModules, pathVars);
			if (external != null) {
				modules.add(external);
			}
		} catch (final CoreException e1) {
			e1.printStackTrace();
		}
		for (final IErlModule m : modules) {
			if (ErlideUtil.withoutExtension(m.getName()).equals(moduleName)) {
				try {
					m.open(null);
					for (final IErlElement e : m.getChildren()) {
						if (e instanceof IErlFunction) {
							final IErlFunction f = (IErlFunction) e;
							addFunctionCompletion(offset, aprefix, result, f,
									true);
						}
					}
				} catch (final ErlModelException e) {
					e.printStackTrace();
				}
			}
		}

		// then check built stuff and otp
		final OtpErlangObject res = ErlideDoc.getProposalsWithDoc(b,
				moduleName, aprefix, stateDir);
		if (res instanceof OtpErlangList) {
			final OtpErlangList resl = (OtpErlangList) res;
			for (int i = 0; i < resl.arity(); i++) {
				// {FunWithArity, FunWithParameters, [{Offset, Length}],
				// Doc}
				final OtpErlangTuple f = (OtpErlangTuple) resl.elementAt(i);
				final String funWithArity = ((OtpErlangString) f.elementAt(0))
						.stringValue();
				final String funWithParameters = ((OtpErlangString) f
						.elementAt(1)).stringValue();
				final OtpErlangList parOffsets = (OtpErlangList) f.elementAt(2);
				final int nPars = parOffsets.arity();
				String docStr = null;
				if (f.arity() > 3) {
					final OtpErlangObject elt = f.elementAt(3);
					if (elt instanceof OtpErlangString) {
						docStr = ((OtpErlangString) elt).stringValue();
					}
				}
				final String cpl = funWithParameters
						.substring(aprefix.length());
				final List<Point> offsetsAndLengths = getOffsetsAndLengths(
						parOffsets, offset);
				int offs = cpl.length();
				if (nPars > 0) {
					offs = offsetsAndLengths.get(0).x;
				}

				addFunctionCompletion(offset, result, funWithArity, docStr,
						cpl, offsetsAndLengths, offs);
			}
		}
		return result;
	}

	/**
	 * @param offset
	 * @param result
	 * @param funWithArity
	 * @param docStr
	 * @param cpl
	 * @param offsetsAndLengths
	 * @param cursorPosition
	 */
	private void addFunctionCompletion(final int offset,
			final List<ICompletionProposal> result, final String funWithArity,
			final String docStr, final String cpl,
			final List<Point> offsetsAndLengths, final int cursorPosition) {
		// first check if it's already there...
		for (final ICompletionProposal c : result) {
			if (c.getDisplayString().equals(funWithArity)) {
				return;
			}
		}
		final ICompletionProposal c = new ErlCompletionProposal(
				offsetsAndLengths, funWithArity, cpl, offset, 0,
				cursorPosition, null, null, docStr, sourceViewer);

		result.add(c);
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
			final IErlFunction function, final boolean externalsOnly) {
		if (!externalsOnly || function.isExported()) {
			addFunctionCompletion(offset, aprefix, result, function
					.getFunction(), getParameterNames(function));
		}
	}

	private List<String> getParameterNames(final IErlFunction function) {
		final String head = function.getHead();
		final int arity = function.getArity();
		final List<String> result = new ArrayList<String>(arity);
		addEmptyParameterNames(arity, result);
		addParametersFromFunctionHead(head, result);
		for (final IErlFunctionClause clause : function.getClauses()) {
			addParametersFromFunctionHead(clause.getHead(), result);
		}
		return result;
	}

	/**
	 * @param head
	 * @param result
	 */
	private void addParametersFromFunctionHead(String head,
			final List<String> result) {
		if (head != null && head.length() > 1) {
			head = betweenParens(head);
			final String[] vars = head.split(",");
			final int n = Math.min(vars.length, result.size());
			for (int i = 0; i < n; ++i) {
				if (result.get(i).equals("_")) {
					final String var = vars[i].trim();
					if (looksLikeParameter(var)) {
						result.set(i, fixVarName(var));
					}
				}
			}
		}
	}

	private String betweenParens(final String head) {
		final int length = head.length();
		if (length < 1) {
			return head;
		}
		final int startIndex = head.charAt(0) == '(' ? 1 : 0;
		final int lastPar = head.indexOf(')');
		final int endIndex = lastPar == -1 ? length : lastPar;
		return head.substring(startIndex, endIndex);
	}

	private List<String> getParameterNames(final ErlangFunction function) {
		final int arity = function.arity;
		final List<String> result = new ArrayList<String>(arity);
		addEmptyParameterNames(arity, result);
		return result;
	}

	/**
	 * @param arity
	 * @param result
	 */
	private void addEmptyParameterNames(final int arity,
			final List<String> result) {
		for (int i = result.size(); i < arity; ++i) {
			result.add("_");
		}
	}

	private String fixVarName(final String var) {
		final String v = var.charAt(0) == '_' ? var.substring(1) : var;
		final char c = v.charAt(0);
		return Character.isLowerCase(c) ? Character.toUpperCase(c)
				+ v.substring(1) : v;
	}

	private String unquote(final String moduleName) {
		if (moduleName.length() > 0 && moduleName.charAt(0) == '\'') {
			return moduleName.substring(1, moduleName.length() - 1);
		} else {
			return moduleName;
		}
	}

	/**
	 * Check if the string looks like an erlang parameter
	 * 
	 * @param parameter
	 *            String the parameter to check
	 * @return true iff parameter is like Par, _Par or _par
	 */
	private boolean looksLikeParameter(final String parameter) {
		if (parameter == null || parameter.length() == 0) {
			return false;
		}
		final char c = parameter.charAt(0);
		final char c2 = parameter.length() > 1 ? parameter.charAt(1) : c;
		return c >= 'A' && c <= 'Z' || c == '_'
				&& (c2 >= 'A' && c <= 'Z' || c2 >= 'a' && c2 <= 'z');
	}

	private void addFunctionCompletion(final int offset, final String aprefix,
			final List<ICompletionProposal> result,
			final ErlangFunction function, final List<String> parameterNames) {
		if (function.name.startsWith(aprefix)) {
			final int offs = function.name.length() - aprefix.length();
			final List<Point> offsetsAndLengths = getOffsetsAndLengths(
					parameterNames, offset + offs + 1);
			final String funWithArity = function.getNameWithArity();
			final String funWithParameters = getNameWithParameters(
					function.name, parameterNames);
			final String cpl = funWithParameters.substring(aprefix.length());
			int cursorPosition = cpl.length();
			if (offsetsAndLengths.size() > 0) {
				cursorPosition = offsetsAndLengths.get(0).x;
			}
			addFunctionCompletion(offset, result, funWithArity, null, cpl,
					offsetsAndLengths, cursorPosition);
		}
	}

	private String getNameWithParameters(final String name,
			final List<String> parameterNames) {
		final StringBuilder b = new StringBuilder();
		b.append(name).append('(');
		for (int i = 0, n = parameterNames.size(); i < n; i++) {
			b.append(parameterNames.get(i));
			if (i < n - 1) {
				b.append(", ");
			}
		}
		b.append(')');
		return b.toString();
	}

	private List<Point> getOffsetsAndLengths(final List<String> parameterNames,
			int replacementOffset) {
		final ArrayList<Point> result = new ArrayList<Point>(parameterNames
				.size());
		for (final String par : parameterNames) {
			result.add(new Point(replacementOffset, par.length()));
			replacementOffset += par.length() + 2;
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

	private int atomPrefixLength(final IDocument doc, final int offset) {
		try {
			for (int n = offset - 1; n >= 0; n--) {
				final char c = doc.getChar(n);
				if (!isErlangIdentifierChar(c)) {
					return offset - n - 1;
				}
			}
		} catch (final BadLocationException e) {
		}
		return 0;
	}

	private String lastText(final IDocument doc, final int offset) {
		// TODO rewrite so it handles stuff like #record{field1 = a, field2
		try {
			for (int n = offset - 1; n >= 0; n--) {
				final char c = doc.getChar(n);
				if (!isErlangIdentifierChar(c) && c != ':' && c != '.'
						&& c != '#' && c != '?' && c != '{' && c != '\'') {
					return doc.get(n + 1, offset - n - 1);
				}
			}
			return doc.get(0, offset);
		} catch (final BadLocationException e) {
		}
		return "";
	}

	private IErlElement getElementAt(final int offset) {
		if (module == null) {
			return null;
		}
		try {
			return module.getElementAt(offset);
		} catch (final ErlModelException e) {
			e.printStackTrace();
		}
		return null;
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

	private void initPathVars() {
		pathVars = getPathVars();
	}

	/**
	 * @return
	 */
	public static ArrayList<Tuple> getPathVars() {
		final IPathVariableManager pvm = ResourcesPlugin.getWorkspace()
				.getPathVariableManager();
		final String[] names = pvm.getPathVariableNames();
		final ArrayList<Tuple> pv = new ArrayList<Tuple>(names.length);
		for (final String name : names) {
			pv.add(new Tuple().add(name).add(pvm.getValue(name).toOSString()));
		}
		return pv;
	}

	public void pathVariableChanged(final IPathVariableChangeEvent event) {
		initPathVars();
	}

}
