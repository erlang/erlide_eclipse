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
package org.erlide.ui.editors.erl.completion;

import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
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
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlPreprocessorDef;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IErlRecordDef;
import org.erlide.core.erlang.ISourceRange;
import org.erlide.core.erlang.ISourceReference;
import org.erlide.core.erlang.IErlElement.Kind;
import org.erlide.core.erlang.util.ErlangFunction;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.util.Util;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.templates.ErlTemplateCompletionProcessor;
import org.erlide.ui.util.ErlModelUtils;
import org.erlide.ui.util.eclipse.HTMLPrinter;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlang.ErlideContextAssist;
import erlang.ErlideDoc;
import erlang.ErlideContextAssist.RecordCompletion;

public class ErlContentAssistProcessor implements IContentAssistProcessor {

	public class CompletionNameComparer implements
			Comparator<ICompletionProposal> {

		public int compare(final ICompletionProposal o1,
				final ICompletionProposal o2) {
			final String s1 = o1.getDisplayString();
			final String s2 = o2.getDisplayString();
			return s1.compareTo(s2);
		}

	}

	private final ISourceViewer sourceViewer;
	private final IErlModule module;
	// private final String externalModules;
	// private final String externalIncludes;
	private static URL fgStyleSheet;

	private static final int DECLARED_FUNCTIONS = 1;
	private static final int EXTERNAL_FUNCTIONS = 2;
	private static final int VARIABLES = 4;
	private static final int RECORD_FIELDS = 8;
	private static final int RECORD_DEFS = 0x10;
	private static final int MODULES = 0x20;
	private static final int MACRO_DEFS = 0x40;
	private static final int IMPORTED_FUNCTIONS = 0x80;
	private static final int AUTO_IMPORTED_FUNCTIONS = 0x100;

	private static final int ARITY_ONLY = 0x1000;
	private static final int UNEXPORTED_ONLY = 0x2000;

	private static final List<ICompletionProposal> EMPTY_COMPLETIONS = new ArrayList<ICompletionProposal>();

	private final CompletionNameComparer completionNameComparer = new CompletionNameComparer();

	public ErlContentAssistProcessor(final ISourceViewer sourceViewer,
			final IErlModule module) {
		this.sourceViewer = sourceViewer;
		this.module = module;
		// this.externalModules = externalModules;
		// this.externalIncludes = externalIncludes;
		initStyleSheet();
	}

	public ICompletionProposal[] computeCompletionProposals(
			final ITextViewer viewer, final int offset) {
		if (module == null) {
			return null;
		}
		try {
			final IDocument doc = viewer.getDocument();
			String before = getBefore(viewer, doc, offset);
			final int commaPos = before.lastIndexOf(',');
			final int colonPos = before.lastIndexOf(':');
			final int hashMarkPos = before.lastIndexOf('#');
			final int dotPos = before.lastIndexOf('.');
			final int parenPos = before.lastIndexOf('(');
			final int leftBracketPos = before.lastIndexOf('{');
			final int interrogationMarkPos = before.lastIndexOf('?');
			final String prefix = getPrefix(before);
			List<ICompletionProposal> result;
			int flags;
			int pos;
			String moduleOrRecord = null;
			final IErlProject project = module.getProject();
			final Backend b = ErlangCore.getBackendManager().getBuildBackend(
					project.getProject());
			final IErlElement element = getElementAt(offset);
			RecordCompletion rc = null;
			if (hashMarkPos >= 0) {
				rc = ErlideContextAssist.checkRecordCompletion(b, before
						.substring(hashMarkPos));
			}
			if (rc != null && rc.isNameWanted()) {
				flags = RECORD_DEFS;
				pos = hashMarkPos;
				before = rc.getPrefix();
			} else if (rc != null && rc.isFieldWanted()) {
				flags = RECORD_FIELDS;
				pos = hashMarkPos;
				if (dotPos > hashMarkPos) {
					pos = dotPos;
				} else if (leftBracketPos > hashMarkPos) {
					pos = leftBracketPos;
				} else {
					assert false;
				}
				before = rc.getPrefix();
				moduleOrRecord = rc.getName();
			} else if (colonPos > commaPos && colonPos > parenPos) {
				moduleOrRecord = ErlideUtil.unquote(getPrefix(before.substring(
						0, colonPos)));
				flags = EXTERNAL_FUNCTIONS;
				pos = colonPos;
				before = before.substring(colonPos + 1);
			} else if (interrogationMarkPos > hashMarkPos
					&& interrogationMarkPos > commaPos
					&& interrogationMarkPos > colonPos) {
				flags = MACRO_DEFS;
				pos = interrogationMarkPos;
				before = before.substring(interrogationMarkPos + 1);
			} else {
				// TODO add more contexts...
				flags = 0;
				pos = colonPos;
				before = prefix;
				if (element != null) {
					if (element.getKind() == IErlElement.Kind.EXPORT) {
						flags = DECLARED_FUNCTIONS | ARITY_ONLY
								| UNEXPORTED_ONLY;
					} else if (element.getKind() == IErlElement.Kind.IMPORT) {
						final IErlImport i = (IErlImport) element;
						moduleOrRecord = i.getImportModule();
						flags = EXTERNAL_FUNCTIONS | ARITY_ONLY;
					} else if (element.getKind() == IErlElement.Kind.FUNCTION
							|| element.getKind() == IErlElement.Kind.CLAUSE) {
						flags = MODULES;
						if (module != null) {
							flags |= VARIABLES | DECLARED_FUNCTIONS
									| IMPORTED_FUNCTIONS
									| AUTO_IMPORTED_FUNCTIONS;

						}
					}
				}
			}
			result = addCompletions(flags, offset, before, moduleOrRecord, pos,
					project, b);
			final ErlTemplateCompletionProcessor t = new ErlTemplateCompletionProcessor(
					doc, offset - before.length(), before.length());
			result.addAll(Arrays.asList(t.computeCompletionProposals(viewer,
					offset)));
			return result.toArray(new ICompletionProposal[result.size()]);
		} catch (final Exception e) {
			ErlLogger.warn(e);
			return null;
		}
	}

	private List<ICompletionProposal> addCompletions(final int flags,
			final int offset, final String prefix, final String moduleOrRecord,
			final int pos, final IErlProject project, final Backend backend)
			throws CoreException, OtpErlangRangeException, BadLocationException {
		final List<ICompletionProposal> result = new ArrayList<ICompletionProposal>();
		if ((flags & DECLARED_FUNCTIONS) != 0) {
			addSorted(result, getDeclaredFunctions(offset, prefix,
					(flags & UNEXPORTED_ONLY) != 0, (flags & ARITY_ONLY) != 0));
		}
		if ((flags & VARIABLES) != 0) {
			addSorted(result, getVariables(backend, offset, prefix));
		}
		if ((flags & IMPORTED_FUNCTIONS) != 0) {
			addSorted(result, getImportedFunctions(backend, offset, prefix));
		}
		if ((flags & AUTO_IMPORTED_FUNCTIONS) != 0) {
			addSorted(result, getAutoImportedFunctions(backend, offset, prefix));
		}
		if ((flags & MODULES) != 0) {
			addSorted(result, getModules(backend, offset, prefix));
		}
		if ((flags & RECORD_DEFS) != 0) {
			addSorted(result, getMacroOrRecordCompletions(backend, offset,
					prefix, IErlElement.Kind.RECORD_DEF));
		}
		if ((flags & RECORD_FIELDS) != 0) {
			addSorted(result, getRecordFieldCompletions(backend,
					moduleOrRecord, offset, prefix, pos));
		}
		if ((flags & MACRO_DEFS) != 0) {
			addSorted(result, getMacroOrRecordCompletions(backend, offset,
					prefix, IErlElement.Kind.MACRO_DEF));
		}
		if ((flags & EXTERNAL_FUNCTIONS) != 0) {
			addSorted(result, getExternalCallCompletions(backend, project,
					moduleOrRecord, offset, prefix, (flags & ARITY_ONLY) != 0));
		}
		return result;
	}

	private void addSorted(final List<ICompletionProposal> result,
			final List<ICompletionProposal> completions) {
		Collections.sort(completions, completionNameComparer);
		result.addAll(completions);
	}

	private List<ICompletionProposal> getRecordFieldCompletions(
			final Backend b, final String recordName, final int offset,
			final String prefix, final int hashMarkPos) {
		if (module == null) {
			return EMPTY_COMPLETIONS;
		}
		final IErlProject erlProject = module.getProject();
		final IProject project = erlProject == null ? null
				: (IProject) erlProject.getResource();
		final IErlModel model = ErlangCore.getModel();
		final IErlPreprocessorDef p = ErlModelUtils.findPreprocessorDef(b,
				project, module, recordName, IErlElement.Kind.RECORD_DEF, model
						.getExternal(erlProject, ErlangCore.EXTERNAL_INCLUDES));
		if (p == null || !(p instanceof IErlRecordDef)) {
			return EMPTY_COMPLETIONS;
		}
		final List<ICompletionProposal> result = new ArrayList<ICompletionProposal>();
		final IErlRecordDef recordDef = (IErlRecordDef) p;
		final List<String> fields = recordDef.getFields();
		for (final String field : fields) {
			if (field.startsWith(prefix)) {
				final int alength = prefix.length();
				result.add(new CompletionProposal(field, offset - alength,
						alength, field.length()));
			}
		}
		return result;
	}

	private List<ICompletionProposal> getModules(final Backend b,
			final int offset, final String prefix) {
		final List<String> allErlangFiles = new ArrayList<String>();
		if (module != null) {
			final IErlProject erlProject = module.getProject();
			final List<IErlModule> modules = ErlModelUtils
					.getModulesWithReferencedProjects(erlProject);
			for (final IErlModule m : modules) {
				if (m.getModuleKind() == IErlModule.ModuleKind.ERL) {
					final String name = ErlideUtil
							.withoutExtension(m.getName());
					if (!allErlangFiles.contains(name)) {
						allErlangFiles.add(name);
					}
				}
			}
			final IErlModel model = ErlangCore.getModel();
			// add external modules
			final List<String> mods = ErlModelUtils.getExternalModules(b,
					prefix, model.getExternal(erlProject,
							ErlangCore.EXTERNAL_MODULES));
			for (final String m : mods) {
				final String name = ErlideUtil.basenameWithoutExtension(m);
				if (!allErlangFiles.contains(name)) {
					allErlangFiles.add(name);
				}
			}
		}
		final List<ICompletionProposal> result = new ArrayList<ICompletionProposal>();
		OtpErlangObject res = null;
		res = ErlideDoc.getModules(b, prefix, allErlangFiles);
		if (res instanceof OtpErlangList) {
			final OtpErlangList resList = (OtpErlangList) res;
			for (int i = 0; i < resList.arity(); ++i) {
				final OtpErlangObject o = resList.elementAt(i);
				if (o instanceof OtpErlangString) {
					final OtpErlangString s = (OtpErlangString) o;
					final String cpl = s.stringValue() + ":";
					final int prefixLength = prefix.length();
					result.add(new CompletionProposal(cpl, offset
							- prefixLength, prefixLength, cpl.length()));
				}
			}
		}
		return result;
	}

	private List<ICompletionProposal> getAutoImportedFunctions(
			final Backend backend, final int offset, final String prefix) {
		final String stateDir = ErlideUIPlugin.getDefault().getStateLocation()
				.toString();
		final OtpErlangObject res = ErlideDoc.getProposalsWithDoc(backend,
				"<auto_imported>", prefix, stateDir);
		final List<ICompletionProposal> result = new ArrayList<ICompletionProposal>();
		addFunctionProposalsWithDoc(offset, prefix, result, res, null, false);
		return result;
	}

	private List<ICompletionProposal> getImportedFunctions(
			final Backend backend, final int offset, final String prefix) {
		final String stateDir = ErlideUIPlugin.getDefault().getStateLocation()
				.toString();
		final List<ICompletionProposal> result = new ArrayList<ICompletionProposal>();
		for (final IErlImport imp : module.getImports()) {
			final OtpErlangObject res = ErlideDoc.getProposalsWithDoc(backend,
					imp.getImportModule(), prefix, stateDir);
			addFunctionProposalsWithDoc(offset, prefix, result, res, imp, false);
		}
		return result;
	}

	private List<ICompletionProposal> getDeclaredFunctions(final int offset,
			final String prefix, final boolean unexportedOnly,
			final boolean arityOnly) throws ErlModelException {
		final List<ICompletionProposal> result = new ArrayList<ICompletionProposal>();
		for (final IErlElement e : module.getChildren()) {
			if (e instanceof IErlFunction) {
				final IErlFunction f = (IErlFunction) e;
				if (unexportedOnly && f.isExported()) {
					continue;
				}
				addFunctionCompletion(offset, prefix, result, f, arityOnly);
			}
		}
		return result;
	}

	private List<ICompletionProposal> getVariables(final Backend b,
			final int offset, final String prefix) throws ErlModelException,
			BadLocationException {
		final List<ICompletionProposal> result = new ArrayList<ICompletionProposal>();
		// get variables
		final IErlElement el = getElementAt(offset);
		if (el instanceof ISourceReference) {
			final ISourceRange r = ((ISourceReference) el).getSourceRange();
			final int o = r.getOffset();
			final IDocument doc = sourceViewer.getDocument();
			final int prefixLength = prefix.length();
			final String src = doc.get(o, offset - o - prefixLength);
			final Collection<String> vars = ErlideContextAssist.getVariables(b,
					src, prefix);
			for (final String var : vars) {
				result.add(new CompletionProposal(var, offset - prefixLength,
						prefixLength, var.length()));
			}
		}
		return result;
	}

	/**
	 * @param b
	 * @param offset
	 * @param prefix
	 * @param kind
	 * @return
	 * @return
	 */
	private List<ICompletionProposal> getMacroOrRecordCompletions(
			final Backend b, final int offset, final String prefix,
			final Kind kind) {
		if (module == null) {
			return EMPTY_COMPLETIONS;
		}
		final IProject project = (IProject) module.getProject().getResource();
		final IErlModel model = ErlangCore.getModel();
		final IErlProject erlProject = module.getProject();
		final List<IErlPreprocessorDef> defs = ErlModelUtils
				.getPreprocessorDefs(b, project, module, kind, model
						.getExternal(erlProject, ErlangCore.EXTERNAL_INCLUDES));
		final List<ICompletionProposal> result = new ArrayList<ICompletionProposal>();
		for (final IErlPreprocessorDef pd : defs) {
			final String name = pd.getDefinedName();
			if (name.startsWith(prefix)) {
				result.add(new CompletionProposal(name, offset
						- prefix.length(), prefix.length(), name.length()));
			}
		}
		return result;
	}

	private List<ICompletionProposal> getExternalCallCompletions(
			final Backend b, final IErlProject project,
			final String moduleName, final int offset, final String aprefix,
			final boolean arityOnly) throws OtpErlangRangeException,
			CoreException {
		final String stateDir = ErlideUIPlugin.getDefault().getStateLocation()
				.toString();
		// we have an external call
		// first check in project, refs and external modules
		final List<IErlModule> modules = ErlModelUtils
				.getModulesWithReferencedProjects(project);
		final IErlModel model = ErlangCore.getModel();
		final IErlProject erlProject = module == null ? null : module
				.getProject();
		final IErlModule external = ErlModelUtils.getExternalModule(moduleName,
				model.getExternal(erlProject, ErlangCore.EXTERNAL_MODULES));
		if (external != null) {
			modules.add(external);
		}
		final List<ICompletionProposal> result = new ArrayList<ICompletionProposal>();
		for (final IErlModule m : modules) {
			if (ErlideUtil.withoutExtension(m.getName()).equals(moduleName)) {
				try {
					m.open(null);
					for (final IErlElement e : m.getChildren()) {
						if (e instanceof IErlFunction) {
							final IErlFunction f = (IErlFunction) e;
							if (f.isExported()) {
								addFunctionCompletion(offset, aprefix, result,
										f, arityOnly);
							}
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
		addFunctionProposalsWithDoc(offset, aprefix, result, res, null,
				arityOnly);
		return result;
	}

	private void addFunctionProposalsWithDoc(final int offset,
			final String aprefix, final List<ICompletionProposal> result,
			final OtpErlangObject res, final IErlImport erlImport,
			final boolean arityOnly) {
		if (res instanceof OtpErlangList) {
			final OtpErlangList resl = (OtpErlangList) res;
			for (final OtpErlangObject i : resl) {
				// {FunWithArity, FunWithParameters, [{Offset, Length}], Doc}
				final OtpErlangTuple f = (OtpErlangTuple) i;
				final String funWithArity = ((OtpErlangString) f.elementAt(0))
						.stringValue();
				if (!filterImported(erlImport, funWithArity)) {
					continue;
				}
				String funWithParameters = arityOnly ? funWithArity
						: ((OtpErlangString) f.elementAt(1)).stringValue();
				final OtpErlangList parOffsets = (OtpErlangList) f.elementAt(2);
				String docStr = null;
				if (f.arity() > 3) {
					final OtpErlangObject elt = f.elementAt(3);
					if (elt instanceof OtpErlangString) {
						final StringBuffer sb = new StringBuffer(Util
								.stringValue(elt));
						if (sb.length() > 0) {
							HTMLPrinter.insertPageProlog(sb, 0, fgStyleSheet);
							HTMLPrinter.addPageEpilog(sb);
						}
						docStr = sb.toString();
					}
				}

				funWithParameters = funWithParameters.substring(aprefix
						.length());
				final List<Point> offsetsAndLengths = new ArrayList<Point>();
				if (!arityOnly) {
					addOffsetsAndLengths(parOffsets, offset, offsetsAndLengths);
				}
				addFunctionCompletion(offset, result, funWithArity, docStr,
						funWithParameters, offsetsAndLengths);
			}
		}
	}

	private boolean filterImported(final IErlImport erlImport,
			final String funWithArity) {
		if (erlImport == null) {
			return true;
		}
		for (final ErlangFunction ef : erlImport.getFunctions()) {
			if (ef.getNameWithArity().equals(funWithArity)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * @param offset
	 * @param result
	 * @param funWithArity
	 * @param docStr
	 * @param funWithParameters
	 * @param offsetsAndLengths
	 * @param cursorPosition
	 */
	private void addFunctionCompletion(final int offset,
			final List<ICompletionProposal> result, final String funWithArity,
			final String docStr, final String funWithParameters,
			final List<Point> offsetsAndLengths) {
		int cursorPosition = funWithParameters.length();
		if (offsetsAndLengths.size() > 0) {
			cursorPosition = offsetsAndLengths.get(0).x;
		}

		// first check if it's already there...
		for (final ICompletionProposal c : result) {
			if (c.getDisplayString().equals(funWithArity)) {
				return;
			}
		}
		final ICompletionProposal c = new ErlCompletionProposal(
				offsetsAndLengths, funWithArity, funWithParameters, offset, 0,
				cursorPosition, null, null, docStr, sourceViewer);

		result.add(c);
	}

	private void addFunctionCompletion(final int offset, final String aprefix,
			final List<ICompletionProposal> result,
			final IErlFunction function, final boolean arityOnly) {
		addFunctionCompletion(offset, aprefix, function.getFunction(), function
				.getComment(), arityOnly, arityOnly ? null
				: getParameterNames(function), result);
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

	private void addFunctionCompletion(final int offset, final String prefix,
			final ErlangFunction function, final String comment,
			final boolean arityOnly, final List<String> parameterNames,
			final List<ICompletionProposal> result) {
		if (function.name.startsWith(prefix)) {
			final int offs = function.name.length() - prefix.length();

			final List<Point> offsetsAndLengths = new ArrayList<Point>();
			if (!arityOnly) {
				addOffsetsAndLengths(parameterNames, offset + offs + 1,
						offsetsAndLengths);
			}
			final String funWithArity = function.getNameWithArity();
			String funWithParameters = arityOnly ? funWithArity
					: getNameWithParameters(function.name, parameterNames);
			funWithParameters = funWithParameters.substring(prefix.length());
			addFunctionCompletion(offset, result, funWithArity, comment,
					funWithParameters, offsetsAndLengths);
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

	private void addOffsetsAndLengths(final List<String> parameterNames,
			int replacementOffset, final List<Point> result) {
		for (final String par : parameterNames) {
			result.add(new Point(replacementOffset, par.length()));
			replacementOffset += par.length() + 2;
		}
	}

	private void addOffsetsAndLengths(final OtpErlangList parOffsets,
			final int replacementOffset, final List<Point> result) {
		for (final OtpErlangObject i : parOffsets) {
			final OtpErlangTuple t = (OtpErlangTuple) i;
			final OtpErlangLong offset = (OtpErlangLong) t.elementAt(0);
			final OtpErlangLong length = (OtpErlangLong) t.elementAt(1);
			try {
				result.add(new Point(offset.intValue() + replacementOffset,
						length.intValue()));
			} catch (final OtpErlangRangeException e) {
			}
		}
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

	private String getPrefix(final String before) {
		for (int n = before.length() - 1; n >= 0; --n) {
			final char c = before.charAt(n);
			if (!isErlangIdentifierChar(c)) {
				return before.substring(n + 1);
			}
		}
		return before;
	}

	private String getBefore(final ITextViewer viewer, final IDocument doc,
			final int offset) {
		try {
			if (module != null) {
				try {
					IErlElement element = module.getElementAt(offset);
					if (element instanceof ISourceReference) {
						ISourceReference sr = (ISourceReference) element;
						int start = sr.getSourceRange().getOffset();
						if (start <= offset) {
							return doc.get(start, offset - start);
						}
					}
				} catch (ErlModelException e) {
				}
			}
			for (int n = offset - 1; n >= 0; --n) {
				final char c = doc.getChar(n);
				final int type = Character.getType(c);
				if (type == Character.LINE_SEPARATOR
						|| type == Character.PARAGRAPH_SEPARATOR
						|| type == Character.CONTROL) {
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

	private static boolean isErlangIdentifierChar(final char char1) {
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

	// private void initPathVars() {
	// pathVars = getPathVars();
	// }

	/**
	 * @return
	 */
	// public static ArrayList<Tuple> getPathVars() {
	// final IPathVariableManager pvm = ResourcesPlugin.getWorkspace()
	// .getPathVariableManager();
	// final String[] names = pvm.getPathVariableNames();
	// final ArrayList<Tuple> pv = new ArrayList<Tuple>(names.length);
	// for (final String name : names) {
	// pv.add(new Tuple().add(name).add(pvm.getValue(name).toOSString()));
	// }
	// return pv;
	// }
	//
	// public void pathVariableChanged(final IPathVariableChangeEvent event) {
	// initPathVars();
	// }
	private void initStyleSheet() {
		final Bundle bundle = Platform.getBundle(ErlideUIPlugin.PLUGIN_ID);
		fgStyleSheet = bundle.getEntry("/edoc.css"); //$NON-NLS-1$
		if (fgStyleSheet != null) {
			try {
				fgStyleSheet = FileLocator.toFileURL(fgStyleSheet);
			} catch (final Exception e) {
			}
		}
	}

}
