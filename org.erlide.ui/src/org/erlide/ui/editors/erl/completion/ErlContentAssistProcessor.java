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
import java.util.EnumSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.IPreferenceChangeListener;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.swt.graphics.Point;
import org.eclipse.ui.services.IDisposable;
import org.erlide.core.common.StringUtils;
import org.erlide.core.common.Util;
import org.erlide.core.model.erlang.IErlFunction;
import org.erlide.core.model.erlang.IErlFunctionClause;
import org.erlide.core.model.erlang.IErlImport;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.erlang.IErlPreprocessorDef;
import org.erlide.core.model.erlang.IErlRecordDef;
import org.erlide.core.model.erlang.IErlRecordField;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlElement.Kind;
import org.erlide.core.model.root.IErlElementLocator;
import org.erlide.core.model.root.IErlProject;
import org.erlide.core.model.root.ISourceRange;
import org.erlide.core.model.root.ISourceReference;
import org.erlide.core.model.util.CoreUtil;
import org.erlide.core.model.util.ErlangFunction;
import org.erlide.core.model.util.ModelUtils;
import org.erlide.core.rpc.IRpcCallSite;
import org.erlide.core.services.codeassist.ErlideContextAssist;
import org.erlide.core.services.codeassist.ErlideContextAssist.RecordCompletion;
import org.erlide.core.services.search.ErlideDoc;
import org.erlide.jinterface.ErlLogger;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.prefs.plugin.CodeAssistPreferences;
import org.erlide.ui.prefs.plugin.NavigationPreferencePage;
import org.erlide.ui.templates.ErlTemplateCompletionProcessor;
import org.erlide.ui.util.eclipse.text.HTMLPrinter;
import org.osgi.framework.Bundle;
import org.osgi.service.prefs.BackingStoreException;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

public class ErlContentAssistProcessor implements IContentAssistProcessor,
        IDisposable {

    public static class CompletionNameComparer implements
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

    enum Kinds {
        DECLARED_FUNCTIONS,
        EXTERNAL_FUNCTIONS,
        VARIABLES,
        RECORD_FIELDS,
        RECORD_DEFS,
        MODULES,
        MACRO_DEFS,
        IMPORTED_FUNCTIONS,
        AUTO_IMPORTED_FUNCTIONS,
        ARITY_ONLY,
        UNEXPORTED_ONLY
    }

    // private static final int DECLARED_FUNCTIONS = 1;
    // private static final int EXTERNAL_FUNCTIONS = 2;
    // private static final int VARIABLES = 4;
    // private static final int RECORD_FIELDS = 8;
    // private static final int RECORD_DEFS = 0x10;
    // private static final int MODULES = 0x20;
    // private static final int MACRO_DEFS = 0x40;
    // private static final int IMPORTED_FUNCTIONS = 0x80;
    // private static final int AUTO_IMPORTED_FUNCTIONS = 0x100;
    //
    // private static final int ARITY_ONLY = 0x1000;
    // private static final int UNEXPORTED_ONLY = 0x2000;

    private static final List<ICompletionProposal> EMPTY_COMPLETIONS = new ArrayList<ICompletionProposal>();

    private final CompletionNameComparer completionNameComparer = new CompletionNameComparer();
    private char[] fCompletionProposalAutoActivationCharacters;
    private final IPreferenceChangeListener fPreferenceChangeListener;
    private final ContentAssistant contentAssistant;

    public ErlContentAssistProcessor(final ISourceViewer sourceViewer,
            final IErlModule module, final ContentAssistant contentAssistant) {
        this.sourceViewer = sourceViewer;
        this.module = module;
        this.contentAssistant = contentAssistant;
        // this.externalModules = externalModules;
        // this.externalIncludes = externalIncludes;
        fPreferenceChangeListener = new PreferenceChangeListener();
        final IEclipsePreferences node = CodeAssistPreferences.getNode();
        node.addPreferenceChangeListener(fPreferenceChangeListener);
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
            List<String> fieldsSoFar = null;
            List<ICompletionProposal> result;
            Set<Kinds> flags;
            int pos;
            String moduleOrRecord = null;
            final IErlProject erlProject = module.getProject();
            final IProject project = erlProject != null ? erlProject
                    .getWorkspaceProject() : null;
            final IErlElement element = getElementAt(offset);
            RecordCompletion rc = null;
            if (hashMarkPos >= 0) {
                rc = ErlideContextAssist.checkRecordCompletion(
                        CoreUtil.getBuildOrIdeBackend(project), before);
            }
            if (rc != null && rc.isNameWanted()) {
                flags = EnumSet.of(Kinds.RECORD_DEFS);
                pos = hashMarkPos;
                before = rc.getPrefix();
            } else if (rc != null && rc.isFieldWanted()) {
                flags = EnumSet.of(Kinds.RECORD_FIELDS);
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
                fieldsSoFar = rc.getFields();
            } else if (colonPos > commaPos && colonPos > parenPos) {
                moduleOrRecord = StringUtils.unquote(getPrefix(before
                        .substring(0, colonPos)));
                flags = EnumSet.of(Kinds.EXTERNAL_FUNCTIONS);
                pos = colonPos;
                before = before.substring(colonPos + 1);
            } else if (interrogationMarkPos > hashMarkPos
                    && interrogationMarkPos > commaPos
                    && interrogationMarkPos > colonPos) {
                flags = EnumSet.of(Kinds.MACRO_DEFS);
                pos = interrogationMarkPos;
                before = before.substring(interrogationMarkPos + 1);
            } else {
                pos = colonPos;
                before = prefix;
                if (element != null) {
                    if (element.getKind() == IErlElement.Kind.EXPORT) {
                        flags = EnumSet.of(Kinds.DECLARED_FUNCTIONS,
                                Kinds.ARITY_ONLY, Kinds.UNEXPORTED_ONLY);
                    } else if (element.getKind() == IErlElement.Kind.IMPORT) {
                        final IErlImport i = (IErlImport) element;
                        moduleOrRecord = i.getImportModule();
                        flags = EnumSet.of(Kinds.EXTERNAL_FUNCTIONS,
                                Kinds.ARITY_ONLY);
                    } else if (element.getKind() == IErlElement.Kind.FUNCTION
                            || element.getKind() == IErlElement.Kind.CLAUSE) {
                        flags = EnumSet.of(Kinds.MODULES);
                        if (module != null) {
                            flags = Sets.union(flags, EnumSet.of(
                                    Kinds.VARIABLES, Kinds.DECLARED_FUNCTIONS,
                                    Kinds.IMPORTED_FUNCTIONS,
                                    Kinds.AUTO_IMPORTED_FUNCTIONS));

                        }
                    } else {
                        flags = EnumSet.noneOf(Kinds.class);
                    }
                } else {
                    flags = EnumSet.noneOf(Kinds.class);
                }
            }
            result = addCompletions(flags, offset, before, moduleOrRecord, pos,
                    fieldsSoFar, erlProject, project);
            final ErlTemplateCompletionProcessor t = new ErlTemplateCompletionProcessor(
                    doc, offset - before.length(), before.length());
            result.addAll(Arrays.asList(t.computeCompletionProposals(viewer,
                    offset)));
            if (result.size() == 0) {
                return getNoCompletion(offset);
            } else {
                return result.toArray(new ICompletionProposal[result.size()]);
            }
        } catch (final Exception e) {
            ErlLogger.warn(e);
            return null;
        }
    }

    private ICompletionProposal[] getNoCompletion(final int offset) {
        return new ICompletionProposal[] { new DummyCompletionProposal(offset) };
    }

    private List<ICompletionProposal> addCompletions(final Set<Kinds> flags,
            final int offset, final String prefix, final String moduleOrRecord,
            final int pos, final List<String> fieldsSoFar,
            final IErlProject erlProject, final IProject project)
            throws CoreException, OtpErlangRangeException, BadLocationException {
        final IRpcCallSite backend = CoreUtil.getBuildOrIdeBackend(project);
        final List<ICompletionProposal> result = new ArrayList<ICompletionProposal>();
        if (flags.contains(Kinds.DECLARED_FUNCTIONS)) {
            addSorted(
                    result,
                    getDeclaredFunctions(offset, prefix,
                            flags.contains(Kinds.UNEXPORTED_ONLY),
                            flags.contains(Kinds.ARITY_ONLY)));
        }
        if (flags.contains(Kinds.VARIABLES)) {
            addSorted(result, getVariables(backend, offset, prefix));
        }
        if (flags.contains(Kinds.IMPORTED_FUNCTIONS)) {
            addSorted(result, getImportedFunctions(backend, offset, prefix));
        }
        if (flags.contains(Kinds.AUTO_IMPORTED_FUNCTIONS)) {
            addSorted(result, getAutoImportedFunctions(backend, offset, prefix));
        }
        if (flags.contains(Kinds.MODULES)) {
            addSorted(result, getModules(backend, offset, prefix));
        }
        if (flags.contains(Kinds.RECORD_DEFS)) {
            addSorted(
                    result,
                    getMacroOrRecordCompletions(offset, prefix,
                            IErlElement.Kind.RECORD_DEF, erlProject, project));
        }
        if (flags.contains(Kinds.RECORD_FIELDS)) {
            addSorted(
                    result,
                    getRecordFieldCompletions(moduleOrRecord, offset, prefix,
                            pos, fieldsSoFar));
        }
        if (flags.contains(Kinds.MACRO_DEFS)) {
            addSorted(
                    result,
                    getMacroOrRecordCompletions(offset, prefix,
                            IErlElement.Kind.MACRO_DEF, erlProject, project));
        }
        if (flags.contains(Kinds.EXTERNAL_FUNCTIONS)) {
            addSorted(
                    result,
                    getExternalCallCompletions(backend, erlProject,
                            moduleOrRecord, offset, prefix,
                            flags.contains(Kinds.ARITY_ONLY)));
        }
        return result;
    }

    private void addSorted(final List<ICompletionProposal> result,
            final List<ICompletionProposal> completions) {
        Collections.sort(completions, completionNameComparer);
        result.addAll(completions);
    }

    private List<ICompletionProposal> getRecordFieldCompletions(
            final String recordName, final int offset, final String prefix,
            final int hashMarkPos, final List<String> fieldsSoFar) {
        if (module == null) {
            return EMPTY_COMPLETIONS;
        }
        IErlPreprocessorDef pd;
        try {
            pd = ModelUtils.findPreprocessorDef(module, recordName,
                    Kind.RECORD_DEF);
        } catch (final CoreException e) {
            return EMPTY_COMPLETIONS;
        }
        if (pd instanceof IErlRecordDef) {
            final List<ICompletionProposal> result = new ArrayList<ICompletionProposal>();
            try {
                for (final IErlElement i : pd.getChildren()) {
                    final IErlRecordField field = (IErlRecordField) i;
                    final String fieldName = field.getFieldName();
                    if (!fieldsSoFar.contains(fieldName)) {
                        addIfMatches(fieldName, prefix, offset, result);
                    }
                }
            } catch (final ErlModelException e) {
            }
            return result;
        }
        return EMPTY_COMPLETIONS;
    }

    private void addIfMatches(final String name, final String prefix,
            final int offset, final List<ICompletionProposal> result) {
        final int length = prefix.length();
        if (name.regionMatches(true, 0, prefix, 0, length)) {
            result.add(new CompletionProposal(name, offset - length, length,
                    name.length()));
        }
    }

    private List<ICompletionProposal> getModules(final IRpcCallSite backend,
            final int offset, final String prefix) throws ErlModelException {
        final List<ICompletionProposal> result = Lists.newArrayList();
        if (module != null) {
            final IErlProject project = module.getProject();
            final List<String> names = ModelUtils.findModulesWithPrefix(prefix,
                    project, true);
            final OtpErlangObject res = ErlideDoc.getModules(backend, prefix,
                    names);
            if (res instanceof OtpErlangList) {
                final OtpErlangList resList = (OtpErlangList) res;
                for (final OtpErlangObject o : resList) {
                    if (o instanceof OtpErlangString) {
                        final OtpErlangString s = (OtpErlangString) o;
                        final String cpl = s.stringValue() + ":";
                        final int prefixLength = prefix.length();
                        result.add(new CompletionProposal(cpl, offset
                                - prefixLength, prefixLength, cpl.length()));
                    }
                }
            }
        }
        return result;
    }

    private List<ICompletionProposal> getAutoImportedFunctions(
            final IRpcCallSite backend, final int offset, final String prefix) {
        final String stateDir = ErlideUIPlugin.getDefault().getStateLocation()
                .toString();
        final OtpErlangObject res = ErlideDoc.getProposalsWithDoc(backend,
                "<auto_imported>", prefix, stateDir);
        final List<ICompletionProposal> result = new ArrayList<ICompletionProposal>();
        addFunctionProposalsWithDoc(offset, prefix, result, res, null, false);
        return result;
    }

    private List<ICompletionProposal> getImportedFunctions(
            final IRpcCallSite backend, final int offset, final String prefix) {
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

    private List<ICompletionProposal> getVariables(final IRpcCallSite b,
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

    private List<ICompletionProposal> getMacroOrRecordCompletions(
            final int offset, final String prefix, final Kind kind,
            final IErlProject erlProject, final IProject project) {
        if (module == null) {
            return EMPTY_COMPLETIONS;
        }
        final List<ICompletionProposal> result = new ArrayList<ICompletionProposal>();
        try {
            final List<IErlPreprocessorDef> defs = ModelUtils
                    .getAllPreprocessorDefs(module, kind);
            for (final IErlPreprocessorDef pd : defs) {
                final String name = pd.getDefinedName();
                addIfMatches(name, prefix, offset, result);
            }
        } catch (final CoreException e) {
            ErlLogger.error(e);
        }
        if (kind == Kind.MACRO_DEF) {
            final String[] names = ModelUtils.getPredefinedMacroNames();
            for (final String name : names) {
                addIfMatches(name, prefix, offset, result);
            }
        }
        return result;
    }

    private List<ICompletionProposal> getExternalCallCompletions(
            final IRpcCallSite b, final IErlProject project, String moduleName,
            final int offset, final String prefix, final boolean arityOnly)
            throws OtpErlangRangeException, CoreException {
        moduleName = ModelUtils.resolveMacroValue(moduleName, module);
        // we have an external call
        final List<ICompletionProposal> result = new ArrayList<ICompletionProposal>();
        final IErlProject erlProject = module == null ? null : module
                .getProject();
        final boolean checkAllProjects = NavigationPreferencePage
                .getCheckAllProjects();
        final IErlModule theModule = ModelUtils.findModule(erlProject,
                moduleName, null,
                checkAllProjects ? IErlElementLocator.Scope.ALL_PROJECTS
                        : IErlElementLocator.Scope.REFERENCED_PROJECTS);
        if (theModule != null) {
            if (ModelUtils.isOtpModule(theModule)) {
                final String stateDir = ErlideUIPlugin.getDefault()
                        .getStateLocation().toString();
                final OtpErlangObject res = ErlideDoc.getProposalsWithDoc(b,
                        moduleName, prefix, stateDir);
                addFunctionProposalsWithDoc(offset, prefix, result, res, null,
                        arityOnly);
            } else {
                addFunctionsFromModule(offset, prefix, arityOnly, result,
                        theModule);
            }
        }
        return result;
    }

    private boolean addFunctionsFromModule(final int offset,
            final String prefix, final boolean arityOnly,
            final List<ICompletionProposal> proposals, final IErlModule m) {
        boolean result = false;
        try {
            m.open(null);
            for (final IErlElement e : m.getChildren()) {
                if (e instanceof IErlFunction) {
                    final IErlFunction f = (IErlFunction) e;
                    if (f.isExported()) {
                        addFunctionCompletion(offset, prefix, proposals, f,
                                arityOnly);
                        result = true;
                    }
                }
            }
        } catch (final ErlModelException e) {
            e.printStackTrace();
        }
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
                        final StringBuffer sb = new StringBuffer(
                                Util.stringValue(elt));
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
        addFunctionCompletion(offset, aprefix, function.getFunction(),
                function.getComment(), arityOnly, arityOnly ? null
                        : getParameterNames(function), result);
    }

    private List<String> getParameterNames(final IErlFunction function) {
        final List<String> parameters = function.getParameters();
        final int arity = function.getArity();
        final List<String> result = new ArrayList<String>(arity);
        addEmptyParameterNames(arity, result);
        addParametersFromFunctionParameters(parameters, result);
        for (final IErlFunctionClause clause : function.getClauses()) {
            addParametersFromFunctionParameters(clause.getParameters(), result);
        }
        return result;
    }

    private void addParametersFromFunctionParameters(
            final List<String> parameters, final List<String> result) {
        final int n = Math.min(parameters.size(), result.size());
        for (int i = 0; i < n; ++i) {
            if (result.get(i).equals("_")) {
                final String var = parameters.get(i).trim();
                if (looksLikeParameter(var)) {
                    result.set(i, fixVarName(var));
                }
            }
        }
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
        if (function.name.regionMatches(0, prefix, 0, prefix.length())) {
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

    private String getPrefix(final String before) {
        for (int n = before.length() - 1; n >= 0; --n) {
            final char c = before.charAt(n);
            if (!isErlangIdentifierChar(c) && c != '?') {
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
                    final IErlElement element = module.getElementAt(offset);
                    if (element instanceof ISourceReference) {
                        final ISourceReference sr = (ISourceReference) element;
                        final int start = sr.getSourceRange().getOffset();
                        if (start <= offset) {
                            return doc.get(start, offset - start);
                        }
                    }
                } catch (final ErlModelException e) {
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

    public void setToPrefs() {
        final CodeAssistPreferences prefs = new CodeAssistPreferences();
        try {
            prefs.load();
            fCompletionProposalAutoActivationCharacters = prefs
                    .getErlangTriggers().toCharArray();
            contentAssistant.setAutoActivationDelay(prefs.getDelayInMS());
            contentAssistant.enableAutoActivation(prefs.isAutoActivate());
            contentAssistant.setAutoActivationDelay(prefs.getDelayInMS());
        } catch (final BackingStoreException e) {
            fCompletionProposalAutoActivationCharacters = new char[0];
        }
    }

    private class PreferenceChangeListener implements IPreferenceChangeListener {
        public void preferenceChange(final PreferenceChangeEvent event) {
            setToPrefs();
        }
    }

    public char[] getCompletionProposalAutoActivationCharacters() {
        return fCompletionProposalAutoActivationCharacters;
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

    public void dispose() {
        final IEclipsePreferences node = CodeAssistPreferences.getNode();
        node.removePreferenceChangeListener(fPreferenceChangeListener);
    }

}
