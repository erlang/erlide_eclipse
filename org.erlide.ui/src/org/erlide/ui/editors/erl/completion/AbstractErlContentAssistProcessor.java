package org.erlide.ui.editors.erl.completion;

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
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.swt.graphics.Point;
import org.erlide.backend.IBackend;
import org.erlide.core.model.erlang.IErlFunction;
import org.erlide.core.model.erlang.IErlFunctionClause;
import org.erlide.core.model.erlang.IErlImport;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.erlang.IErlPreprocessorDef;
import org.erlide.core.model.erlang.IErlRecordDef;
import org.erlide.core.model.erlang.IErlRecordField;
import org.erlide.core.model.erlang.ISourceRange;
import org.erlide.core.model.erlang.ISourceReference;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlElement.Kind;
import org.erlide.core.model.root.IErlElementLocator;
import org.erlide.core.model.root.IErlProject;
import org.erlide.core.model.util.CoreUtil;
import org.erlide.core.model.util.ErlangFunction;
import org.erlide.core.model.util.ModelUtils;
import org.erlide.core.services.codeassist.ErlideContextAssist;
import org.erlide.core.services.codeassist.ErlideContextAssist.RecordCompletion;
import org.erlide.core.services.search.ErlideDoc;
import org.erlide.jinterface.ErlLogger;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.prefs.plugin.NavigationPreferencePage;
import org.erlide.ui.templates.ErlTemplateCompletionProcessor;
import org.erlide.ui.util.eclipse.text.HTMLPrinter;
import org.erlide.utils.StringUtils;
import org.erlide.utils.Util;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

public abstract class AbstractErlContentAssistProcessor {

    public static class CompletionNameComparer implements
            Comparator<ICompletionProposal> {

        @Override
        public int compare(final ICompletionProposal o1,
                final ICompletionProposal o2) {
            final String s1 = o1.getDisplayString();
            final String s2 = o2.getDisplayString();
            return s1.compareTo(s2);
        }

    }

    static boolean isErlangIdentifierChar(final char char1) {
        return Character.isJavaIdentifierPart(char1);
    }

    static boolean filterImported(final IErlImport erlImport,
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

    protected final ISourceViewer sourceViewer;
    protected final IErlModule module;

    protected enum Kinds {
        //@formatter:off
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
        UNEXPORTED_ONLY, 
        INCLUDES, 
        INCLUDE_LIBS
        //@formatter:on
    }

    protected static final List<ICompletionProposal> EMPTY_COMPLETIONS = new ArrayList<ICompletionProposal>();
    protected final CompletionNameComparer completionNameComparer = new CompletionNameComparer();
    protected final ContentAssistant contentAssistant;

    public AbstractErlContentAssistProcessor(final ISourceViewer sourceViewer,
            final IErlModule module, final ContentAssistant contentAssistant) {
        this.sourceViewer = sourceViewer;
        this.module = module;
        this.contentAssistant = contentAssistant;
    }

    protected List<ICompletionProposal> getModules(final IBackend backend,
            final int offset, final String prefix, final Kinds kind)
            throws ErlModelException {
        final List<ICompletionProposal> result = Lists.newArrayList();
        if (module != null) {
            final IErlProject project = module.getProject();
            final boolean includes = kind == Kinds.INCLUDES
                    || kind == Kinds.INCLUDE_LIBS;
            final List<String> names = ModelUtils.findUnitsWithPrefix(prefix,
                    project, kind != Kinds.INCLUDES, includes);
            final OtpErlangObject res = ErlideDoc.getModules(backend, prefix,
                    names, includes);
            if (res instanceof OtpErlangList) {
                final OtpErlangList resList = (OtpErlangList) res;
                for (final OtpErlangObject o : resList) {
                    if (o instanceof OtpErlangString) {
                        final OtpErlangString s = (OtpErlangString) o;
                        final String suffix = includes ? "" : ":";
                        final String cpl = quoted(s.stringValue() + suffix,
                                kind);
                        final int prefixLength = prefix.length();
                        result.add(new CompletionProposal(cpl, offset
                                - prefixLength, prefixLength, cpl.length()));
                    }
                }
            }
        }
        return result;
    }

    protected abstract String quoted(String string, Kinds kind);

    public ICompletionProposal[] computeCompletionProposals(
            final ITextViewer viewer, final int offset) {
        if (module == null) {
            return null;
        }
        try {
            final IDocument doc = viewer.getDocument();
            String before = getBefore(viewer, doc, offset);
            ErlLogger.debug("computeCompletionProposals before %s", before);
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
            Set<Kinds> flags = EnumSet.noneOf(Kinds.class);
            int pos;
            String moduleOrRecord = null;
            final IErlProject erlProject = module.getProject();
            final IProject project = erlProject != null ? erlProject
                    .getWorkspaceProject() : null;
            IErlElement element = getElementAt(offset);
            for (int i = 1; element == null && i <= 15; ++i) {
                element = getElementAt(offset - i);
            }
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
                ErlLogger.debug("element %s", element);
                if (element != null) {
                    switch (element.getKind()) {
                    case EXPORT:
                        flags = EnumSet.of(Kinds.DECLARED_FUNCTIONS,
                                Kinds.ARITY_ONLY, Kinds.UNEXPORTED_ONLY);
                        break;
                    case IMPORT:
                        final IErlImport i = (IErlImport) element;
                        moduleOrRecord = i.getImportModule();
                        flags = EnumSet.of(Kinds.EXTERNAL_FUNCTIONS,
                                Kinds.ARITY_ONLY);
                        break;
                    case FUNCTION:
                    case CLAUSE:
                        flags = EnumSet.of(Kinds.MODULES);
                        if (module != null) {
                            flags = Sets.union(flags, EnumSet.of(
                                    Kinds.VARIABLES, Kinds.DECLARED_FUNCTIONS,
                                    Kinds.IMPORTED_FUNCTIONS,
                                    Kinds.AUTO_IMPORTED_FUNCTIONS));

                        }
                        break;
                    case ATTRIBUTE:
                        if (element.getName().equals("include")) {
                            flags = EnumSet.of(Kinds.INCLUDES);
                        } else if (element.getName().equals("include_lib")) {
                            flags = EnumSet.of(Kinds.INCLUDE_LIBS);
                        }
                        break;
                    default:
                        break;
                    }
                }
            }
            flags = filterFlags(flags);
            result = addCompletions(flags, offset, before, moduleOrRecord, pos,
                    fieldsSoFar, erlProject, project);
            final ErlTemplateCompletionProcessor t = new ErlTemplateCompletionProcessor(
                    doc, offset - before.length(), before.length());
            result.addAll(Arrays.asList(t.computeCompletionProposals(viewer,
                    offset)));
            if (result.size() == 0) {
                ErlLogger.debug("no results");
                return getNoCompletion(offset);
            } else {
                ErlLogger.debug("%d results", result.size());
                return result.toArray(new ICompletionProposal[result.size()]);
            }
        } catch (final Exception e) {
            ErlLogger.warn(e);
            return null;
        }
    }

    protected abstract Set<Kinds> filterFlags(Set<Kinds> flags);

    private ICompletionProposal[] getNoCompletion(final int offset) {
        return new ICompletionProposal[] { new DummyCompletionProposal(offset) };
    }

    private List<ICompletionProposal> addCompletions(final Set<Kinds> flags,
            final int offset, final String prefix, final String moduleOrRecord,
            final int pos, final List<String> fieldsSoFar,
            final IErlProject erlProject, final IProject project)
            throws CoreException, OtpErlangRangeException, BadLocationException {
        final IBackend backend = CoreUtil.getBuildOrIdeBackend(project);
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
            addSorted(result,
                    getModules(backend, offset, prefix, Kinds.MODULES));
        }
        if (flags.contains(Kinds.INCLUDES)) {
            addSorted(result,
                    getModules(backend, offset, prefix, Kinds.INCLUDES));
        }
        if (flags.contains(Kinds.INCLUDE_LIBS)) {
            addSorted(result,
                    getModules(backend, offset, prefix, Kinds.INCLUDE_LIBS));
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

    String getBefore(final ITextViewer viewer, final IDocument doc,
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

    protected IErlElement getElementAt(final int offset) {
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

    String getPrefix(final String before) {
        for (int n = before.length() - 1; n >= 0; --n) {
            final char c = before.charAt(n);
            if (!isErlangIdentifierChar(c) && c != '?') {
                return before.substring(n + 1);
            }
        }
        return before;
    }

    List<ICompletionProposal> getDeclaredFunctions(final int offset,
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

    List<ICompletionProposal> getVariables(final IBackend b, final int offset,
            final String prefix) throws ErlModelException, BadLocationException {
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

    List<ICompletionProposal> getMacroOrRecordCompletions(final int offset,
            final String prefix, final Kind kind, final IErlProject erlProject,
            final IProject project) {
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

    List<ICompletionProposal> getExternalCallCompletions(final IBackend b,
            final IErlProject project, String moduleName, final int offset,
            final String prefix, final boolean arityOnly)
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

    List<ICompletionProposal> getRecordFieldCompletions(
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

    void addIfMatches(final String name, final String prefix, final int offset,
            final List<ICompletionProposal> result) {
        final int length = prefix.length();
        if (name.regionMatches(true, 0, prefix, 0, length)) {
            result.add(new CompletionProposal(name, offset - length, length,
                    name.length()));
        }
    }

    List<ICompletionProposal> getAutoImportedFunctions(final IBackend backend,
            final int offset, final String prefix) {
        final String stateDir = ErlideUIPlugin.getDefault().getStateLocation()
                .toString();
        final OtpErlangObject res = ErlideDoc.getProposalsWithDoc(backend,
                "<auto_imported>", prefix, stateDir);
        final List<ICompletionProposal> result = new ArrayList<ICompletionProposal>();
        addFunctionProposalsWithDoc(offset, prefix, result, res, null, false);
        return result;
    }

    List<ICompletionProposal> getImportedFunctions(final IBackend backend,
            final int offset, final String prefix) {
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

    boolean addFunctionsFromModule(final int offset, final String prefix,
            final boolean arityOnly, final List<ICompletionProposal> proposals,
            final IErlModule m) {
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

    void addFunctionProposalsWithDoc(final int offset, final String aprefix,
            final List<ICompletionProposal> result, final OtpErlangObject res,
            final IErlImport erlImport, final boolean arityOnly) {
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
                        docStr = HTMLPrinter.asHtml(Util.stringValue(elt));
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

    /**
     * @param offset
     * @param result
     * @param funWithArity
     * @param docStr
     * @param funWithParameters
     * @param offsetsAndLengths
     * @param cursorPosition
     */
    protected void addFunctionCompletion(final int offset,
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

    void addFunctionCompletion(final int offset, final String aprefix,
            final List<ICompletionProposal> result,
            final IErlFunction function, final boolean arityOnly) {
        addFunctionCompletion(offset, aprefix, function.getFunction(),
                function.getComment(), arityOnly, arityOnly ? null
                        : getParameterNames(function), result);
    }

    List<String> getParameterNames(final IErlFunction function) {
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

    void addFunctionCompletion(final int offset, final String prefix,
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
            addFunctionCompletion(offset, result, funWithArity,
                    HTMLPrinter.asHtml(comment), funWithParameters,
                    offsetsAndLengths);
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

    void addOffsetsAndLengths(final OtpErlangList parOffsets,
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
}
