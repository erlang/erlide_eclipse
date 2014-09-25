package org.erlide.ui.editors.erl.completion;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.ContentAssistEvent;
import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.contentassist.ICompletionListener;
import org.eclipse.jface.text.contentassist.ICompletionListenerExtension;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.swt.graphics.Point;
import org.erlide.backend.BackendCore;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.erlang.ErlangFunction;
import org.erlide.engine.model.erlang.IErlComment;
import org.erlide.engine.model.erlang.IErlFunction;
import org.erlide.engine.model.erlang.IErlFunctionClause;
import org.erlide.engine.model.erlang.IErlImport;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.erlang.IErlPreprocessorDef;
import org.erlide.engine.model.erlang.IErlRecordDef;
import org.erlide.engine.model.erlang.IErlRecordField;
import org.erlide.engine.model.erlang.ISourceRange;
import org.erlide.engine.model.erlang.ISourceReference;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlElementLocator;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.services.codeassist.RecordCompletion;
import org.erlide.engine.services.search.ModelFindService;
import org.erlide.engine.services.search.OtpDocService;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.internal.information.HoverUtil;
import org.erlide.ui.prefs.plugin.NavigationPreferencePage;
import org.erlide.ui.templates.ErlTemplateCompletionProcessor;
import org.erlide.ui.util.eclipse.text.HTMLPrinter;
import org.erlide.util.ErlLogger;
import org.erlide.util.StringUtils;
import org.erlide.util.Util;
import org.erlide.util.event_tracer.ErlideEventTracer;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.base.Objects;
import com.google.common.collect.Lists;

public abstract class AbstractErlContentAssistProcessor implements
        IContentAssistProcessor {

    public boolean restarted = false;

    private final class CompletionListener implements ICompletionListener,
            ICompletionListenerExtension {

        @Override
        public void assistSessionStarted(final ContentAssistEvent event) {
            if (event.processor != AbstractErlContentAssistProcessor.this) {
                return;
            }
            restarted = false;
        }

        @Override
        public void assistSessionEnded(final ContentAssistEvent event) {
            if (event.processor != AbstractErlContentAssistProcessor.this) {
                return;
            }
            restarted = false;
        }

        @Override
        public void selectionChanged(final ICompletionProposal proposal,
                final boolean smartToggle) {
        }

        @Override
        public void assistSessionRestarted(final ContentAssistEvent event) {
            if (event.processor != AbstractErlContentAssistProcessor.this) {
                return;
            }
            restarted = true;
        }
    }

    public static class CompletionNameComparer implements Comparator<ICompletionProposal> {

        private final String prefix;

        public CompletionNameComparer(final String prefix) {
            this.prefix = prefix;
        }

        @Override
        public int compare(final ICompletionProposal o1, final ICompletionProposal o2) {
            final String s1 = o1.getDisplayString();
            final String s2 = o2.getDisplayString();
            // exact prefix matches get higher priority
            if (s1.startsWith(prefix)) {
                return -1;
            }
            if (s2.startsWith(prefix)) {
                return 1;
            }
            return s1.compareTo(s2);
        }

    }

    static boolean isErlangIdentifierChar(final char char1) {
        return Character.isJavaIdentifierPart(char1);
    }

    static boolean filterImported(final IErlImport erlImport, final String funWithArity) {
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
    protected final IErlProject project;

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
        INCLUDE_LIBS,
        TYPES
        //@formatter:on
    }

    protected static final List<ICompletionProposal> EMPTY_COMPLETIONS = new ArrayList<ICompletionProposal>();
    protected final ContentAssistant contentAssistant;
    private IDocument oldDoc;
    private String oldBefore;
    private int oldSuggestions = -1;

    public AbstractErlContentAssistProcessor(final ISourceViewer sourceViewer,
            final IErlModule module, final IErlProject project,
            final ContentAssistant contentAssistant) {
        this.sourceViewer = sourceViewer;
        this.module = module;
        this.project = project;
        this.contentAssistant = contentAssistant;
        if (contentAssistant != null) {
            contentAssistant.addCompletionListener(new CompletionListener());
        }
    }

    protected List<ICompletionProposal> getModules(final IOtpRpc backend,
            final int offset, final String prefix, final Kinds kind)
            throws ErlModelException {
        final List<ICompletionProposal> result = Lists.newArrayList();
        final boolean includes = kind == Kinds.INCLUDES || kind == Kinds.INCLUDE_LIBS;
        final List<String> names = ErlangEngine.getInstance().getModelUtilService()
                .findUnitsWithPrefix(prefix, project, kind != Kinds.INCLUDES, includes);
        final OtpErlangObject res = ErlangEngine.getInstance()
                .getService(OtpDocService.class)
                .getModules(backend, prefix, names, includes);
        if (res instanceof OtpErlangList) {
            final OtpErlangList resList = (OtpErlangList) res;
            for (final OtpErlangObject o : resList) {
                if (o instanceof OtpErlangString) {
                    final OtpErlangString s = (OtpErlangString) o;
                    final String suffix = includes ? "" : ":";
                    final String cpl = quoted(s.stringValue() + suffix, kind);
                    final int prefixLength = prefix.length();
                    result.add(new CompletionProposal(cpl, offset - prefixLength,
                            prefixLength, cpl.length()));
                }
            }
        }
        return result;
    }

    protected abstract String quoted(String string, Kinds kind);

    @Override
    public ICompletionProposal[] computeCompletionProposals(final ITextViewer viewer,
            final int offset) {
        final String id = Integer.toHexString(viewer.hashCode()) + "@" + offset;
        try {
            ErlideEventTracer.getInstance().traceOperationStart("completion", id);
            try {

                final IDocument doc = viewer.getDocument();
                String before = getBefore(viewer, doc, offset);
                // ErlLogger.debug("computeCompletionProposals before = %s %d %s",
                // before, oldSuggestions, oldDoc);

                if (restarted && offset > 0) {
                    final char last = doc.get(offset - 1, 1).charAt(0);
                    if (last == ',' || last == '.' || last == ';' || last == ')'
                            || last == '(') {
                        return null;
                    }
                }

                if (Objects.equal(oldDoc, doc) && oldBefore != null
                        && before.startsWith(oldBefore) && oldSuggestions == 0) {
                    return getNoCompletion(offset);
                }
                oldDoc = doc;
                oldBefore = before;

                final int commaPos = before.lastIndexOf(',');
                final int colonPos = before.lastIndexOf(':');
                final boolean doubleColon = colonPos >= 0
                        && before.charAt(colonPos - 1) == ':';
                final int hashMarkPos = before.lastIndexOf('#');
                final int dotPos = before.lastIndexOf('.');
                final int parenPos = before.lastIndexOf('(');
                final int leftBracketPos = before.lastIndexOf('{');
                final int interrogationMarkPos = before.lastIndexOf('?');
                final int arrowPos = before.lastIndexOf("->");
                final String prefix = getPrefix(before);
                List<String> fieldsSoFar = null;
                List<ICompletionProposal> result;
                EnumSet<Kinds> flags = EnumSet.noneOf(Kinds.class);
                int pos;
                String moduleOrRecord = null;
                IErlElement element = getElementAt(offset);
                for (int i = 1; element == null && i <= 15; ++i) {
                    element = getElementAt(offset - i);
                }
                RecordCompletion rc = null;
                if (hashMarkPos >= 0) {
                    final IErlProject aproject = project;
                    if (aproject != null) {
                        rc = ErlangEngine
                                .getInstance()
                                .getContextAssistService()
                                .checkRecordCompletion(
                                        BackendCore.getBuildBackend(aproject), before);
                    }
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
                    if (doubleColon) {
                        flags = EnumSet.of(Kinds.TYPES);
                        pos = colonPos;
                        before = before.substring(colonPos + 1);
                    } else {
                        moduleOrRecord = StringUtils.unquote(getPrefix(before.substring(
                                0, colonPos)));
                        flags = EnumSet.of(Kinds.EXTERNAL_FUNCTIONS);
                        pos = colonPos;
                        before = before.substring(colonPos + 1);
                    }
                } else if (interrogationMarkPos > hashMarkPos
                        && interrogationMarkPos > commaPos
                        && interrogationMarkPos > colonPos
                        && interrogationMarkPos > arrowPos) {
                    flags = EnumSet.of(Kinds.MACRO_DEFS);
                    pos = interrogationMarkPos;
                    before = before.substring(interrogationMarkPos + 1);
                } else {
                    pos = colonPos;
                    before = prefix;
                    if (element != null) {
                        switch (element.getKind()) {
                        case EXPORT:
                            flags = EnumSet.of(Kinds.DECLARED_FUNCTIONS,
                                    Kinds.ARITY_ONLY, Kinds.UNEXPORTED_ONLY);
                            break;
                        case IMPORT:
                            final IErlImport i = (IErlImport) element;
                            moduleOrRecord = i.getImportModule();
                            flags = EnumSet
                                    .of(Kinds.EXTERNAL_FUNCTIONS, Kinds.ARITY_ONLY);
                            break;
                        case FUNCTION:
                        case CLAUSE:
                            flags = EnumSet.of(Kinds.MODULES);
                            if (module != null) {
                                flags.addAll(EnumSet.of(Kinds.VARIABLES,
                                        Kinds.DECLARED_FUNCTIONS,
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
                    } else {
                        if (doubleColon) {
                            flags = EnumSet.of(Kinds.TYPES);
                        } else {
                            flags = EnumSet.of(Kinds.MODULES);
                        }
                    }
                }
                flags = filterFlags(flags);
                result = addCompletions(flags, offset, before, moduleOrRecord, pos,
                        fieldsSoFar);
                final ErlTemplateCompletionProcessor t = new ErlTemplateCompletionProcessor(
                        doc, offset - before.length(), before.length());
                result.addAll(Arrays.asList(t.computeCompletionProposals(viewer, offset)));
                oldSuggestions = result.size();
                if (result.isEmpty()) {
                    ErlLogger.debug("no results");
                    return getNoCompletion(offset);
                }
                // ErlLogger.debug("%d results", result.size());
                return result.toArray(new ICompletionProposal[result.size()]);
            } catch (final Exception e) {
                ErlLogger.warn(e);
                return null;
            }
        } finally {
            ErlideEventTracer.getInstance().traceOperationEnd("completion", id);
        }
    }

    protected abstract EnumSet<Kinds> filterFlags(EnumSet<Kinds> flags);

    private ICompletionProposal[] getNoCompletion(final int offset) {
        return new ICompletionProposal[] { new DummyCompletionProposal(offset) };
    }

    private List<ICompletionProposal> addCompletions(final Set<Kinds> flags,
            final int offset, final String prefix, final String moduleOrRecord,
            final int pos, final List<String> fieldsSoFar) throws CoreException,
            BadLocationException {
        final List<ICompletionProposal> result = new ArrayList<ICompletionProposal>();
        final IErlProject aProject = project;
        if (aProject == null) {
            return result;
        }
        final IOtpRpc backend = BackendCore.getBuildBackend(aProject);
        if (flags.contains(Kinds.DECLARED_FUNCTIONS)) {
            addSorted(
                    prefix,
                    result,
                    getDeclaredFunctions(offset, prefix,
                            flags.contains(Kinds.UNEXPORTED_ONLY),
                            flags.contains(Kinds.ARITY_ONLY)));
        }
        if (flags.contains(Kinds.VARIABLES)) {
            addSorted(prefix, result, getVariables(backend, offset, prefix));
        }
        if (flags.contains(Kinds.IMPORTED_FUNCTIONS)) {
            addSorted(prefix, result, getImportedFunctions(backend, offset, prefix));
        }
        if (flags.contains(Kinds.AUTO_IMPORTED_FUNCTIONS)) {
            addSorted(prefix, result, getAutoImportedFunctions(backend, offset, prefix));
        }
        if (flags.contains(Kinds.MODULES)) {
            addSorted(prefix, result, getModules(backend, offset, prefix, Kinds.MODULES));
        }
        if (flags.contains(Kinds.INCLUDES)) {
            addSorted(prefix, result, getModules(backend, offset, prefix, Kinds.INCLUDES));
        }
        if (flags.contains(Kinds.INCLUDE_LIBS)) {
            addSorted(prefix, result,
                    getModules(backend, offset, prefix, Kinds.INCLUDE_LIBS));
        }
        if (flags.contains(Kinds.RECORD_DEFS)) {
            addSorted(
                    prefix,
                    result,
                    getMacroOrRecordCompletions(offset, prefix, ErlElementKind.RECORD_DEF));
        }
        if (flags.contains(Kinds.RECORD_FIELDS)) {
            addSorted(
                    prefix,
                    result,
                    getRecordFieldCompletions(moduleOrRecord, offset, prefix, pos,
                            fieldsSoFar));
        }
        if (flags.contains(Kinds.MACRO_DEFS)) {
            addSorted(prefix, result,
                    getMacroOrRecordCompletions(offset, prefix, ErlElementKind.MACRO_DEF));
        }
        if (flags.contains(Kinds.EXTERNAL_FUNCTIONS)) {
            addSorted(
                    prefix,
                    result,
                    getExternalCallCompletions(backend, moduleOrRecord, offset, prefix,
                            flags.contains(Kinds.ARITY_ONLY)));
        }
        if (flags.contains(Kinds.TYPES)) {
            addSorted(prefix, result,
                    getTypeCompletions(backend, moduleOrRecord, offset, prefix));
        }

        return result;
    }

    private void addSorted(final String prefix, final List<ICompletionProposal> result,
            final List<ICompletionProposal> completions) {
        final CompletionNameComparer completionNameComparer = new CompletionNameComparer(
                prefix);
        Collections.sort(completions, completionNameComparer);
        result.addAll(completions);
    }

    String getBefore(final ITextViewer viewer, final IDocument doc, final int offset) {
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
            ErlLogger.error(e);
        }
        return null;
    }

    String getPrefix(final String before) {
        for (int n = before.length() - 1; n >= 0; --n) {
            final char c = before.charAt(n);
            if (!isErlangIdentifierChar(c) && c != '?' && c != '\'') {
                return before.substring(n + 1);
            }
        }
        return before;
    }

    List<ICompletionProposal> getDeclaredFunctions(final int offset, final String prefix,
            final boolean unexportedOnly, final boolean arityOnly)
            throws ErlModelException {
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

    List<ICompletionProposal> getVariables(final IOtpRpc b, final int offset,
            final String prefix) throws BadLocationException {
        final List<ICompletionProposal> result = new ArrayList<ICompletionProposal>();
        // get variables
        final IErlElement el = getElementAt(offset);
        if (el instanceof ISourceReference) {
            final ISourceRange r = ((ISourceReference) el).getSourceRange();
            final int o = r.getOffset();
            final IDocument doc = sourceViewer.getDocument();
            final int prefixLength = prefix.length();
            final String src = doc.get(o, offset - o - prefixLength);
            final Collection<String> vars = ErlangEngine.getInstance()
                    .getContextAssistService().getVariables(src, prefix);
            for (final String var : vars) {
                result.add(new CompletionProposal(var, offset - prefixLength,
                        prefixLength, var.length()));
            }
        }
        return result;
    }

    List<ICompletionProposal> getMacroOrRecordCompletions(final int offset,
            final String prefix, final ErlElementKind kind) {
        if (module == null) {
            return EMPTY_COMPLETIONS;
        }
        final List<ICompletionProposal> result = new ArrayList<ICompletionProposal>();
        try {
            final List<IErlPreprocessorDef> defs = ErlangEngine.getInstance()
                    .getModelUtilService().getAllPreprocessorDefs(module, kind);
            for (final IErlPreprocessorDef pd : defs) {
                final String name = pd.getDefinedName();
                addIfMatches(name, prefix, offset, result);
            }
        } catch (final CoreException e) {
            ErlLogger.error(e);
        }
        if (kind == ErlElementKind.MACRO_DEF) {
            final String[] names = ErlangEngine.getInstance().getModelUtilService()
                    .getPredefinedMacroNames();
            for (final String name : names) {
                addIfMatches(name, prefix, offset, result);
            }
        }
        return result;
    }

    List<ICompletionProposal> getExternalCallCompletions(final IOtpRpc b,
            final String moduleName0, final int offset, final String prefix,
            final boolean arityOnly) throws CoreException {
        final ModelFindService modelFindService = ErlangEngine.getInstance()
                .getModelFindService();
        final String moduleName = modelFindService.resolveMacroValue(moduleName0, module);
        // we have an external call
        final List<ICompletionProposal> result = new ArrayList<ICompletionProposal>();
        final boolean checkAllProjects = NavigationPreferencePage.getCheckAllProjects();
        final IErlElementLocator model = ErlangEngine.getInstance().getModel();
        final IErlModule theModule = modelFindService.findModule(model, project,
                moduleName, null,
                checkAllProjects ? IErlElementLocator.Scope.ALL_PROJECTS
                        : IErlElementLocator.Scope.REFERENCED_PROJECTS);
        if (theModule != null) {
            if (ErlangEngine.getInstance().getModelUtilService().isOtpModule(theModule)) {
                final String stateDir = ErlideUIPlugin.getDefault().getStateLocation()
                        .toString();
                final OtpErlangObject res = ErlangEngine.getInstance()
                        .getService(OtpDocService.class)
                        .getProposalsWithDoc(b, moduleName, prefix, stateDir);
                addFunctionProposalsWithDoc(offset, prefix, result, res, null, arityOnly);
            } else {
                addFunctionsFromModule(offset, prefix, arityOnly, result, theModule);
            }
        }
        return result;
    }

    List<ICompletionProposal> getRecordFieldCompletions(final String recordName,
            final int offset, final String prefix, final int hashMarkPos,
            final List<String> fieldsSoFar) {
        if (module == null) {
            return EMPTY_COMPLETIONS;
        }
        IErlPreprocessorDef pd;
        try {
            pd = ErlangEngine.getInstance().getModelFindService()
                    .findPreprocessorDef(module, recordName, ErlElementKind.RECORD_DEF);
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
            result.add(new CompletionProposal(name, offset - length, length, name
                    .length()));
        }
    }

    List<ICompletionProposal> getAutoImportedFunctions(final IOtpRpc backend,
            final int offset, final String prefix) {
        final String stateDir = ErlangEngine.getInstance().getStateDir();
        final OtpErlangObject res = ErlangEngine.getInstance()
                .getService(OtpDocService.class)
                .getProposalsWithDoc(backend, "<auto_imported>", prefix, stateDir);
        final List<ICompletionProposal> result = new ArrayList<ICompletionProposal>();
        addFunctionProposalsWithDoc(offset, prefix, result, res, null, false);
        return result;
    }

    List<ICompletionProposal> getImportedFunctions(final IOtpRpc backend,
            final int offset, final String prefix) {
        final String stateDir = ErlangEngine.getInstance().getStateDir();
        final List<ICompletionProposal> result = new ArrayList<ICompletionProposal>();
        for (final IErlImport imp : module.getImports()) {
            final OtpErlangObject res = ErlangEngine
                    .getInstance()
                    .getService(OtpDocService.class)
                    .getProposalsWithDoc(backend, imp.getImportModule(), prefix, stateDir);
            addFunctionProposalsWithDoc(offset, prefix, result, res, imp, false);
        }
        return result;
    }

    List<ICompletionProposal> getTypeCompletions(final IOtpRpc backend,
            final String moduleOrRecord, final int offset, final String prefix) {
        final List<ICompletionProposal> result = new ArrayList<ICompletionProposal>();
        for (final String builtin : getBuiltinTypeCompletions()) {
            if (builtin.startsWith(prefix.trim())) {
                result.add(new CompletionProposal(builtin, offset - prefix.length(),
                        prefix.length(), builtin.length()));
            }
        }
        // TODO provide types completions from workspace
        return result;
    }

    List<String> getBuiltinTypeCompletions() {
        final List<String> result = Lists.newArrayList("any()", "binary()",
                "bitstring()", "boolean()", "byte()", "char()", "float()", "fun()",
                "integer()", "iolist()", "list()", "maybe_improper_list()", "mfa()",
                "module()", "neg_integer()", "no_return()", "node()",
                "non_neg_integer()", "none()", "nonempty_list()", "number()", "pid()",
                "port()", "pos_integer()", "reference()", "term()", "timeout()",
                "tuple()");
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
                        addFunctionCompletion(offset, prefix, proposals, f, arityOnly);
                        result = true;
                    }
                }
            }
        } catch (final ErlModelException e) {
            ErlLogger.error(e);
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

                funWithParameters = funWithParameters.substring(aprefix.length());
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
        if (!offsetsAndLengths.isEmpty()) {
            cursorPosition = offsetsAndLengths.get(0).x;
        }

        // first check if it's already there...
        for (final ICompletionProposal c : result) {
            if (c.getDisplayString().equals(funWithArity)) {
                return;
            }
        }
        final ICompletionProposal c = new ErlCompletionProposal(offsetsAndLengths,
                funWithArity, funWithParameters, offset, 0, cursorPosition, null, null,
                docStr, sourceViewer);

        result.add(c);
    }

    void addFunctionCompletion(final int offset, final String aprefix,
            final List<ICompletionProposal> result, final IErlFunction function,
            final boolean arityOnly) {
        addFunctionCompletion(offset, aprefix, function.getFunction(),
                function.getComments(), arityOnly, arityOnly ? null
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

    private void addParametersFromFunctionParameters(final List<String> parameters,
            final List<String> result) {
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

    private void addEmptyParameterNames(final int arity, final List<String> result) {
        for (int i = result.size(); i < arity; ++i) {
            result.add("_");
        }
    }

    private String fixVarName(final String var) {
        final String v = var.charAt(0) == '_' ? var.substring(1) : var;
        final char c = v.charAt(0);
        return Character.isLowerCase(c) ? Character.toUpperCase(c) + v.substring(1) : v;
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
            final ErlangFunction function, final Collection<IErlComment> comments,
            final boolean arityOnly, final List<String> parameterNames,
            final List<ICompletionProposal> result) {
        if (function.name.regionMatches(0, prefix, 0, prefix.length())) {
            final int offs = function.name.length() - prefix.length();

            final List<Point> offsetsAndLengths = new ArrayList<Point>();
            if (!arityOnly) {
                addOffsetsAndLengths(parameterNames, offset + offs + 1, offsetsAndLengths);
            }
            final String funWithArity = function.getNameWithArity();
            String funWithParameters = arityOnly ? funWithArity : getNameWithParameters(
                    function.name, parameterNames);
            funWithParameters = funWithParameters.substring(prefix.length());
            final String htmlComment = comments == null ? "" : HTMLPrinter
                    .asHtml(HoverUtil.getDocumentationString(comments, null));
            addFunctionCompletion(offset, result, funWithArity, htmlComment,
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
            final int replacementOffset0, final List<Point> result) {
        int replacementOffset = replacementOffset0;
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
                result.add(new Point(offset.intValue() + replacementOffset, length
                        .intValue()));
            } catch (final OtpErlangRangeException e) {
            }
        }
    }

    @Override
    public IContextInformation[] computeContextInformation(final ITextViewer viewer,
            final int offset) {
        return null;
    }

    @Override
    public char[] getContextInformationAutoActivationCharacters() {
        return null;
    }

    @Override
    public String getErrorMessage() {
        return null;
    }

    @Override
    public IContextInformationValidator getContextInformationValidator() {
        return null;
    }

}
