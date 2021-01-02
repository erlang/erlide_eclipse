package org.erlide.engine.internal.services.codeassist;

import java.util.ArrayList;
import java.util.Collection;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.annotation.NonNull;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.ErlElementKind;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.IErlElement;
import org.erlide.engine.model.erlang.ErlangFunction;
import org.erlide.engine.model.erlang.IErlComment;
import org.erlide.engine.model.erlang.IErlFunction;
import org.erlide.engine.model.erlang.IErlFunctionClause;
import org.erlide.engine.model.erlang.IErlImport;
import org.erlide.engine.model.erlang.IErlPreprocessorDef;
import org.erlide.engine.model.erlang.IErlRecordDef;
import org.erlide.engine.model.erlang.IErlRecordField;
import org.erlide.engine.model.root.IErlElementLocator;
import org.erlide.engine.model.root.IErlModule;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.services.codeassist.CompletionData;
import org.erlide.engine.services.codeassist.CompletionNameComparer;
import org.erlide.engine.services.codeassist.CompletionService;
import org.erlide.engine.services.codeassist.FunctionCompletionData;
import org.erlide.engine.services.codeassist.Location;
import org.erlide.engine.services.search.ModelFindService;
import org.erlide.engine.services.text.DocumentationFormatter;
import org.erlide.runtime.rpc.IOtpRpc;
import org.erlide.util.ErlLogger;
import org.erlide.util.StringUtils;
import org.erlide.util.Util;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;

public class ErlangCompletionService implements CompletionService {

    private IErlProject project;
    private IErlModule module;
    private String elementBefore;
    private final ErlideContextAssist contextAssistService;

    private static final List<CompletionData> EMPTY_COMPLETIONS = new ArrayList<>();

    public ErlangCompletionService(final IOtpRpc backend) {
        contextAssistService = new ErlideContextAssist(backend);
    }

    @Override
    public List<CompletionData> computeCompletions(final IOtpRpc backend,
            final IErlProject project0, final IErlModule module0,
            final String elementBefore0, final int offset, final String before0,
            final boolean inString) throws CoreException {
        // FIXME these should be passed on as parameters, where needed
        project = project0;
        module = module0;
        elementBefore = elementBefore0;

        String before = before0;
        final int commaPos = before.lastIndexOf(',');
        final int colonPos = before.lastIndexOf(':');
        final boolean doubleColon = colonPos >= 0 && before.charAt(colonPos - 1) == ':';
        final int hashMarkPos = before.lastIndexOf('#');
        final int dotPos = before.lastIndexOf('.');
        final int parenPos = before.lastIndexOf('(');
        final int leftBracketPos = before.lastIndexOf('{');
        final int interrogationMarkPos = before.lastIndexOf('?');
        final int arrowPos = before.lastIndexOf("->");
        final String prefix = getPrefix(before);
        List<String> fieldsSoFar = null;
        List<CompletionData> result;
        EnumSet<@NonNull CompletionFlag> flags = EnumSet.noneOf(CompletionFlag.class);
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
                rc = contextAssistService.checkRecordCompletion(backend, before);
            }
        }
        if (rc != null && rc.isNameWanted()) {
            flags = EnumSet.of(CompletionFlag.RECORD_DEFS);
            pos = hashMarkPos;
            before = rc.getPrefix();
        } else if (rc != null && rc.isFieldWanted()) {
            flags = EnumSet.of(CompletionFlag.RECORD_FIELDS);
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
                flags = EnumSet.of(CompletionFlag.TYPES);
                pos = colonPos;
                before = before.substring(colonPos + 1);
            } else {
                moduleOrRecord = StringUtils
                        .unquote(getPrefix(before.substring(0, colonPos)));
                flags = EnumSet.of(CompletionFlag.EXTERNAL_FUNCTIONS);
                pos = colonPos;
                before = before.substring(colonPos + 1);
            }
        } else if (interrogationMarkPos > hashMarkPos && interrogationMarkPos > commaPos
                && interrogationMarkPos > colonPos && interrogationMarkPos > arrowPos) {
            flags = EnumSet.of(CompletionFlag.MACRO_DEFS);
            pos = interrogationMarkPos;
            before = before.substring(interrogationMarkPos + 1);
        } else {
            pos = colonPos;
            before = prefix;
            if (element != null) {
                switch (element.getKind()) {
                case EXPORT:
                    flags = EnumSet.of(CompletionFlag.DECLARED_FUNCTIONS,
                            CompletionFlag.ARITY_ONLY, CompletionFlag.UNEXPORTED_ONLY);
                    break;
                case IMPORT:
                    final IErlImport i = (IErlImport) element;
                    moduleOrRecord = i.getImportModule();
                    flags = EnumSet.of(CompletionFlag.EXTERNAL_FUNCTIONS,
                            CompletionFlag.ARITY_ONLY);
                    break;
                case FUNCTION:
                case CLAUSE:
                    flags = EnumSet.of(CompletionFlag.MODULES);
                    if (module != null) {
                        flags.addAll(EnumSet.of(CompletionFlag.VARIABLES,
                                CompletionFlag.DECLARED_FUNCTIONS,
                                CompletionFlag.IMPORTED_FUNCTIONS,
                                CompletionFlag.AUTO_IMPORTED_FUNCTIONS));

                    }
                    break;
                case ATTRIBUTE:
                    if ("include".equals(element.getName())) {
                        flags = EnumSet.of(CompletionFlag.INCLUDES);
                    } else if ("include_lib".equals(element.getName())) {
                        flags = EnumSet.of(CompletionFlag.INCLUDE_LIBS);
                    }
                    break;
                default:
                    break;
                }
            } else if (doubleColon) {
                flags = EnumSet.of(CompletionFlag.TYPES);
            } else {
                flags = EnumSet.of(CompletionFlag.MODULES);
            }
        }
        // TODO flags = filterFlags(flags);
        result = addCompletions(backend, flags, offset, before, moduleOrRecord, pos,
                fieldsSoFar, inString);
        return result;
    }

    protected List<CompletionData> getModules(final IOtpRpc backend, final int offset,
            final String prefix, final CompletionFlag kind, final boolean inString)
            throws ErlModelException {
        final List<String> mods = getModules0(backend, offset, prefix, kind);
        final boolean includes = kind == CompletionFlag.INCLUDES
                || kind == CompletionFlag.INCLUDE_LIBS;
        final List<CompletionData> result = Lists.newArrayList();
        for (final String m : mods) {
            final String suffix = includes ? "" : ":";
            final String cpl = quoted(m + suffix, kind, inString);
            final int prefixLength = prefix.length();
            result.add(new CompletionData(null, cpl, offset - prefixLength, prefixLength,
                    cpl.length()));
        }
        return result;
    }

    private String quoted(final String string, final CompletionFlag kind,
            final boolean inString) {
        if (kind == CompletionFlag.INCLUDES || kind == CompletionFlag.INCLUDE_LIBS) {
            if (!inString) {
                return "\"" + string + "\"";
            }
        }
        if (kind == CompletionFlag.MODULES) {
            final String string1 = string.substring(0, string.length() - 1);
            return new OtpErlangAtom(string1).toString() + ":";
        }
        return string;
    }

    List<String> getModules0(final IOtpRpc backend, final int offset, final String prefix,
            final CompletionFlag kind) throws ErlModelException {
        final List<String> result = Lists.newArrayList();
        final boolean includes = kind == CompletionFlag.INCLUDES
                || kind == CompletionFlag.INCLUDE_LIBS;
        final List<String> names = ErlangEngine.getInstance().getModelUtilService()
                .findUnitsWithPrefix(prefix, project, kind != CompletionFlag.INCLUDES,
                        includes);
        final OtpErlangObject res = ErlangEngine.getInstance().getOtpDocService()
                .getModules(backend, prefix, names, includes);
        if (res instanceof OtpErlangList) {
            final OtpErlangList resList = (OtpErlangList) res;
            for (final OtpErlangObject o : resList) {
                if (o instanceof OtpErlangString) {
                    final OtpErlangString s = (OtpErlangString) o;
                    result.add(s.stringValue());
                }
            }
        }
        return result;
    }

    String getPrefix(final String before) {
        for (int n = before.length() - 1; n >= 0; --n) {
            final char c = before.charAt(n);
            if (!ErlangCompletionService.isErlangIdentifierChar(c) && c != '?'
                    && c != '\'') {
                return before.substring(n + 1);
            }
        }
        return before;
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

    List<CompletionData> getDeclaredFunctions(final int offset, final String prefix,
            final boolean unexportedOnly, final boolean arityOnly)
            throws ErlModelException {
        final List<CompletionData> result = new ArrayList<>();
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

    List<CompletionData> getVariables(final IOtpRpc b, final int offset,
            final String prefix) {
        final List<CompletionData> result = new ArrayList<>();
        final Collection<String> vars = contextAssistService.getVariables(elementBefore,
                prefix);
        for (final String var : vars) {
            result.add(new CompletionData(null, var, offset - prefix.length(),
                    prefix.length(), var.length()));
        }
        return result;
    }

    List<CompletionData> getMacroOrRecordCompletions(final int offset,
            final String prefix, final ErlElementKind kind) {
        if (module == null) {
            return ErlangCompletionService.EMPTY_COMPLETIONS;
        }
        final List<CompletionData> result = new ArrayList<>();
        try {
            final List<IErlPreprocessorDef> defs = ErlangCompletionService
                    .getAllPreprocessorDefs(module, kind);
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

    public static List<IErlPreprocessorDef> getAllPreprocessorDefs(
            final IErlModule module, final ErlElementKind kind) throws CoreException {
        final List<IErlPreprocessorDef> result = Lists.newArrayList();
        final List<IErlModule> modulesWithIncludes = Lists.newArrayList(ErlangEngine
                .getInstance().getModelFindService().findAllIncludedFiles(module));
        modulesWithIncludes.add(module);
        for (final IErlModule m : modulesWithIncludes) {
            result.addAll(m.getPreprocessorDefs(kind));
        }
        return result;
    }

    List<CompletionData> getExternalCallCompletions(final IOtpRpc b,
            final String moduleName0, final int offset, final String prefix,
            final boolean arityOnly) throws CoreException {
        final ModelFindService modelFindService = ErlangEngine.getInstance()
                .getModelFindService();
        final String moduleName = modelFindService.resolveMacroValue(moduleName0, module);
        // we have an external call
        final List<CompletionData> result = new ArrayList<>();
        final IErlElementLocator model = ErlangEngine.getInstance().getModel();
        final IErlModule theModule = modelFindService.findModule(model, project,
                moduleName, null, IErlElementLocator.Scope.ALL_PROJECTS);
        // FIXME or IErlElementLocator.Scope.REFERENCED_PROJECTS
        if (theModule != null) {
            if (ErlangEngine.getInstance().getModelUtilService().isOtpModule(theModule)) {
                final OtpErlangObject res = ErlangEngine.getInstance().getOtpDocService()
                        .getProposalsWithDoc(b, moduleName, prefix);
                addFunctionProposalsWithDoc(offset, prefix, result, res, null, arityOnly);
            } else {
                addFunctionsFromModule(offset, prefix, arityOnly, result, theModule);
            }
        }
        return result;
    }

    List<CompletionData> getRecordFieldCompletions(final String recordName,
            final int offset, final String prefix, final int hashMarkPos,
            final List<String> fieldsSoFar) {
        if (module == null) {
            return ErlangCompletionService.EMPTY_COMPLETIONS;
        }
        IErlPreprocessorDef pd;
        try {
            pd = ErlangEngine.getInstance().getModelFindService()
                    .findPreprocessorDef(module, recordName, ErlElementKind.RECORD_DEF);
        } catch (final CoreException e) {
            return ErlangCompletionService.EMPTY_COMPLETIONS;
        }
        if (pd instanceof IErlRecordDef) {
            final List<CompletionData> result = new ArrayList<>();
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
        return ErlangCompletionService.EMPTY_COMPLETIONS;
    }

    List<CompletionData> addCompletions(final IOtpRpc backend,
            final Set<@NonNull CompletionFlag> flags, final int offset,
            final String prefix, final String moduleOrRecord, final int pos,
            final List<String> fieldsSoFar, final boolean inString) throws CoreException {
        final List<CompletionData> result = new ArrayList<>();
        final IErlProject aProject = project;
        if (aProject == null) {
            return result;
        }
        addSorted(prefix, result, addDeclaredFunctionsCompletions(flags, offset, prefix));
        if (flags.contains(CompletionFlag.VARIABLES)) {
            addSorted(prefix, result, getVariables(backend, offset, prefix));
        }
        if (flags.contains(CompletionFlag.IMPORTED_FUNCTIONS)) {
            addSorted(prefix, result, getImportedFunctions(backend, offset, prefix));
        }
        if (flags.contains(CompletionFlag.AUTO_IMPORTED_FUNCTIONS)) {
            addSorted(prefix, result, getAutoImportedFunctions(backend, offset, prefix));
        }
        if (flags.contains(CompletionFlag.MODULES)) {
            addSorted(prefix, result, getModules(backend, offset, prefix,
                    CompletionFlag.MODULES, inString));
        }
        if (flags.contains(CompletionFlag.INCLUDES)) {
            addSorted(prefix, result, getModules(backend, offset, prefix,
                    CompletionFlag.INCLUDES, inString));
        }
        if (flags.contains(CompletionFlag.INCLUDE_LIBS)) {
            addSorted(prefix, result, getModules(backend, offset, prefix,
                    CompletionFlag.INCLUDE_LIBS, inString));
        }
        if (flags.contains(CompletionFlag.RECORD_DEFS)) {
            addSorted(prefix, result, getMacroOrRecordCompletions(offset, prefix,
                    ErlElementKind.RECORD_DEF));
        }
        if (flags.contains(CompletionFlag.RECORD_FIELDS)) {
            addSorted(prefix, result, getRecordFieldCompletions(moduleOrRecord, offset,
                    prefix, pos, fieldsSoFar));
        }
        if (flags.contains(CompletionFlag.MACRO_DEFS)) {
            addSorted(prefix, result, getMacroOrRecordCompletions(offset, prefix,
                    ErlElementKind.MACRO_DEF));
        }
        if (flags.contains(CompletionFlag.EXTERNAL_FUNCTIONS)) {
            addSorted(prefix, result, getExternalCallCompletions(backend, moduleOrRecord,
                    offset, prefix, flags.contains(CompletionFlag.ARITY_ONLY)));
        }
        if (flags.contains(CompletionFlag.TYPES)) {
            addSorted(prefix, result,
                    getTypeCompletions(backend, moduleOrRecord, offset, prefix));
        }

        return result;
    }

    private List<CompletionData> addDeclaredFunctionsCompletions(
            final Set<@NonNull CompletionFlag> flags, final int offset,
            final String prefix) throws CoreException {
        final List<CompletionData> result = new ArrayList<>();
        if (flags.contains(CompletionFlag.DECLARED_FUNCTIONS)) {
            addSorted(prefix, result,
                    getDeclaredFunctions(offset, prefix,
                            flags.contains(CompletionFlag.UNEXPORTED_ONLY),
                            flags.contains(CompletionFlag.ARITY_ONLY)));
        }
        return result;
    }

    private void addSorted(final String prefix, final List<CompletionData> result,
            final List<CompletionData> completions) {
        final CompletionNameComparer completionNameComparer = new CompletionNameComparer(
                prefix);
        completions.sort(completionNameComparer);
        result.addAll(completions);
    }

    List<CompletionData> getTypeCompletions(final IOtpRpc backend,
            final String moduleOrRecord, final int offset, final String prefix) {
        final List<CompletionData> result = new ArrayList<>();
        for (final String builtin : getBuiltinTypeCompletions()) {
            if (builtin.startsWith(prefix.trim())) {
                result.add(new CompletionData(null, builtin, offset - prefix.length(),
                        prefix.length(), builtin.length()));
            }
        }
        // TODO provide types completions from workspace
        return result;
    }

    List<String> getBuiltinTypeCompletions() {
        final List<String> result = Lists.newArrayList("any()", "binary()", "bitstring()",
                "boolean()", "byte()", "char()", "float()", "fun()", "integer()",
                "iolist()", "list()", "maybe_improper_list()", "mfa()", "module()",
                "neg_integer()", "no_return()", "node()", "non_neg_integer()", "none()",
                "nonempty_list()", "number()", "pid()", "port()", "pos_integer()",
                "reference()", "term()", "timeout()", "tuple()");
        return result;
    }

    void addFunctionProposalsWithDoc(final int offset, final String aprefix,
            final List<CompletionData> result, final OtpErlangObject res,
            final IErlImport erlImport, final boolean arityOnly) {
        if (res instanceof OtpErlangList) {
            final OtpErlangList resl = (OtpErlangList) res;
            for (final OtpErlangObject i : resl) {
                // {FunWithArity, FunWithParameters, [{Offset, Length}], Doc}
                final OtpErlangTuple f = (OtpErlangTuple) i;
                final String funWithArity = ((OtpErlangString) f.elementAt(0))
                        .stringValue();
                if (!ErlangCompletionService.filterImported(erlImport, funWithArity)) {
                    continue;
                }
                String funWithParameters = arityOnly ? funWithArity
                        : ((OtpErlangString) f.elementAt(1)).stringValue();
                final OtpErlangList parOffsets = (OtpErlangList) f.elementAt(2);
                String docStr = null;
                if (f.arity() > 3) {
                    final OtpErlangObject elt = f.elementAt(3);
                    if (elt instanceof OtpErlangString) {
                        docStr = Util.stringValue(elt);
                    }
                }

                funWithParameters = funWithParameters.substring(aprefix.length());
                final List<Location> offsetsAndLengths = new ArrayList<>();
                if (!arityOnly) {
                    addOffsetsAndLengths(parOffsets, offset, offsetsAndLengths);
                }
                addFunctionCompletion(offset, result, funWithArity, docStr,
                        funWithParameters, offsetsAndLengths);
            }
        }
    }

    protected void addFunctionCompletion(final int offset,
            final List<CompletionData> result, final String funWithArity,
            final String docStr, final String funWithParameters,
            final List<Location> offsetsAndLengths) {
        int cursorPosition = funWithParameters.length();
        if (!offsetsAndLengths.isEmpty()) {
            cursorPosition = offsetsAndLengths.get(0).getOffset();
        }

        // first check if it's already there...
        for (final CompletionData c : result) {
            if (c.getDisplayString().equals(funWithArity)) {
                return;
            }
        }
        final CompletionData c = new FunctionCompletionData(offsetsAndLengths,
                funWithArity, funWithParameters, offset, 0, cursorPosition, docStr);

        result.add(c);
    }

    void addFunctionCompletion(final int offset, final String aprefix,
            final List<CompletionData> result, final IErlFunction function,
            final boolean arityOnly) {
        addFunctionCompletion(offset, aprefix, function.getFunction(),
                function.getComments(), arityOnly,
                arityOnly ? null : getParameterNames(function), result);
    }

    List<String> getParameterNames(final IErlFunction function) {
        final List<String> parameters = function.getParameters();
        final int arity = function.getArity();
        final List<String> result = new ArrayList<>(arity);
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
            if ("_".equals(result.get(i))) {
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
        if (parameter == null || parameter.isEmpty()) {
            return false;
        }
        final char c = parameter.charAt(0);
        final char c2 = parameter.length() > 1 ? parameter.charAt(1) : c;
        return c >= 'A' && c <= 'Z'
                || c == '_' && (c2 >= 'A' && c <= 'Z' || c2 >= 'a' && c2 <= 'z');
    }

    void addFunctionCompletion(final int offset, final String prefix,
            final ErlangFunction function, final Collection<IErlComment> comments,
            final boolean arityOnly, final List<String> parameterNames,
            final List<CompletionData> result) {
        if (function.name.regionMatches(0, prefix, 0, prefix.length())) {
            final int offs = function.name.length() - prefix.length();

            final List<Location> offsetsAndLengths = new ArrayList<>();
            if (!arityOnly) {
                addOffsetsAndLengths(parameterNames, offset + offs + 1,
                        offsetsAndLengths);
            }
            final String funWithArity = function.getNameWithArity();
            String funWithParameters = arityOnly ? funWithArity
                    : getNameWithParameters(function.name, parameterNames);
            funWithParameters = funWithParameters.substring(prefix.length());
            final String htmlComment = comments == null ? ""
                    : DocumentationFormatter.getDocumentationString(comments, null);
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
            final int replacementOffset0, final List<Location> result) {
        int replacementOffset = replacementOffset0;
        for (final String par : parameterNames) {
            result.add(new Location(replacementOffset, par.length()));
            replacementOffset += par.length() + 2;
        }
    }

    void addOffsetsAndLengths(final OtpErlangList parOffsets, final int replacementOffset,
            final List<Location> result) {
        for (final OtpErlangObject i : parOffsets) {
            final OtpErlangTuple t = (OtpErlangTuple) i;
            final OtpErlangLong offset = (OtpErlangLong) t.elementAt(0);
            final OtpErlangLong length = (OtpErlangLong) t.elementAt(1);
            try {
                result.add(new Location(offset.intValue() + replacementOffset,
                        length.intValue()));
            } catch (final OtpErlangRangeException e) {
            }
        }
    }

    List<CompletionData> getAutoImportedFunctions(final IOtpRpc backend, final int offset,
            final String prefix) {
        final OtpErlangObject res = ErlangEngine.getInstance().getOtpDocService()
                .getProposalsWithDoc(backend, "<auto_imported>", prefix);
        final List<CompletionData> result = new ArrayList<>();
        addFunctionProposalsWithDoc(offset, prefix, result, res, null, false);
        return result;
    }

    List<CompletionData> getImportedFunctions(final IOtpRpc backend, final int offset,
            final String prefix) {
        final List<CompletionData> result = new ArrayList<>();
        for (final IErlImport imp : module.getImports()) {
            final OtpErlangObject res = ErlangEngine.getInstance().getOtpDocService()
                    .getProposalsWithDoc(backend, imp.getImportModule(), prefix);
            addFunctionProposalsWithDoc(offset, prefix, result, res, imp, false);
        }
        return result;
    }

    boolean addFunctionsFromModule(final int offset, final String prefix,
            final boolean arityOnly, final List<CompletionData> proposals,
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

    void addIfMatches(final String name, final String prefix, final int offset,
            final List<CompletionData> result) {
        final int length = prefix.length();
        if (name.regionMatches(true, 0, prefix, 0, length)) {
            result.add(new CompletionData(null, name, offset - length, length,
                    name.length()));
        }
    }

    static boolean isErlangIdentifierChar(final char char1) {
        return Character.isJavaIdentifierPart(char1);
    }

}
