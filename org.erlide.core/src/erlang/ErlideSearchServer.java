package erlang;

import java.util.List;

import org.eclipse.core.resources.IResource;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.search.ErlSearchScope;
import org.erlide.core.search.ModuleLineFunctionArityRef;
import org.erlide.core.text.ErlangToolkit;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.util.Util;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;

public class ErlideSearchServer {

    private static final int SEARCH_LONG_TIMEOUT = 50000;

    private static OtpErlangList getModulesFromScope(
            final ErlSearchScope scope, final ErlSearchScope externalScope) {
        final OtpErlangObject result[] = new OtpErlangObject[scope.size()
                + externalScope.size()];
        int i = 0;
        for (final IErlModule module : scope.getModules()) {
            result[i] = make2Tuple(
                    ErlangToolkit.createScannerModuleName(module),
                    module.getFilePath());
            i++;
        }
        for (final IErlModule module : externalScope.getModules()) {
            result[i] = make2Tuple(
                    ErlangToolkit.createScannerModuleName(module),
                    module.getFilePath());
            i++;
        }
        return new OtpErlangList(result);
    }

    private static OtpErlangTuple make2Tuple(final String scannerModuleName,
            final String path) {
        return new OtpErlangTuple(
                new OtpErlangObject[] { new OtpErlangAtom(scannerModuleName),
                        new OtpErlangString(path) });
    }

    public static List<ModuleLineFunctionArityRef> findRefs(final Backend b,
            final ErlangSearchPattern ref, final ErlSearchScope scope,
            final ErlSearchScope externalScope, final String stateDir) {
        final List<ModuleLineFunctionArityRef> result = Lists.newArrayList();
        try {
            // ErlLogger.debug("Search for " + ref.getSearchObject() + "    " +
            // getModulesFromScope(scope));
            final OtpErlangObject r = b.call(SEARCH_LONG_TIMEOUT,
                    "erlide_search_server", "find_refs", "xxs",
                    ref.getSearchObject(),
                    getModulesFromScope(scope, externalScope), stateDir);
            if (Util.isOk(r)) {
                addSearchResult(result, r);
            }
        } catch (final Exception e) {
            ErlLogger.error(e); // TODO report error
        }
        return result;
    }

    public static List<ModuleLineFunctionArityRef> findRefs(final Backend b,
            final ErlangSearchPattern ref, final IErlModule module,
            final String stateDir) {
        final ErlSearchScope scope = new ErlSearchScope();
        final ErlSearchScope externalScope = new ErlSearchScope();
        final IResource r = module.getResource();
        if (r != null) {
            scope.addModule(module);
        } else {
            externalScope.addModule(module);
        }
        return findRefs(b, ref, scope, externalScope, stateDir);
    }

    private static void addSearchResult(
            final List<ModuleLineFunctionArityRef> result,
            final OtpErlangObject r) throws OtpErlangRangeException {
        final OtpErlangTuple t = (OtpErlangTuple) r;
        final OtpErlangList l = (OtpErlangList) t.elementAt(1);
        for (final OtpErlangObject i : l) {
            /*
             * find_data([#ref{function=F, arity=A, clause=C, data=D, offset=O,
             * length=L, sub_clause=S} | Rest], Data, M, Acc) -> case D of Data
             * -> find_data(Rest, Data, M, [{M, F, A, C, S, O, L} | Acc]); _ ->
             * find_data(Rest, Data, M, Acc) end.
             */
            final OtpErlangTuple modLineT = (OtpErlangTuple) i;
            final String modName = Util.stringValue(modLineT.elementAt(0));
            final OtpErlangObject nameO = modLineT.elementAt(1);
            final OtpErlangLong arityL = (OtpErlangLong) modLineT.elementAt(2);
            final int arity = arityL.intValue();
            final String clauseHead = Util.stringValue(modLineT.elementAt(3));
            final OtpErlangAtom subClause = (OtpErlangAtom) modLineT
                    .elementAt(4);
            final OtpErlangLong offsetL = (OtpErlangLong) modLineT.elementAt(5);
            final OtpErlangLong lengthL = (OtpErlangLong) modLineT.elementAt(6);
            final OtpErlangAtom isDef = (OtpErlangAtom) modLineT.elementAt(7);
            String name;
            if (nameO instanceof OtpErlangAtom) {
                final OtpErlangAtom nameA = (OtpErlangAtom) nameO;
                name = nameA.atomValue();
            } else {
                name = Util.stringValue(nameO);
            }
            result.add(new ModuleLineFunctionArityRef(modName, offsetL
                    .intValue(), lengthL.intValue(), name, arity, clauseHead,
                    "true".equals(subClause.atomValue()), "true".equals(isDef
                            .atomValue())));
        }
    }

    // public static List<ModuleLineFunctionArityRef> functionUse(
    // final ErlideBackend backend,
    // final ErlangExternalFunctionCallRef searchRef,
    // final List<IResource> scope) {
    // return functionUse(backend, searchRef.getModule(), searchRef
    // .getFunction(), searchRef.getArity(), scope);
    // }

}
