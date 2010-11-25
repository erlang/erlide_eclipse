package erlang;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.erlide.core.erlang.util.ModelUtils;
import org.erlide.core.erlang.util.SourcePathProvider;
import org.erlide.core.util.Tuple;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.backend.util.Util;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlang;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;

public class ErlideOpen {

    public static String getIncludeLib(final Backend b, String s)
            throws BackendException {
        final OtpErlangObject t = b.call("erlide_open", "get_include_lib", "s",
                s);
        if (t instanceof OtpErlangTuple) {
            final OtpErlangObject es = ((OtpErlangTuple) t).elementAt(1);
            s = ((OtpErlangString) es).stringValue();
        }
        return s;
    }

    public static OtpErlangObject getSourceFromModule(final Backend b,
            final OtpErlangList pathVars, final String mod,
            final String externalModules) throws BackendException {
        final OtpErlangObject res2 = b.call("erlide_open",
                "get_source_from_module", "ax", mod,
                mkContext(externalModules, null, pathVars, null, null));
        return res2;
    }

    @SuppressWarnings("boxing")
    public static OpenResult open(final Backend b, final String scannerName,
            final int offset, final List<OtpErlangObject> imports,
            final String externalModules, final OtpErlangList pathVars)
            throws BackendException {
        // ErlLogger.debug("open offset " + offset);
        final Collection<String> extra = getExtraSourcePaths();
        final OtpErlangObject res = b.call("erlide_open", "open", "aix",
                scannerName, offset,
                mkContext(externalModules, null, pathVars, extra, imports));
        // ErlLogger.debug(">>>> " + res);
        return new OpenResult(res);
    }

    public static Collection<String> getExtraSourcePaths() {
        final Collection<SourcePathProvider> spps = ModelUtils
                .getSourcePathProviders();
        final List<String> result = Lists.newArrayList();
        for (final SourcePathProvider spp : spps) {
            final Collection<IPath> paths = spp.getSourcePaths();
            for (final IPath p : paths) {
                result.add(p.toString());
            }
        }
        return result;
    }

    static OtpErlangTuple mkContext(final String externalModules,
            final String externalIncludes, final OtpErlangList pathVars,
            final Collection<String> extraSourcePaths,
            final Collection<OtpErlangObject> imports) {
        final OtpErlangAtom tag = new OtpErlangAtom("open_context");
        final OtpErlangAtom UNDEFINED = new OtpErlangAtom("undefined");

        final List<OtpErlangObject> result = Lists.newArrayList();
        // order must match definition of #open_context !
        // TODO use a proplist instead?
        result.add(tag);
        result.add(externalModules != null ? new OtpErlangString(
                externalModules) : UNDEFINED);
        result.add(externalIncludes != null ? new OtpErlangString(
                externalIncludes) : UNDEFINED);
        result.add(pathVars != null ? pathVars : UNDEFINED);
        result.add(extraSourcePaths != null ? OtpErlang
                .mkStringList(extraSourcePaths) : UNDEFINED);
        result.add(imports != null ? OtpErlang.mkList(imports) : UNDEFINED);
        return new OtpErlangTuple(result.toArray(new OtpErlangObject[] {}));
    }

    public static OtpErlangTuple findFirstVar(final Backend b,
            final String name, final String source) {
        OtpErlangObject res;
        try {
            res = b.call("erlide_open", "find_first_var", "as", name, source);
            if (res instanceof OtpErlangTuple) {
                return (OtpErlangTuple) res;
            }
        } catch (final BackendException e) {
            ErlLogger.warn(e);
        }
        return null;
    }

    public static List<String> getExternalModules(final Backend b,
            final String prefix, final String externalModules,
            final OtpErlangList pathVars) {
        try {
            final OtpErlangObject res = b.call("erlide_open",
                    "get_external_modules", "sx", prefix,
                    mkContext(externalModules, null, pathVars, null, null));
            if (Util.isOk(res)) {
                final OtpErlangTuple t = (OtpErlangTuple) res;
                final OtpErlangList l = (OtpErlangList) t.elementAt(1);
                final List<String> result = new ArrayList<String>(l.arity());
                for (final OtpErlangObject i : l.elements()) {
                    result.add(Util.stringValue(i));
                }
                return result;
            }
        } catch (final BackendException e) {
            ErlLogger.warn(e);
        }
        return new ArrayList<String>();
    }

    public static List<String> getExternal1(final Backend b,
            final String externalModules, final OtpErlangList pathVars,
            final boolean isRoot) {
        try {
            final OtpErlangObject res = b.call("erlide_open", "get_external_1",
                    "sxo", externalModules, pathVars, isRoot);
            if (Util.isOk(res)) {
                final OtpErlangTuple t = (OtpErlangTuple) res;
                final OtpErlangList l = (OtpErlangList) t.elementAt(1);
                final List<String> result = new ArrayList<String>(l.arity());
                for (final OtpErlangObject i : l.elements()) {
                    result.add(Util.stringValue(i));
                }
                return result;
            }
        } catch (final BackendException e) {
            ErlLogger.warn(e);
        }
        return new ArrayList<String>();
    }

    public static List<Tuple<String, List<String>>> getExternalModuleTree(
            final Backend b, final String externalModules,
            final OtpErlangList pathVars) {
        try {
            final OtpErlangObject res = b.call("erlide_open",
                    "get_external_module_tree", "x",
                    mkContext(externalModules, null, pathVars, null, null));
            if (Util.isOk(res)) {
                OtpErlangTuple t = (OtpErlangTuple) res;
                final OtpErlangList l = (OtpErlangList) t.elementAt(1);
                final List<Tuple<String, List<String>>> result = Lists
                        .newArrayListWithCapacity(l.arity());
                for (final OtpErlangObject i : l) {
                    t = (OtpErlangTuple) i;
                    final OtpErlangString parentS = (OtpErlangString) t
                            .elementAt(0);
                    final String parent = Util.stringValue(parentS);
                    final OtpErlangList childrenL = (OtpErlangList) t
                            .elementAt(1);
                    final List<String> children = Lists
                            .newArrayListWithCapacity(childrenL.arity());
                    for (final OtpErlangObject child : childrenL) {
                        children.add(Util.stringValue(child));
                    }
                    result.add(new Tuple<String, List<String>>(parent, children));
                }
                return result;
            }
        } catch (final BackendException e) {
            ErlLogger.warn(e);
        }
        return Lists.newArrayList();
    }

    public static String getExternalInclude(final Backend b,
            final String filePath, final String externalIncludes,
            final OtpErlangList pathVars) {
        try {
            final OtpErlangObject res = b.call("erlide_open",
                    "get_external_include", "sx", filePath,
                    mkContext(null, externalIncludes, pathVars, null, null));
            if (Util.isOk(res)) {
                final OtpErlangTuple t = (OtpErlangTuple) res;
                return Util.stringValue(t.elementAt(1));
            }
        } catch (final BackendException e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String getExternalModule(final Backend b, final String mod,
            final String externalModules, final OtpErlangList pathVars) {
        try {
            final OtpErlangObject res = b.call("erlide_open",
                    "get_external_module", "sx", mod,
                    mkContext(externalModules, null, pathVars, null, null));
            if (Util.isOk(res)) {
                final OtpErlangTuple t = (OtpErlangTuple) res;
                return Util.stringValue(t.elementAt(1));
            }
        } catch (final BackendException e) {
            e.printStackTrace();
        }
        return null;
    }
}
