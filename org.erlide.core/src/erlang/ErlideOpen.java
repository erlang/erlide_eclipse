package erlang;

import java.util.Collection;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.erlide.core.erlang.util.ModelUtils;
import org.erlide.core.erlang.util.SourcePathProvider;
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

    public static String getIncludeLib(final Backend backend, String s)
            throws BackendException {
        final OtpErlangObject t = backend.call("erlide_open",
                "get_include_lib", "s", s);
        if (t instanceof OtpErlangTuple) {
            final OtpErlangObject es = ((OtpErlangTuple) t).elementAt(1);
            s = ((OtpErlangString) es).stringValue();
        }
        return s;
    }

    public static OtpErlangObject getSourceFromModule(final Backend backend,
            final OtpErlangList pathVars, final String mod,
            final String externalModules) throws BackendException {
        final OtpErlangObject res2 = backend.call("erlide_open",
                "get_source_from_module", "ax", mod,
                mkContext(externalModules, null, pathVars, null, null));
        return res2;
    }

    @SuppressWarnings("boxing")
    public static OpenResult open(final Backend backend,
            final String scannerName, final int offset,
            final List<OtpErlangObject> imports, final String externalModules,
            final OtpErlangList pathVars) throws BackendException {
        // ErlLogger.debug("open offset " + offset);
        final Collection<String> extra = getExtraSourcePaths();
        final OtpErlangObject res = backend.call("erlide_open", "open", "aix",
                scannerName, offset,
                mkContext(externalModules, null, pathVars, extra, imports));
        // ErlLogger.debug(">>>> " + res);
        return new OpenResult(res);
    }

    public static Collection<String> getExtraSourcePaths() {
        final List<String> result = Lists.newArrayList();
        Collection<SourcePathProvider> spps;
        try {
            spps = ModelUtils.getSourcePathProviders();
            for (final SourcePathProvider spp : spps) {
                final Collection<IPath> paths = spp.getSourcePaths();
                for (final IPath p : paths) {
                    result.add(p.toString());
                }
            }
        } catch (final CoreException e) {
            ErlLogger.error(e);
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

    public static OtpErlangTuple findFirstVar(final Backend backend,
            final String name, final String source) {
        try {
            final OtpErlangObject res = backend.call("erlide_open",
                    "find_first_var", "as", name, source);
            if (res instanceof OtpErlangTuple) {
                return (OtpErlangTuple) res;
            }
        } catch (final BackendException e) {
            ErlLogger.warn(e);
        }
        return null;
    }

    // public static List<String> getExternalModules(final Backend backend,
    // final String prefix, final String externalModules,
    // final OtpErlangList pathVars) {
    // try {
    // final OtpErlangObject res = backend.call("erlide_open",
    // "get_external_modules", "sx", prefix,
    // mkContext(externalModules, null, pathVars, null, null));
    // if (Util.isOk(res)) {
    // final OtpErlangTuple t = (OtpErlangTuple) res;
    // final OtpErlangList l = (OtpErlangList) t.elementAt(1);
    // final List<String> result = new ArrayList<String>(l.arity());
    // for (final OtpErlangObject i : l.elements()) {
    // result.add(Util.stringValue(i));
    // }
    // return result;
    // }
    // } catch (final BackendException e) {
    // ErlLogger.warn(e);
    // }
    // return new ArrayList<String>();
    // }

    // public static List<String> getExternal1(final Backend backend,
    // final String externalModules, final OtpErlangList pathVars,
    // final boolean isRoot) {
    // try {
    // final OtpErlangObject res = backend.call("erlide_open",
    // "get_external_1", "sxo", externalModules, pathVars, isRoot);
    // if (Util.isOk(res)) {
    // final OtpErlangTuple t = (OtpErlangTuple) res;
    // final OtpErlangList l = (OtpErlangList) t.elementAt(1);
    // final List<String> result = new ArrayList<String>(l.arity());
    // for (final OtpErlangObject i : l.elements()) {
    // result.add(Util.stringValue(i));
    // }
    // return result;
    // }
    // } catch (final BackendException e) {
    // ErlLogger.warn(e);
    // }
    // return new ArrayList<String>();
    // }

    public static class ExternalTreeEntry {
        private final String parentPath;
        private final String path;
        // private final String name;
        private final boolean isModule;

        public ExternalTreeEntry(final String parentPath, final String path,
        // final String name,
                final boolean isModule) {
            super();
            this.parentPath = parentPath;
            this.path = path;
            // this.name = name;
            this.isModule = isModule;
        }

        public String getParentPath() {
            return parentPath;
        }

        public String getPath() {
            return path;
        }

        // public String getName() {
        // return name;
        // }

        public boolean isModule() {
            return isModule;
        }
    }

    public static List<ExternalTreeEntry> getExternalModuleTree(
            final Backend backend, final String externalModules,
            final OtpErlangList pathVars) {
        try {
            final OtpErlangObject res = backend.call("erlide_open",
                    "get_external_module_tree", "x",
                    mkContext(externalModules, null, pathVars, null, null));
            if (Util.isOk(res)) {
                OtpErlangTuple t = (OtpErlangTuple) res;
                final OtpErlangList l = (OtpErlangList) t.elementAt(1);
                final List<ExternalTreeEntry> result = Lists
                        .newArrayListWithCapacity(l.arity());
                for (final OtpErlangObject i : l) {
                    t = (OtpErlangTuple) i;
                    final String parentPath = Util.stringValue(t.elementAt(0));
                    final String path = Util.stringValue(t.elementAt(1));
                    // final String name = Util.stringValue(t.elementAt(2));
                    // final OtpErlangAtom isModuleA = (OtpErlangAtom) t
                    // .elementAt(3);
                    final OtpErlangAtom isModuleA = (OtpErlangAtom) t
                            .elementAt(2);
                    result.add(new ExternalTreeEntry(parentPath, path,// name,
                            isModuleA.atomValue().equals("module")));
                }
                return result;
            }
        } catch (final BackendException e) {
            ErlLogger.warn(e);
        }
        return Lists.newArrayList();
    }

    public static String getExternalInclude(final Backend backend,
            final String filePath, final String externalIncludes,
            final OtpErlangList pathVars) {
        try {
            final OtpErlangObject res = backend.call("erlide_open",
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

    // private static String getExternalModule(final Backend b, final String
    // mod,
    // final String externalModules, final OtpErlangList pathVars) {
    // try {
    // final OtpErlangObject res = b.call("erlide_open",
    // "get_external_module", "sx", mod,
    // mkContext(externalModules, null, pathVars, null, null));
    // if (Util.isOk(res)) {
    // final OtpErlangTuple t = (OtpErlangTuple) res;
    // return Util.stringValue(t.elementAt(1));
    // }
    // } catch (final BackendException e) {
    // e.printStackTrace();
    // }
    // return null;
    // }

    // public static boolean hasExternalWithPath(final Backend backend,
    // final String externalModules, final String path,
    // final OtpErlangList pathVars) {
    // try {
    // final OtpErlangObject res = backend.call("erlide_open",
    // "has_external_with_path", "sx", path,
    // mkContext(externalModules, null, pathVars, null, null));
    // if (Util.isOk(res)) {
    // return true;
    // }
    // } catch (final BackendException e) {
    // e.printStackTrace();
    // }
    // return false;
    // }
}
