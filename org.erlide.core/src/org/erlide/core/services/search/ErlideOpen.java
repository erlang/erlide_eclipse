package org.erlide.core.services.search;

import java.util.Collection;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.erlide.backend.BackendUtils;
import org.erlide.backend.IBackend;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.jinterface.ErlLogger;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.utils.Util;

import com.ericsson.otp.erlang.OtpErlang;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;

public class ErlideOpen {

    public static OtpErlangObject getSourceFromModule(final IBackend backend,
            final OtpErlangList pathVars, final String mod,
            final String externalModules) throws RpcException {
        final OtpErlangObject res2 = backend.call("erlide_open",
                "get_source_from_module", "ax", mod,
                mkContext(externalModules, null, pathVars, null, null));
        return res2;
    }

    @SuppressWarnings("boxing")
    public static OpenResult open(final IBackend backend,
            final IErlModule module, final int offset,
            final List<OtpErlangObject> imports, final String externalModules,
            final OtpErlangList pathVars) throws RpcException {
        // ErlLogger.debug("open offset " + offset);
        final Collection<IPath> extra = BackendUtils.getExtraSourcePaths();
        final String scanner = module.getScannerName();
        final OtpErlangObject res = backend.call("erlide_open", "open", "aix",
                scanner, offset,
                mkContext(externalModules, null, pathVars, extra, imports));
        ErlLogger.debug("open: %s %d = %s", module.getName(), offset, res);
        return new OpenResult(res);
    }

    public static OtpErlangTuple mkContext(final String externalModules,
            final String externalIncludes, final OtpErlangList pathVars,
            final Collection<IPath> extraSourcePaths,
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

    public static OtpErlangTuple findFirstVar(final IBackend backend,
            final String name, final String source) {
        try {
            final OtpErlangObject res = backend.call("erlide_open",
                    "find_first_var", "as", name, source);
            if (res instanceof OtpErlangTuple) {
                return (OtpErlangTuple) res;
            }
        } catch (final RpcException e) {
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
            final IBackend backend, final String externalModules,
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
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
        return Lists.newArrayList();
    }

    public static String getExternalInclude(final IBackend backend,
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
        } catch (final RpcException e) {
            ErlLogger.error(e);
        }
        return null;
    }

    public static List<String> getLibDirs(final IBackend backend) {
        try {
            final OtpErlangObject res = backend.call("erlide_open",
                    "get_lib_dirs", "");
            return getStringListTuple(res);
        } catch (final RpcException e) {
            ErlLogger.error(e);
            return null;
        }
    }

    public static List<String> getLibFiles(final IBackend backend,
            final String entry) {
        try {
            final OtpErlangObject res = backend.call("erlide_open",
                    "get_lib_files", "s", entry);
            return getStringListTuple(res);
        } catch (final RpcException e) {
            ErlLogger.error(e);
            return null;
        }
    }

    public static List<String> getLibSrcInclude(final IBackend backend,
            final String entry) {
        try {
            final OtpErlangObject res = backend.call("erlide_open",
                    "get_lib_src_include", "s", entry);
            return getStringListTuple(res);
        } catch (final RpcException e) {
            ErlLogger.error(e);
            return null;
        }
    }

    private static List<String> getStringListTuple(final OtpErlangObject res) {
        if (Util.isOk(res)) {
            final OtpErlangTuple t = (OtpErlangTuple) res;
            final OtpErlangList l = (OtpErlangList) t.elementAt(1);
            final List<String> result = Lists.newArrayListWithCapacity(l
                    .arity());
            for (final OtpErlangObject o : l) {
                result.add(Util.stringValue(o));
            }
            return result;
        }
        return null;
    }

    public static Collection<String> getIncludesInDir(final IBackend backend,
            final String directory) {
        try {
            final OtpErlangObject res = backend.call("erlide_open",
                    "get_includes_in_dir", "s", directory);
            if (Util.isOk(res)) {
                final OtpErlangTuple t = (OtpErlangTuple) res;
                final OtpErlangList l = (OtpErlangList) t.elementAt(1);
                final List<String> result = Lists.newArrayListWithCapacity(l
                        .arity());
                for (final OtpErlangObject object : l) {
                    result.add(Util.stringValue(object));
                }
                return result;
            }
        } catch (final RpcException e) {
            ErlLogger.error(e);
        }
        return null;
    }

}
