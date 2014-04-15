package org.erlide.engine.internal.services.search;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.eclipse.core.runtime.IPath;
import org.erlide.engine.internal.model.erlang.ErlExternalReferenceEntry;
import org.erlide.engine.internal.model.erlang.ErlOtpExternalReferenceEntryList;
import org.erlide.engine.model.root.IErlExternalRoot;
import org.erlide.engine.services.search.OpenResult;
import org.erlide.engine.services.search.OpenService;
import org.erlide.engine.util.SourcePathUtils;
import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.util.ErlLogger;
import org.erlide.util.Util;
import org.erlide.util.erlang.ErlUtils;
import org.erlide.util.erlang.OtpErlang;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.base.Stopwatch;
import com.google.common.collect.Lists;

public class ErlideOpen implements OpenService {

    private static final String ERLIDE_OPEN = "erlide_open";

    private final IRpcSite ideBackend;

    private final String stateDir;

    public ErlideOpen(final IRpcSite backend, final String stateDir) {
        ideBackend = backend;
        this.stateDir = stateDir;
    }

    @Override
    public OtpErlangObject getSourceFromModule(final OtpErlangList pathVars,
            final String mod, final String externalModules) throws RpcException {
        final OtpErlangObject res2 = ideBackend.call(ERLIDE_OPEN,
                "get_source_from_module", "ax", mod,
                mkContext(externalModules, null, pathVars, null, null));
        return res2;
    }

    @Override
    @SuppressWarnings("boxing")
    public OpenResult open(final String scannerName, final int offset,
            final List<OtpErlangObject> imports, final String externalModules,
            final OtpErlangList pathVars) throws RpcException {
        // ErlLogger.debug("open offset " + offset);
        final Collection<IPath> extra = SourcePathUtils.getExtraSourcePaths();
        final OtpErlangObject res = ideBackend.call(ERLIDE_OPEN, "open", "aix",
                scannerName, offset,
                mkContext(externalModules, null, pathVars, extra, imports));
        return new OpenResult(res);
    }

    @Override
    @SuppressWarnings("boxing")
    public OpenResult openText(final String text, final int offset)
            throws RpcException {
        final OtpErlangObject res = ideBackend.call(ERLIDE_OPEN, "open_text",
                "si", text, offset);
        return new OpenResult(res);
    }

    @Override
    public OtpErlangTuple mkContext(final String externalModules,
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

    @Override
    public OtpErlangTuple findFirstVar(final String name, final String source) {
        try {
            final OtpErlangObject res = ideBackend.call(ERLIDE_OPEN,
                    "find_first_var", "as", name, source);
            if (res instanceof OtpErlangTuple) {
                return (OtpErlangTuple) res;
            }
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
        return null;
    }

    @Override
    public List<ExternalTreeEntry> getExternalModuleTree(
            final IRpcSite backend, final String externalModules,
            final OtpErlangList pathVars) {
        ErlLogger.debug("open:external_module_tree -> " + externalModules);
        final Stopwatch stopwatch = Stopwatch.createStarted();
        try {
            final OtpErlangObject res = backend.call(ERLIDE_OPEN,
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
                final String msg = "open:external_module_tree <- " + stopwatch;
                if (stopwatch.elapsed(TimeUnit.SECONDS) > 5) {
                    ErlLogger.warn("WARNING " + msg);
                } else {
                    ErlLogger.debug(msg);
                }
                return result;
            }
        } catch (final RpcException e) {
            ErlLogger.warn("open:external_module_tree TIMEOUT <- " + stopwatch);
            ErlLogger.warn(e);
        }
        return Lists.newArrayList();
    }

    @Override
    public String getExternalInclude(final String filePath,
            final String externalIncludes, final OtpErlangList pathVars) {
        try {
            final OtpErlangObject res = ideBackend.call(ERLIDE_OPEN,
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

    @Override
    public IErlExternalRoot getOtpLibStructure(final IRpcSite backend) {
        try {
            final OtpErlangObject res = backend.call(ERLIDE_OPEN,
                    "get_otp_lib_structure", "s", stateDir);
            if (Util.isOk(res)) {
                final OtpErlangTuple tres = (OtpErlangTuple) res;
                final OtpErlangList lot = (OtpErlangList) tres.elementAt(1);
                return mkOtpStructureMap(lot);
            }
        } catch (final RpcException e) {
            ErlLogger.error(e);
        }
        return null;
    }

    private IErlExternalRoot mkOtpStructureMap(final OtpErlangList input) {
        final IErlExternalRoot root = new ErlOtpExternalReferenceEntryList(
                null, "otp");

        for (final OtpErlangObject o : input) {
            final OtpErlangTuple t = (OtpErlangTuple) o;
            final String lib = ((OtpErlangString) t.elementAt(0)).stringValue();
            final OtpErlangList dirs = (OtpErlangList) t.elementAt(1);
            final String group = ErlUtils.asString(t.elementAt(2));

            final ErlExternalReferenceEntry extLib = new ErlExternalReferenceEntry(
                    root, lib, "", true, false);
            extLib.setGroup(group);
            root.addChild(extLib);

            for (final OtpErlangObject dir : dirs.elements()) {
                final OtpErlangTuple tdir = (OtpErlangTuple) dir;
                final String dname = ((OtpErlangString) tdir.elementAt(0))
                        .stringValue();
                final OtpErlangList files = (OtpErlangList) tdir.elementAt(1);

                final ErlExternalReferenceEntry subdir = new ErlExternalReferenceEntry(
                        extLib, dname, "", true, "include".equals(dname));
                extLib.addChild(subdir);

                for (final OtpErlangObject fn : files.elements()) {
                    final OtpErlangString sfn = (OtpErlangString) fn;
                    final ErlExternalReferenceEntry ext = new ErlExternalReferenceEntry(
                            subdir, sfn.stringValue(), "", true, false);
                    subdir.addChild(ext);
                }
            }
        }
        return root;
    }

    @Override
    public List<String> getLibFiles(final String entry) {
        try {
            final OtpErlangObject res = ideBackend.call(ERLIDE_OPEN,
                    "get_lib_files", "ss", entry, stateDir);
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
        } catch (final RpcException e) {
            ErlLogger.error(e);
        }
        return null;
    }

    @Override
    public Collection<String> getIncludesInDir(final String directory) {
        try {
            final OtpErlangObject res = ideBackend.call(ERLIDE_OPEN,
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
