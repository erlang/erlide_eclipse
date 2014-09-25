package org.erlide.engine.internal.model.erlang;

import java.util.List;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.internal.model.root.Openable;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.IParent;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlExternalRoot;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.services.search.OpenService;
import org.erlide.engine.util.OtpRpcFactory;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.util.erlang.ErlUtils;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.base.Charsets;

public class ErlOtpExternalReferenceEntryList extends Openable implements
        IErlExternalRoot {

    public ErlOtpExternalReferenceEntryList(final IParent parent, final String name) {
        super(parent, name);
    }

    @Override
    public ErlElementKind getKind() {
        return ErlElementKind.EXTERNAL_ROOT;
    }

    @Override
    public boolean buildStructure(final IProgressMonitor pm) throws ErlModelException {
        final IErlProject erlProject = ErlangEngine.getInstance().getModelUtilService()
                .getProject(this);
        final IOtpRpc backend = OtpRpcFactory.getOtpRpcForProject(erlProject);
        if (backend != null) {
            addExternalEntries(pm, backend);
        }
        return true;
    }

    private void addExternalEntries(final IProgressMonitor pm, final IOtpRpc backend) {
        final OtpErlangList structure = ErlangEngine.getInstance()
                .getService(OpenService.class).getOtpLibStructure(backend);
        mkOtpStructureMap(structure);
    }

    @Override
    public String getFilePath() {
        return null;
    }

    public String getExternalName() {
        return getName();
    }

    @Override
    public boolean isOTP() {
        return true;
    }

    @Override
    public IResource getResource() {
        return null;
    }

    @Override
    public boolean hasIncludes() {
        return true;
    }

    @Override
    public List<IErlElement> internalGetChildren() {
        return super.internalGetChildren();
    }

    private void mkOtpStructureMap(final OtpErlangList input) {
        for (final OtpErlangObject o : input) {
            final OtpErlangTuple t = (OtpErlangTuple) o;
            final String lib = ((OtpErlangString) t.elementAt(0)).stringValue();
            final OtpErlangList dirs = (OtpErlangList) t.elementAt(1);
            final String group = ErlUtils.asString(t.elementAt(2));

            final ErlExternalReferenceEntry extLib = new ErlExternalReferenceEntry(this,
                    getLibName(lib), lib, true, false);
            extLib.setGroup(group);
            addChild(extLib);

            for (final OtpErlangObject dir : dirs.elements()) {
                final OtpErlangTuple tdir = (OtpErlangTuple) dir;
                final String dname = ((OtpErlangString) tdir.elementAt(0)).stringValue();
                final OtpErlangList files = (OtpErlangList) tdir.elementAt(1);

                final ErlExternalReferenceEntry subdir = new ErlExternalReferenceEntry(
                        extLib, getLibName(dname), dname, true, includePath(dname));
                extLib.addChild(subdir);

                for (final OtpErlangObject fn : files.elements()) {
                    final String sfn = ((OtpErlangString) fn).stringValue();
                    final IErlModule ext = new ErlModule(subdir, getModuleName(sfn), sfn,
                            Charsets.ISO_8859_1.toString(), null);
                    subdir.addChild(ext);
                }
            }
        }
    }

    private final boolean includePath(final String path) {
        final IPath p = new Path(path);
        return p.lastSegment().equals("include");
    }

    private String getLibName(final String libDir) {
        final IPath p = new Path(libDir);
        String s = p.lastSegment();
        if ("ebin".equals(s)) {
            s = p.removeLastSegments(1).lastSegment();
        }
        final int dashPos = s.lastIndexOf('-');
        if (dashPos != -1) {
            return s.substring(0, dashPos);
        }
        return s;
    }

    private String getModuleName(final String spath) {
        final IPath p = new Path(spath);
        final String s = p.lastSegment();
        return s;
    }

}
