package org.erlide.ui.wizards;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import org.erlide.jinterface.ErlLogger;
import org.erlide.utils.Util;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlProjectImport {
    private final SortedSet<String> resources;
    private final List<String> sourceDirs;
    private final List<String> includeDirs;
    private final String beamDir;
    private final List<String> directories;

    public ErlProjectImport(final OtpErlangObject o) {
        final OtpErlangTuple t = (OtpErlangTuple) o;
        OtpErlangList l = (OtpErlangList) t.elementAt(0);
        resources = (SortedSet<String>) erlangStringList2Collection(l,
                new TreeSet<String>());
        l = (OtpErlangList) t.elementAt(1);
        sourceDirs = (List<String>) erlangStringList2Collection(l,
                new ArrayList<String>());
        l = (OtpErlangList) t.elementAt(2);
        includeDirs = (List<String>) erlangStringList2Collection(l,
                new ArrayList<String>());
        ErlLogger.debug(">>> %s", t);
        final OtpErlangObject beamDirElement = t.elementAt(3);
        if (beamDirElement instanceof OtpErlangAtom) {
            beamDir = "ebin";
        } else {
            beamDir = Util.stringValue(beamDirElement);
        }
        ErlLogger.debug(">>> %s", beamDir);
        l = (OtpErlangList) t.elementAt(4);
        directories = (List<String>) erlangStringList2Collection(l,
                new ArrayList<String>());
        directories.add(0, ".");
    }

    private static Collection<String> erlangStringList2Collection(
            final OtpErlangList l, final Collection<String> c) {
        for (final OtpErlangObject o : l) {
            c.add(Util.stringValue(o));
        }
        return c;
    }

    public Collection<String> getResources() {
        return resources;
    }

    public List<String> getDirectories() {
        return directories;
    }

    public List<String> getSourceDirs() {
        return sourceDirs;
    }

    public List<String> getIncludeDirs() {
        return includeDirs;
    }

    public String getBeamDir() {
        return beamDir;
    }
}
