package org.erlide.launch.debug.model;

import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.debug.core.sourcelookup.ISourceContainer;
import org.eclipse.debug.core.sourcelookup.ISourceContainerType;
import org.eclipse.debug.core.sourcelookup.containers.CompositeSourceContainer;
import org.eclipse.debug.core.sourcelookup.containers.DirectorySourceContainer;

public class ErlangOtpSourceContainer extends CompositeSourceContainer
        implements ISourceContainer {

    private final IPath otpHome;

    public ErlangOtpSourceContainer(final IPath otpHome) {
        this.otpHome = otpHome;
    }

    @Override
    public String getName() {
        return otpHome.toString();
    }

    private int versionFromName(final String name) {
        final String[] parts = name.split("-");
        if (parts.length < 2) {
            return 0;
        }
        final String[] digits = parts[parts.length - 1].split("\\.");
        int r = 0, f = 100000000;
        for (final String d : digits) {
            r += Integer.parseInt(d) * f;
            f /= 100;
        }
        return r;
    }

    private String highestVersion(final File directory, final String prefix) {
        final String candidates[] = directory.list(new FilenameFilter() {
            @Override
            public boolean accept(final File dir, final String name) {
                return name.startsWith(prefix);
            }
        });
        if (candidates.length == 0) {
            return null;
        }
        String r = candidates[0];
        int v = 0;
        for (final String candidate : candidates) {
            final int version = versionFromName(candidate);
            if (version > v) {
                v = version;
                r = candidate;
            }
        }
        return r;
    }

    private Set<String> modules(final File directory) {
        final Set<String> r = new TreeSet<String>();
        final String n[] = directory.list();
        for (final String f : n) {
            final String[] parts = f.split("-");
            if (parts.length > 1) {
                r.add(parts[0]);
            }
        }
        return r;
    }

    @Override
    public ISourceContainerType getType() {
        // FIXME implement this properly!
        return new ISourceContainerType() {

            @Override
            public String getMemento(final ISourceContainer container)
                    throws CoreException {
                return null;
            }

            @Override
            public ISourceContainer createSourceContainer(final String memento)
                    throws CoreException {
                return null;
            }

            @Override
            public String getName() {
                return null;
            }

            @Override
            public String getId() {
                return null;
            }

            @Override
            public String getDescription() {
                return null;
            }
        };
    }

    @Override
    protected ISourceContainer[] createSourceContainers() throws CoreException {
        final IPath lib = otpHome.addTrailingSeparator().append("lib")
                .addTrailingSeparator();
        final Set<String> moduleNames = modules(lib.toFile());
        final List<IPath> moduleDirs = new ArrayList<IPath>();
        for (final String moduleName : moduleNames) {
            final String h = highestVersion(lib.toFile(), moduleName);
            if (h != null) {
                moduleDirs.add(lib.addTrailingSeparator().append(h));
            }
        }
        final List<ISourceContainer> r = new ArrayList<ISourceContainer>();
        for (final IPath dir : moduleDirs) {
            r.add(new DirectorySourceContainer(dir.addTrailingSeparator()
                    .append("src"), false));
        }
        return r.toArray(new ISourceContainer[r.size()]);
    }

}
