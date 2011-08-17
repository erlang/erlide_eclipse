package org.erlide.core.backend.runtimeinfo;

import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.preferences.DefaultScope;

public class RuntimeInfoInitializer {
    RuntimeInfoManager runtimeInfoManager;

    public RuntimeInfoInitializer(final RuntimeInfoManager runtimeInfoManager) {
        this.runtimeInfoManager = runtimeInfoManager;
    }

    /**
     * If runtime is not set, try to locate one. The first one found as below is
     * set as default. All "obvious" runtimes found are stored.
     * <ul>
     * <li>A system property <code>erlide.runtime</code> can be set to point to
     * a location.</li>
     * <li>A preference in the default scope
     * <code>org.erlide.core/default_runtime</code> can be set to point to a
     * location.</li>
     * <li>Look for existing Erlang runtimes in a few obvious places and install
     * them, choosing a suitable one as default.</li>
     * </ul>
     * 
     */
    public void initializeRuntimesList() {
        if (runtimeInfoManager.getDefaultRuntime() != null) {
            return;
        }
        guessRuntimeLocations();
        setDefaultRuntimes();
    }

    private void setDefaultRuntimes() {
        final List<RuntimeInfo> list = new ArrayList<RuntimeInfo>(
                runtimeInfoManager.getRuntimes());
        Collections.sort(list, new Comparator<RuntimeInfo>() {
            public int compare(final RuntimeInfo o1, final RuntimeInfo o2) {
                final int x = o2.getVersion().compareTo(o1.getVersion());
                if (x != 0) {
                    return x;
                }
                return o2.getName().compareTo(o1.getName());
            }
        });
        if (list.size() > 0) {
            runtimeInfoManager.setDefaultRuntime(list.get(0).getName());
            runtimeInfoManager.setErlideRuntime(runtimeInfoManager
                    .getDefaultRuntime());
        }
    }

    private void guessRuntimeLocations() {
        final String[] locations = {
                System.getProperty("erlide.runtime"),
                new DefaultScope().getNode("org.erlide.core").get(
                        "default_runtime", null), "c:/program files",
                "c:/program files (x86)", "c:/programs", "c:/", "c:/apps",
                System.getProperty("user.home"), "/usr", "/usr/local",
                "/usr/local/lib", "/Library/Frameworks/erlang/Versions" };
        for (final String loc : locations) {
            final Collection<File> roots = findRuntime(loc);
            for (final File root : roots) {
                final RuntimeInfo rt = new RuntimeInfo();
                rt.setOtpHome(root.getPath());
                rt.setName(root.getName());
                final IWorkspaceRoot wroot = ResourcesPlugin.getWorkspace()
                        .getRoot();
                final String location = wroot.getLocation().toPortableString();
                rt.setWorkingDir(location);
                runtimeInfoManager.addRuntime(rt);
            }
        }
    }

    private static Collection<File> findRuntime(final String loc) {
        final Collection<File> result = new ArrayList<File>();
        if (loc == null) {
            return result;
        }
        final File folder = new File(loc);
        if (!folder.exists()) {
            return result;
        }
        final File[] candidates = folder.listFiles(new FileFilter() {
            public boolean accept(final File pathname) {
                return pathname.isDirectory()
                        && (pathname.getName().startsWith("erl")
                                || pathname.getName().startsWith("Erl") || pathname
                                .getName().startsWith("R"));
            }
        });
        for (final File f : candidates) {
            if (RuntimeInfo.validateLocation(f.getPath())) {
                result.add(f);
            }
        }
        return result;
    }

}
