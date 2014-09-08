package org.erlide.runtime.runtimeinfo;

import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import org.erlide.util.SystemConfiguration;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

public class RuntimeFinder {

    public static Collection<RuntimeInfo> guessRuntimeLocations() {
        final List<RuntimeInfo> result = Lists.newArrayList();
        final String[] locations = { "c:/program files", "c:/program files (x86)",
                "c:/programs", "c:/", "c:/apps", "/usr", "/usr/lib", "/usr/lib64",
                "/usr/local", "/usr/local/lib", "/Library/Frameworks/erlang/Versions",
                "/proj/uz/erlide", SystemConfiguration.getInstance().getHomeDir(),
                SystemConfiguration.getInstance().getHomeDir() + "/erlide_tools",
                System.getProperty("erlide.runtime") };
        final Set<String> locs = Sets.newHashSet(Arrays.asList(locations));
        for (final String loc : locs) {
            final Collection<File> roots = findRuntime(loc);
            for (final File root : roots) {
                final RuntimeInfo rt = new RuntimeInfo.Builder().withName(root.getName())
                        .withHomeDir(root.getPath()).build();
                result.add(rt);
            }
        }
        return result;
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
        if (RuntimeInfo.validateLocation(folder.getPath())) {
            result.add(folder);
            return result;
        }
        final File[] candidates = folder.listFiles(new FileFilter() {
            @Override
            public boolean accept(final File pathname) {
                final String path = pathname.getName();
                return pathname.isDirectory()
                        && (path.startsWith("otp") || path.startsWith("erl")
                                || path.startsWith("Erl") || path.startsWith("R"));
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
