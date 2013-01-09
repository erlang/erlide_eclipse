package org.erlide.runtime.runtimeinfo;

import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import com.google.common.collect.Lists;

public class RuntimeFinder {

    public static Collection<RuntimeInfo> guessRuntimeLocations() {
        final List<RuntimeInfo> result = Lists.newArrayList();
        final String[] locations = { System.getProperty("erlide.runtime"),
                "c:/program files", "c:/program files (x86)", "c:/programs",
                "c:/", "c:/apps", System.getProperty("user.home"), "/usr",
                "/usr/lib", "/usr/lib64", "/usr/local", "/usr/local/lib",
                "/Library/Frameworks/erlang/Versions", "/proj/uz/erlide" };
        for (final String loc : locations) {
            final Collection<File> roots = findRuntime(loc);
            for (final File root : roots) {
                final RuntimeInfo rt = new RuntimeInfo(root.getName())
                        .setOtpHome(root.getPath());
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
                                || path.startsWith("Erl") || path
                                    .startsWith("R"));
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
