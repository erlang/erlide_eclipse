package org.erlide.runtime.runtimeinfo;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.ListExtensions;
import org.erlide.util.ErlLogger;
import org.erlide.util.SystemConfiguration;

import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

@SuppressWarnings("all")
public class RuntimeFinder {
    private static final List<String> locations = Collections.<String> unmodifiableList(
            CollectionLiterals.<String> newArrayList("c:/program files",
                    "c:/program files (x86)", "c:/programs", "c:/", "c:/apps", "/usr",
                    "/usr/lib", "/usr/lib64", "/usr/local", "/usr/local/lib",
                    "/Library/Frameworks/erlang/Versions"));

    public static Collection<RuntimeInfo> guessRuntimeLocations() {
        final List<RuntimeInfo> result = CollectionLiterals.<RuntimeInfo> newArrayList();
        final String homeDir = SystemConfiguration.getInstance().getHomeDir();
        final String[] syspath = System.getenv("PATH").split(File.pathSeparator);
        final Iterable<String> _plus = Iterables.<String> concat(
                (Iterable<? extends String>) Conversions.doWrapArray(syspath),
                RuntimeFinder.locations);
        final Set<String> locs = CollectionLiterals.<String> newHashSet(
                (String[]) Conversions.unwrapArray(_plus, String.class));
        locs.add(homeDir);
        final String envRuntime = System.getProperty("erlide.runtime");
        if (envRuntime != null) {
            locs.add(envRuntime);
        }
        Iterables.<String> addAll(locs, RuntimeFinder.getKerlLocations());
        for (final String loc : locs) {
            {
                final Collection<File> roots = RuntimeFinder.findRuntime(loc);
                for (final File root : roots) {
                    {
                        final RuntimeInfo rt = new RuntimeInfo.Builder()
                                .withName(root.getName()).withHomeDir(root.getPath())
                                .build();
                        result.add(rt);
                    }
                }
            }
        }
        return result;
    }

    public static Iterable<String> getKerlLocations() {
        final List<String> result = Lists.<String> newArrayList();
        final ProcessBuilder builder = new ProcessBuilder(
                Collections.<String> unmodifiableList(CollectionLiterals
                        .<String> newArrayList("kerl", "list", "installations")));
        try {
            final Process process = builder.start();
            final StringBuilder line = new StringBuilder();
            int chr = 0;
            while ((chr = process.getInputStream().read()) != -1) {
                if (chr == 10 || chr == 13) {
                    final int _length = line.length();
                    final boolean _notEquals = _length != 0;
                    if (_notEquals) {
                        result.add(line.toString());
                        line.setLength(0);
                    }
                } else {
                    line.append((char) chr);
                }
            }
            final int _length = line.length();
            final boolean _notEquals = _length != 0;
            if (_notEquals) {
                result.add(line.toString());
            }
        } catch (final Throwable _t) {
            if (_t instanceof IOException) {
                ErlLogger.info("kerl not found");
            } else {
                throw Exceptions.sneakyThrow(_t);
            }
        }
        final Function1<String, String> _function = (final String it) -> {
            String _xifexpression = null;
            final int _length = it.split(" ", 2).length;
            final boolean _greaterEqualsThan = _length >= 2;
            if (_greaterEqualsThan) {
                _xifexpression = it.split(" ", 2)[1];
            } else {
                _xifexpression = null;
            }
            return _xifexpression;
        };
        return IterableExtensions.<String> filterNull(
                ListExtensions.<String, String> map(result, _function));
    }

    private static Collection<File> findRuntime(final String loc) {
        final Collection<File> result = CollectionLiterals.<File> newArrayList();
        if (loc == null) {
            return result;
        }
        final File folder = new File(loc);
        final boolean _exists = folder.exists();
        final boolean _not = !_exists;
        if (_not) {
            return result;
        }
        final boolean _validateLocation = RuntimeInfo.validateLocation(folder.getPath());
        if (_validateLocation) {
            result.add(folder);
            return result;
        }
        final File[] files = folder.listFiles();
        if (files != null) {
            for (final File f : files) {
                if (f.isDirectory() && RuntimeInfo.validateLocation(f.getPath())) {
                    result.add(f);
                }
            }
        }
        return result;
    }
}
