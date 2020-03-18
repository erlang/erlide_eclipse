package org.erlide.core.builder;

import java.io.File;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.List;

import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.filesystem.IFileStore;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.ListExtensions;

import com.google.common.base.Objects;
import com.google.common.io.Files;

@SuppressWarnings("all")
public class ErlangToolExtensions {
    private static boolean hasTopFile(final IContainer container, final String filename) {
        final IResource _topFile = ErlangToolExtensions.getTopFile(container, filename);
        return _topFile != null;
    }

    public static IResource getTopFile(final IContainer container,
            final String filename) {
        try {
            final Function1<IResource, Boolean> _function = (final IResource it) -> {
                final String _name = it.getName();
                return Boolean.valueOf(Objects.equal(_name, filename));
            };
            return IterableExtensions.<IResource> findFirst(
                    (Iterable<IResource>) Conversions.doWrapArray(container.members()),
                    _function);
        } catch (final Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
        }
    }

    public static boolean isUniversalMake(final IFile makefile) {
        try {
            final File file = ErlangToolExtensions.getRealFile(makefile);
            if (file == null) {
                return false;
            }
            final String top = Files.asCharSource(file, StandardCharsets.ISO_8859_1)
                    .readFirstLine();
            return Objects.equal(top,
                    "# Copyright 2012 Erlware, LLC. All Rights Reserved.");
        } catch (final Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
        }
    }

    public static File getRealFile(final IResource ifile) {
        try {
            File _xblockexpression = null;
            {
                final URI uri = ifile.getRawLocationURI();
                if (uri == null) {
                    return null;
                }
                final IFileStore _store = EFS.getStore(uri);
                final NullProgressMonitor _nullProgressMonitor = new NullProgressMonitor();
                _xblockexpression = _store.toLocalFile(0, _nullProgressMonitor);
            }
            return _xblockexpression;
        } catch (final Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
        }
    }

    public static Iterable<String> getMakefileTargets(final IFile makefile) {
        try {
            Iterable<String> _xblockexpression = null;
            {
                final List<String> lines = Files.readLines(
                        ErlangToolExtensions.getRealFile(makefile),
                        StandardCharsets.ISO_8859_1);
                final Function1<String, String> _function = (final String it) -> {
                    String _xifexpression = null;
                    final boolean _hasTarget = ErlangToolExtensions.hasTarget(it);
                    if (_hasTarget) {
                        _xifexpression = IterableExtensions
                                .<String> head((Iterable<String>) Conversions
                                        .doWrapArray(it.split(":")));
                    } else {
                        _xifexpression = null;
                    }
                    return _xifexpression;
                };
                _xblockexpression = IterableExtensions.<String> filterNull(
                        ListExtensions.<String, String> map(lines, _function));
            }
            return _xblockexpression;
        } catch (final Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
        }
    }

    private static boolean hasTarget(final String line) {
        return line.matches("[a-z0-9_-]+:.*");
    }

    public static boolean buildsWithMake(final IProject project) {
        return ErlangToolExtensions.hasTopFile(project, "Makefile")
                && ErlangToolExtensions.hasMakeBuilderEnabled(project);
    }

    public static boolean buildsWithEmake(final IProject project) {
        return ErlangToolExtensions.hasTopFile(project, "Emakefile")
                && ErlangToolExtensions.hasEmakeBuilderEnabled(project);
    }

    public static boolean buildsWithRebar(final IProject project) {
        return ErlangToolExtensions.hasTopFile(project, "rebar.config")
                && ErlangToolExtensions.hasRebarBuilderEnabled(project);
    }

    public static boolean hasMakeBuilderEnabled(final IProject project) {
        return false;
    }

    public static boolean hasEmakeBuilderEnabled(final IProject project) {
        return false;
    }

    public static boolean hasRebarBuilderEnabled(final IProject project) {
        return false;
    }
}
