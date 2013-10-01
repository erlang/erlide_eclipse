package org.erlide.core.internal.builder;

import com.google.common.base.Charsets;
import com.google.common.base.Objects;
import com.google.common.io.Files;
import java.io.File;
import java.net.URI;
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

@SuppressWarnings("all")
public class ErlangToolExtensions {
  private static boolean hasTopFile(final IContainer container, final String filename) {
    IResource _topFile = ErlangToolExtensions.getTopFile(container, filename);
    return (!Objects.equal(_topFile, null));
  }
  
  public static IResource getTopFile(final IContainer container, final String filename) {
    try {
      IResource[] _members = container.members();
      final Function1<IResource,Boolean> _function = new Function1<IResource,Boolean>() {
        public Boolean apply(final IResource it) {
          String _name = it.getName();
          boolean _equals = Objects.equal(_name, filename);
          return Boolean.valueOf(_equals);
        }
      };
      return IterableExtensions.<IResource>findFirst(((Iterable<IResource>)Conversions.doWrapArray(_members)), _function);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public static boolean hasRebarConfig(final IProject project) {
    boolean _hasTopFile = ErlangToolExtensions.hasTopFile(project, "rebar.config");
    return _hasTopFile;
  }
  
  public static boolean hasMakefile(final IProject project) {
    boolean _hasTopFile = ErlangToolExtensions.hasTopFile(project, "Makefile");
    return _hasTopFile;
  }
  
  public static boolean hasConcreteMk(final IProject project) {
    boolean _hasTopFile = ErlangToolExtensions.hasTopFile(project, "concrete.mk");
    return _hasTopFile;
  }
  
  public static boolean hasErlangMk(final IProject project) {
    boolean _hasTopFile = ErlangToolExtensions.hasTopFile(project, "erlang.mk");
    return _hasTopFile;
  }
  
  public static boolean hasEmakefile(final IProject project) {
    boolean _hasTopFile = ErlangToolExtensions.hasTopFile(project, "Emakefile");
    return _hasTopFile;
  }
  
  public static boolean isUniversalMake(final IFile makefile) {
    try {
      final File file = ErlangToolExtensions.getRealFile(makefile);
      boolean _equals = Objects.equal(file, null);
      if (_equals) {
        return false;
      }
      final String top = Files.readFirstLine(file, Charsets.ISO_8859_1);
      return Objects.equal(top, "# Copyright 2012 Erlware, LLC. All Rights Reserved.");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public static File getRealFile(final IResource ifile) {
    try {
      File _xblockexpression = null;
      {
        final URI uri = ifile.getRawLocationURI();
        boolean _equals = Objects.equal(uri, null);
        if (_equals) {
          return null;
        }
        IFileStore _store = EFS.getStore(uri);
        NullProgressMonitor _nullProgressMonitor = new NullProgressMonitor();
        File _localFile = _store.toLocalFile(0, _nullProgressMonitor);
        _xblockexpression = (_localFile);
      }
      return _xblockexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public static Iterable<String> getMakeTargets(final IFile makefile) {
    try {
      Iterable<String> _xblockexpression = null;
      {
        File _realFile = ErlangToolExtensions.getRealFile(makefile);
        final List<String> lines = Files.readLines(_realFile, Charsets.ISO_8859_1);
        final Function1<String,String> _function = new Function1<String,String>() {
          public String apply(final String it) {
            String _xifexpression = null;
            boolean _hasTarget = ErlangToolExtensions.hasTarget(it);
            if (_hasTarget) {
              String[] _split = it.split(":");
              String _head = IterableExtensions.<String>head(((Iterable<String>)Conversions.doWrapArray(_split)));
              _xifexpression = _head;
            } else {
              _xifexpression = null;
            }
            return _xifexpression;
          }
        };
        List<String> _map = ListExtensions.<String, String>map(lines, _function);
        Iterable<String> _filterNull = IterableExtensions.<String>filterNull(_map);
        _xblockexpression = (_filterNull);
      }
      return _xblockexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  private static boolean hasTarget(final String line) {
    boolean _matches = line.matches("[a-z0-9_-]+:.*");
    return _matches;
  }
}
