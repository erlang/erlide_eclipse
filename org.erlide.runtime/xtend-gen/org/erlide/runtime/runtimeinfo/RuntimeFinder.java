package org.erlide.runtime.runtimeinfo;

import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
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
import org.eclipse.xtext.xbase.lib.ListExtensions;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.util.SystemConfiguration;

@SuppressWarnings("all")
public class RuntimeFinder {
  private final static List<String> locations = Collections.<String>unmodifiableList(CollectionLiterals.<String>newArrayList("c:/program files", "c:/program files (x86)", "c:/programs", "c:/", "c:/apps", "/usr", "/usr/lib", "/usr/lib64", "/usr/local", "/usr/local/lib", "/Library/Frameworks/erlang/Versions"));
  
  public static Collection<RuntimeInfo> guessRuntimeLocations() {
    final List<RuntimeInfo> result = CollectionLiterals.<RuntimeInfo>newArrayList();
    SystemConfiguration _instance = SystemConfiguration.getInstance();
    final String homeDir = _instance.getHomeDir();
    String _env = System.getenv("PATH");
    final String[] syspath = _env.split(File.pathSeparator);
    Iterable<String> _plus = Iterables.<String>concat(((Iterable<? extends String>)Conversions.doWrapArray(syspath)), RuntimeFinder.locations);
    final Set<String> locs = CollectionLiterals.<String>newHashSet(((String[])Conversions.unwrapArray(_plus, String.class)));
    locs.add(homeDir);
    final String envRuntime = System.getProperty("erlide.runtime");
    if ((envRuntime != null)) {
      locs.add(envRuntime);
    }
    Collection<String> _kerlLocations = RuntimeFinder.getKerlLocations();
    locs.addAll(_kerlLocations);
    for (final String loc : locs) {
      {
        final Collection<File> roots = RuntimeFinder.findRuntime(loc);
        for (final File root : roots) {
          {
            RuntimeInfo.Builder _builder = new RuntimeInfo.Builder();
            String _name = root.getName();
            RuntimeInfo.Builder _withName = _builder.withName(_name);
            String _path = root.getPath();
            RuntimeInfo.Builder _withHomeDir = _withName.withHomeDir(_path);
            final RuntimeInfo rt = _withHomeDir.build();
            result.add(rt);
          }
        }
      }
    }
    return result;
  }
  
  public static Collection<String> getKerlLocations() {
    final List<String> result = Lists.<String>newArrayList();
    final ProcessBuilder builder = new ProcessBuilder(Collections.<String>unmodifiableList(CollectionLiterals.<String>newArrayList("kerl", "list", "installations")));
    try {
      final Process process = builder.start();
      final StringBuilder line = new StringBuilder();
      int chr = 0;
      while (((chr = process.getInputStream().read()) != (-1))) {
        if (((chr == 10) || (chr == 13))) {
          int _length = line.length();
          boolean _notEquals = (_length != 0);
          if (_notEquals) {
            String _string = line.toString();
            result.add(_string);
            line.setLength(0);
          }
        } else {
          line.append(((char) chr));
        }
      }
      int _length = line.length();
      boolean _notEquals = (_length != 0);
      if (_notEquals) {
        String _string = line.toString();
        result.add(_string);
      }
    } catch (final Throwable _t) {
      if (_t instanceof IOException) {
        final IOException e = (IOException)_t;
      } else {
        throw Exceptions.sneakyThrow(_t);
      }
    }
    final Function1<String, String> _function = new Function1<String, String>() {
      @Override
      public String apply(final String it) {
        String[] _split = it.split(" ", 2);
        return _split[1];
      }
    };
    return ListExtensions.<String, String>map(result, _function);
  }
  
  private static Collection<File> findRuntime(final String loc) {
    final Collection<File> result = CollectionLiterals.<File>newArrayList();
    if ((loc == null)) {
      return result;
    }
    final File folder = new File(loc);
    boolean _exists = folder.exists();
    boolean _not = (!_exists);
    if (_not) {
      return result;
    }
    String _path = folder.getPath();
    boolean _validateLocation = RuntimeInfo.validateLocation(_path);
    if (_validateLocation) {
      result.add(folder);
      return result;
    }
    final File[] files = folder.listFiles();
    if ((files != null)) {
      for (final File f : files) {
        if ((f.isDirectory() && RuntimeInfo.validateLocation(f.getPath()))) {
          result.add(f);
        }
      }
    }
    return result;
  }
}
