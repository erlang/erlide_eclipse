package org.erlide.util;

import com.google.common.base.Objects;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Collections;
import java.util.List;
import java.util.Properties;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.erlide.util.ErlLogger;
import org.erlide.util.ErlangHostnameRetriever;
import org.erlide.util.MessageReporter;
import org.osgi.framework.Bundle;

@SuppressWarnings("all")
public class HostnameChecker {
  private static class Holder {
    private final static HostnameChecker INSTANCE = new HostnameChecker();
  }
  
  private final static String longNameFallback = "127.0.0.1";
  
  private final static String shortNameFallback = "localhost";
  
  private String longName;
  
  private String shortName;
  
  private HostnameChecker() {
  }
  
  public static HostnameChecker getInstance() {
    return HostnameChecker.Holder.INSTANCE;
  }
  
  public String getErlangHostName(final boolean useLongName) {
    String _xifexpression = null;
    if (useLongName) {
      _xifexpression = this.longName;
    } else {
      _xifexpression = this.shortName;
    }
    return _xifexpression;
  }
  
  public boolean isThisHost(final String host) {
    return (Objects.equal(host, this.longName) || Objects.equal(host, this.shortName));
  }
  
  public boolean canUseLongNames() {
    return (this.longName != null);
  }
  
  public boolean canUseShortNames() {
    return (this.shortName != null);
  }
  
  public String getJavaLongHostName() {
    String _xblockexpression = null;
    {
      String name = null;
      try {
        final InetAddress addr = InetAddress.getLocalHost();
        String _canonicalHostName = addr.getCanonicalHostName();
        name = _canonicalHostName;
        boolean _contains = name.contains(".");
        boolean _not = (!_contains);
        if (_not) {
          name = null;
        }
      } catch (final Throwable _t) {
        if (_t instanceof UnknownHostException) {
          final UnknownHostException e1 = (UnknownHostException)_t;
        } else {
          throw Exceptions.sneakyThrow(_t);
        }
      }
      _xblockexpression = name;
    }
    return _xblockexpression;
  }
  
  public String getJavaShortHostName() {
    String _xblockexpression = null;
    {
      String name = null;
      try {
        final InetAddress addr = InetAddress.getLocalHost();
        String _hostName = addr.getHostName();
        name = _hostName;
        boolean _contains = name.contains(".");
        if (_contains) {
          name = null;
        }
      } catch (final Throwable _t) {
        if (_t instanceof UnknownHostException) {
          final UnknownHostException e1 = (UnknownHostException)_t;
        } else {
          throw Exceptions.sneakyThrow(_t);
        }
      }
      _xblockexpression = name;
    }
    return _xblockexpression;
  }
  
  /**
   * Start erlang nodes and find out how they resolve the long/short host names.
   */
  public boolean detectHostNames(final String otpHome) {
    this.notifyDeprecatedUsage();
    final ErlangHostnameRetriever retriever = new ErlangHostnameRetriever(otpHome);
    final boolean loadProperties = this.getErlideHostsFromProperties();
    if (loadProperties) {
      return true;
    }
    String _hostsFileName = this.getHostsFileName();
    final boolean loaded = this.loadErlideHosts(_hostsFileName);
    if (loaded) {
      if ((retriever.canConnect(this.shortName, false) || retriever.canConnect(this.longName, true))) {
        return true;
      }
    }
    final Function0<String> _function = new Function0<String>() {
      @Override
      public String apply() {
        return HostnameChecker.this.longName;
      }
    };
    final Function0<String> _function_1 = new Function0<String>() {
      @Override
      public String apply() {
        return retriever.getErlangHostName(true);
      }
    };
    final Function0<String> _function_2 = new Function0<String>() {
      @Override
      public String apply() {
        return HostnameChecker.this.getJavaLongHostName();
      }
    };
    final Function0<String> _function_3 = new Function0<String>() {
      @Override
      public String apply() {
        return HostnameChecker.longNameFallback;
      }
    };
    final Iterable<Function0<? extends String>> longValues = Collections.<Function0<? extends String>>unmodifiableList(CollectionLiterals.<Function0<? extends String>>newArrayList(_function, _function_1, _function_2, _function_3));
    final Function1<String, Boolean> _function_4 = new Function1<String, Boolean>() {
      @Override
      public Boolean apply(final String it) {
        return Boolean.valueOf(((it != null) && retriever.canConnect(it, true)));
      }
    };
    String _findFirstValue = this.findFirstValue(longValues, _function_4);
    this.longName = _findFirstValue;
    final Function0<String> _function_5 = new Function0<String>() {
      @Override
      public String apply() {
        return HostnameChecker.this.shortName;
      }
    };
    final Function0<String> _function_6 = new Function0<String>() {
      @Override
      public String apply() {
        return retriever.getErlangHostName(false);
      }
    };
    final Function0<String> _function_7 = new Function0<String>() {
      @Override
      public String apply() {
        return HostnameChecker.this.getJavaShortHostName();
      }
    };
    final Function0<String> _function_8 = new Function0<String>() {
      @Override
      public String apply() {
        return HostnameChecker.shortNameFallback;
      }
    };
    final Iterable<Function0<? extends String>> shortValues = Collections.<Function0<? extends String>>unmodifiableList(CollectionLiterals.<Function0<? extends String>>newArrayList(_function_5, _function_6, _function_7, _function_8));
    final Function1<String, Boolean> _function_9 = new Function1<String, Boolean>() {
      @Override
      public Boolean apply(final String it) {
        return Boolean.valueOf(((it != null) && retriever.canConnect(it, false)));
      }
    };
    String _findFirstValue_1 = this.findFirstValue(shortValues, _function_9);
    this.shortName = _findFirstValue_1;
    ErlLogger.debug("Detected:: \'%s\' && \'%s\'", this.shortName, this.longName);
    return (this.canUseLongNames() || this.canUseShortNames());
  }
  
  public boolean getErlideHostsFromProperties() {
    String _property = System.getProperty("erlide.host.short");
    this.shortName = _property;
    String _property_1 = System.getProperty("erlide.host.long");
    this.longName = _property_1;
    return (this.canUseLongNames() || this.canUseShortNames());
  }
  
  public List<List<Function0<? extends String>>> getAllHostNameValues(final String otpHome) {
    List<List<Function0<? extends String>>> _xblockexpression = null;
    {
      final Properties p = this.readErlideHosts();
      final ErlangHostnameRetriever retriever = new ErlangHostnameRetriever(otpHome);
      final Function0<String> _function = new Function0<String>() {
        @Override
        public String apply() {
          return p.getProperty("long", "");
        }
      };
      final Function0<String> _function_1 = new Function0<String>() {
        @Override
        public String apply() {
          return retriever.getErlangHostName(true);
        }
      };
      final Function0<String> _function_2 = new Function0<String>() {
        @Override
        public String apply() {
          return HostnameChecker.this.getJavaLongHostName();
        }
      };
      final Function0<String> _function_3 = new Function0<String>() {
        @Override
        public String apply() {
          return HostnameChecker.longNameFallback;
        }
      };
      final Function0<String> _function_4 = new Function0<String>() {
        @Override
        public String apply() {
          return p.getProperty("short", "");
        }
      };
      final Function0<String> _function_5 = new Function0<String>() {
        @Override
        public String apply() {
          return retriever.getErlangHostName(false);
        }
      };
      final Function0<String> _function_6 = new Function0<String>() {
        @Override
        public String apply() {
          return HostnameChecker.this.getJavaShortHostName();
        }
      };
      final Function0<String> _function_7 = new Function0<String>() {
        @Override
        public String apply() {
          return HostnameChecker.shortNameFallback;
        }
      };
      _xblockexpression = Collections.<List<Function0<? extends String>>>unmodifiableList(CollectionLiterals.<List<Function0<? extends String>>>newArrayList(Collections.<Function0<? extends String>>unmodifiableList(CollectionLiterals.<Function0<? extends String>>newArrayList(_function, _function_1, _function_2, _function_3)), Collections.<Function0<? extends String>>unmodifiableList(CollectionLiterals.<Function0<? extends String>>newArrayList(_function_4, _function_5, _function_6, _function_7))));
    }
    return _xblockexpression;
  }
  
  private String findFirstValue(final Iterable<Function0<? extends String>> list, final Function1<? super String, ? extends Boolean> predicate) {
    String _xblockexpression = null;
    {
      if (((list == null) || IterableExtensions.isEmpty(list))) {
        return null;
      }
      Function0<? extends String> _head = IterableExtensions.<Function0<? extends String>>head(list);
      final String value = _head.apply();
      String _xifexpression = null;
      Boolean _apply = predicate.apply(value);
      if ((_apply).booleanValue()) {
        _xifexpression = value;
      } else {
        Iterable<Function0<? extends String>> _tail = IterableExtensions.<Function0<? extends String>>tail(list);
        _xifexpression = this.findFirstValue(_tail, predicate);
      }
      _xblockexpression = _xifexpression;
    }
    return _xblockexpression;
  }
  
  private void notifyDeprecatedUsage() {
    if (((System.getProperty("erlide.long.name") != null) || (System.getProperty("erlide.short.name") != null))) {
      final Job job = ((Job) new Job("") {
        @Override
        protected IStatus run(final IProgressMonitor monitor) {
          Bundle _bundle = Platform.getBundle("org.erlide.ui");
          int _state = _bundle.getState();
          boolean _tripleNotEquals = (_state != Bundle.ACTIVE);
          if (_tripleNotEquals) {
            this.schedule(500);
          } else {
            StringConcatenation _builder = new StringConcatenation();
            _builder.append("Deprecation notice: the system properties erlide.long.name or erlide.short.name to set the host names to be used by both Erlang and Java.");
            _builder.newLine();
            _builder.newLine();
            _builder.append("The new way to do that is to edit ~/.erlide.hosts and change the values there if they aren\'t correct.");
            _builder.newLine();
            _builder.append("Please remove the use of the system properties.");
            MessageReporter.showInfo(_builder.toString());
          }
          return Status.OK_STATUS;
        }
      });
      job.schedule(500);
    }
  }
  
  private boolean loadErlideHosts(final String hostsFileName) {
    boolean _xblockexpression = false;
    {
      final Properties props = new Properties();
      boolean loaded = false;
      try {
        final File f = new File(hostsFileName);
        final FileInputStream is = new FileInputStream(f);
        try {
          props.load(is);
          loaded = true;
        } finally {
          is.close();
        }
      } catch (final Throwable _t) {
        if (_t instanceof Exception) {
          final Exception e = (Exception)_t;
        } else {
          throw Exceptions.sneakyThrow(_t);
        }
      }
      String _property = props.getProperty("long", null);
      this.longName = _property;
      String _property_1 = props.getProperty("short", null);
      this.shortName = _property_1;
      _xblockexpression = (loaded && (this.canUseLongNames() || this.canUseShortNames()));
    }
    return _xblockexpression;
  }
  
  public Properties readErlideHosts() {
    Properties _xblockexpression = null;
    {
      final Properties props = new Properties();
      boolean loaded = false;
      try {
        String _hostsFileName = this.getHostsFileName();
        final File f = new File(_hostsFileName);
        final FileInputStream is = new FileInputStream(f);
        try {
          props.load(is);
          loaded = true;
        } finally {
          is.close();
        }
      } catch (final Throwable _t) {
        if (_t instanceof Exception) {
          final Exception e = (Exception)_t;
        } else {
          throw Exceptions.sneakyThrow(_t);
        }
      }
      _xblockexpression = props;
    }
    return _xblockexpression;
  }
  
  public void saveErlideHosts(final String longName, final String shortName) {
    try {
      final Properties props = new Properties();
      props.put("long", longName);
      props.put("short", shortName);
      String _hostsFileName = this.getHostsFileName();
      final File f = new File(_hostsFileName);
      final OutputStream out = new FileOutputStream(f);
      try {
        props.store(out, null);
      } finally {
        out.close();
      }
      String _hostsFileName_1 = this.getHostsFileName();
      ErlLogger.debug("  # written to %s", _hostsFileName_1);
    } catch (final Throwable _t) {
      if (_t instanceof IOException) {
        final IOException e = (IOException)_t;
        e.printStackTrace();
      } else {
        throw Exceptions.sneakyThrow(_t);
      }
    }
  }
  
  private String getHostsFileName() {
    StringConcatenation _builder = new StringConcatenation();
    String _property = System.getProperty("user.home");
    _builder.append(_property, "");
    _builder.append("/.erlide.hosts");
    return _builder.toString();
  }
}
