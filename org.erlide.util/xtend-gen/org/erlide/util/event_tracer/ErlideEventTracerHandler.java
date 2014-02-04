package org.erlide.util.event_tracer;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.Inet4Address;
import java.net.InetAddress;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.erlide.util.ErlLogger;
import org.erlide.util.IDisposable;
import org.erlide.util.event_tracer.ErlideEvent;
import org.erlide.util.event_tracer.ErlideSessionEvent;

@SuppressWarnings("all")
public class ErlideEventTracerHandler implements IDisposable {
  protected String user = System.getProperty("user.name");
  
  protected String machine = new Function0<String>() {
    public String apply() {
      try {
        InetAddress _localHost = Inet4Address.getLocalHost();
        String _canonicalHostName = _localHost.getCanonicalHostName();
        return _canonicalHostName;
      } catch (Throwable _e) {
        throw Exceptions.sneakyThrow(_e);
      }
    }
  }.apply();
  
  private final IPath storagePath;
  
  private PrintWriter file;
  
  private final SimpleDateFormat formatter = new SimpleDateFormat("yyyyMMdd-HHmmss-SSS");
  
  public ErlideEventTracerHandler(final String path) {
    boolean _tripleEquals = (path == null);
    if (_tripleEquals) {
      this.storagePath = null;
      return;
    }
    Path _path = new Path(path);
    IPath _append = _path.append(this.machine);
    IPath _append_1 = _append.append(this.user);
    this.storagePath = _append_1;
    String _portableString = this.storagePath.toPortableString();
    File _file = new File(_portableString);
    _file.mkdirs();
  }
  
  protected void _handle(final ErlideSessionEvent event) {
    boolean _tripleEquals = (this.storagePath == null);
    if (_tripleEquals) {
      return;
    }
    long _timestamp = event.getTimestamp();
    final Date date = new Date(_timestamp);
    final String sdate = this.formatter.format(date);
    String _hexString = Integer.toHexString(event.workspace);
    IPath _append = this.storagePath.append(_hexString);
    IPath _append_1 = _append.append((sdate + ".log"));
    final String name = _append_1.toPortableString();
    try {
      FileWriter _fileWriter = new FileWriter(name, false);
      BufferedWriter _bufferedWriter = new BufferedWriter(_fileWriter);
      PrintWriter _printWriter = new PrintWriter(_bufferedWriter);
      this.file = _printWriter;
    } catch (final Throwable _t) {
      if (_t instanceof IOException) {
        final IOException e = (IOException)_t;
        ErlLogger.warn("Could not create event trace log file: %s", name);
        this.file = null;
      } else {
        throw Exceptions.sneakyThrow(_t);
      }
    }
    event.print(this.file);
  }
  
  protected void _handle(final ErlideEvent event) {
    event.print(this.file);
  }
  
  public void dispose() {
    boolean _tripleNotEquals = (this.file != null);
    if (_tripleNotEquals) {
      this.file.flush();
      try {
        this.file.close();
      } catch (final Throwable _t) {
        if (_t instanceof IOException) {
          final IOException e = (IOException)_t;
          e.printStackTrace();
        } else {
          throw Exceptions.sneakyThrow(_t);
        }
      }
    }
  }
  
  public void handle(final ErlideEvent event) {
    if (event instanceof ErlideSessionEvent) {
      _handle((ErlideSessionEvent)event);
      return;
    } else if (event != null) {
      _handle(event);
      return;
    } else {
      throw new IllegalArgumentException("Unhandled parameter types: " +
        Arrays.<Object>asList(event).toString());
    }
  }
}
