package org.erlide.util.event_tracer;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.erlide.util.ErlLogger;
import org.erlide.util.event_tracer.ErlideEvent;
import org.erlide.util.event_tracer.ErlideEventTracerHandler;
import org.erlide.util.event_tracer.ErlideSessionEvent;

@SuppressWarnings("all")
public class FileEventTracer extends ErlideEventTracerHandler {
  private final IPath storagePath;
  
  private PrintWriter file;
  
  private final SimpleDateFormat formatter = new Function0<SimpleDateFormat>() {
    public SimpleDateFormat apply() {
      SimpleDateFormat _simpleDateFormat = new SimpleDateFormat("yyyyMMdd-HHmmss-SSS");
      return _simpleDateFormat;
    }
  }.apply();
  
  public FileEventTracer(final String path) {
    Path _path = new Path(path);
    IPath _append = _path.append(this.machine);
    IPath _append_1 = _append.append(this.user);
    this.storagePath = _append_1;
    String _portableString = this.storagePath.toPortableString();
    File _file = new File(_portableString);
    _file.mkdirs();
  }
  
  protected void _handle(final ErlideSessionEvent event) {
    long _timestamp = event.getTimestamp();
    Date _date = new Date(_timestamp);
    final Date date = _date;
    final String sdate = this.formatter.format(date);
    String _hexString = Integer.toHexString(event.workspace);
    IPath _append = this.storagePath.append(_hexString);
    String _plus = (sdate + ".log");
    IPath _append_1 = _append.append(_plus);
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
      try {
        this.file.flush();
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
