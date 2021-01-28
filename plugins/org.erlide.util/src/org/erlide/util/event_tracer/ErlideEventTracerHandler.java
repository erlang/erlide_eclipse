package org.erlide.util.event_tracer;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.InetAddress;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.erlide.util.ErlLogger;
import org.erlide.util.IDisposable;

@SuppressWarnings("all")
public class ErlideEventTracerHandler implements IDisposable {
    protected String user = System.getProperty("user.name");

    protected String machine = null;

    private final IPath storagePath;

    private PrintWriter file;

    private final SimpleDateFormat formatter = new SimpleDateFormat(
            "yyyyMMdd-HHmmss-SSS");

    public ErlideEventTracerHandler(final String path) {
        if (path == null) {
            storagePath = null;
            return;
        }
        storagePath = new Path(path).append(machine).append(user);
        final String _portableString = storagePath.toPortableString();
        new File(_portableString).mkdirs();
    }

    protected void _handle(final ErlideSessionEvent event) {
        try {
            if (machine == null) {
                machine = InetAddress.getLocalHost().getCanonicalHostName();
            }
            if (storagePath == null) {
                return;
            }
            final long _timestamp = event.getTimestamp();
            final Date date = new Date(_timestamp);
            final String sdate = formatter.format(date);
            final String name = storagePath.append(Integer.toHexString(event.workspace))
                    .append(sdate + ".log").toPortableString();
            try {
                final FileWriter _fileWriter = new FileWriter(name, false);
                final BufferedWriter _bufferedWriter = new BufferedWriter(_fileWriter);
                final PrintWriter _printWriter = new PrintWriter(_bufferedWriter);
                file = _printWriter;
            } catch (final Throwable _t) {
                if (_t instanceof IOException) {
                    ErlLogger.warn("Could not create event trace log file: %s", name);
                    file = null;
                } else {
                    throw Exceptions.sneakyThrow(_t);
                }
            }
            event.print(file);
        } catch (final Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
        }
    }

    protected void _handle(final ErlideEvent event) {
        event.print(file);
    }

    @Override
    public void dispose() {
        if (file != null) {
            file.flush();
            try {
                file.close();
            } catch (final Throwable _t) {
                if (_t instanceof IOException) {
                    final IOException e = (IOException) _t;
                    e.printStackTrace();
                } else {
                    throw Exceptions.sneakyThrow(_t);
                }
            }
        }
    }

    public void handle(final ErlideEvent event) {
        if (event instanceof ErlideSessionEvent) {
            _handle((ErlideSessionEvent) event);
        } else if (event != null) {
            _handle(event);
        } else {
            throw new IllegalArgumentException("Unhandled parameter types: "
                    + Arrays.<Object> asList(event).toString());
        }
    }
}
