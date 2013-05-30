package org.erlide.util.event_tracer;

import java.net.Inet4Address;
import java.net.InetAddress;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.erlide.util.IDisposable;
import org.erlide.util.event_tracer.ErlideEvent;

@SuppressWarnings("all")
public abstract class ErlideEventTracerHandler implements IDisposable {
  protected String user = new Function0<String>() {
    public String apply() {
      String _property = System.getProperty("user.name");
      return _property;
    }
  }.apply();
  
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
  
  public abstract void handle(final ErlideEvent event);
}
