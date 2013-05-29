package org.erlide.util;

import java.net.Inet4Address;
import java.net.InetAddress;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.erlide.util.ErlideEvent;
import org.erlide.util.IDisposable;
import org.osgi.service.event.Event;
import org.osgi.service.event.EventHandler;

@SuppressWarnings("all")
public abstract class ErlideEventTracerHandler implements EventHandler, IDisposable {
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
  
  protected String workspace = new Function0<String>() {
    public String apply() {
      IWorkspace _workspace = ResourcesPlugin.getWorkspace();
      IWorkspaceRoot _root = _workspace.getRoot();
      IPath _location = _root.getLocation();
      String _portableString = _location.toPortableString();
      return _portableString;
    }
  }.apply();
  
  public void handleEvent(final Event event) {
    Object _property = event.getProperty("event");
    final ErlideEvent myEvent = ((ErlideEvent) _property);
    this.handle(myEvent);
  }
  
  public abstract void handle(final ErlideEvent event);
}
