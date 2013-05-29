package org.erlide.util;

import java.net.Inet4Address;
import java.net.InetAddress;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.xtend.lib.Data;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.eclipse.xtext.xbase.lib.util.ToStringHelper;

@Data
@SuppressWarnings("all")
public abstract class ErlideEvent {
  private final String _user = new Function0<String>() {
    public String apply() {
      String _property = System.getProperty("user.name");
      return _property;
    }
  }.apply();
  
  public String getUser() {
    return this._user;
  }
  
  private final String _machine = new Function0<String>() {
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
  
  public String getMachine() {
    return this._machine;
  }
  
  private final String _workspace = new Function0<String>() {
    public String apply() {
      IWorkspace _workspace = ResourcesPlugin.getWorkspace();
      IWorkspaceRoot _root = _workspace.getRoot();
      IPath _location = _root.getLocation();
      String _portableString = _location.toPortableString();
      return _portableString;
    }
  }.apply();
  
  public String getWorkspace() {
    return this._workspace;
  }
  
  private final long _timestamp;
  
  public long getTimestamp() {
    return this._timestamp;
  }
  
  public ErlideEvent(final long timestamp) {
    super();
    this._timestamp = timestamp;
  }
  
  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((_user== null) ? 0 : _user.hashCode());
    result = prime * result + ((_machine== null) ? 0 : _machine.hashCode());
    result = prime * result + ((_workspace== null) ? 0 : _workspace.hashCode());
    result = prime * result + (int) (_timestamp ^ (_timestamp >>> 32));
    return result;
  }
  
  @Override
  public boolean equals(final Object obj) {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (getClass() != obj.getClass())
      return false;
    ErlideEvent other = (ErlideEvent) obj;
    if (_user == null) {
      if (other._user != null)
        return false;
    } else if (!_user.equals(other._user))
      return false;
    if (_machine == null) {
      if (other._machine != null)
        return false;
    } else if (!_machine.equals(other._machine))
      return false;
    if (_workspace == null) {
      if (other._workspace != null)
        return false;
    } else if (!_workspace.equals(other._workspace))
      return false;
    if (other._timestamp != _timestamp)
      return false;
    return true;
  }
  
  @Override
  public String toString() {
    String result = new ToStringHelper().toString(this);
    return result;
  }
}
