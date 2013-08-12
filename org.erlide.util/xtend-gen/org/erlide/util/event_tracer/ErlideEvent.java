package org.erlide.util.event_tracer;

import java.io.PrintWriter;
import org.eclipse.xtend.lib.Data;
import org.eclipse.xtext.xbase.lib.util.ToStringHelper;

@Data
@SuppressWarnings("all")
public abstract class ErlideEvent {
  private final long _timestamp;
  
  public long getTimestamp() {
    return this._timestamp;
  }
  
  public void print(final PrintWriter file) {
    boolean _tripleNotEquals = (file != null);
    if (_tripleNotEquals) {
      String _print = this.print();
      file.print(_print);
    }
  }
  
  public abstract String print();
  
  public ErlideEvent(final long timestamp) {
    super();
    this._timestamp = timestamp;
  }
  
  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
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
