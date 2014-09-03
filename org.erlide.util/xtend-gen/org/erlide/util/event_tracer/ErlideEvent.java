package org.erlide.util.event_tracer;

import java.io.PrintWriter;
import org.eclipse.xtend.lib.annotations.Data;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;

@Data
@SuppressWarnings("all")
public abstract class ErlideEvent {
  private final long timestamp;
  
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
    this.timestamp = timestamp;
  }
  
  @Override
  @Pure
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + (int) (this.timestamp ^ (this.timestamp >>> 32));
    return result;
  }
  
  @Override
  @Pure
  public boolean equals(final Object obj) {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (getClass() != obj.getClass())
      return false;
    ErlideEvent other = (ErlideEvent) obj;
    if (other.timestamp != this.timestamp)
      return false;
    return true;
  }
  
  @Override
  @Pure
  public String toString() {
    ToStringBuilder b = new ToStringBuilder(this);
    b.add("timestamp", this.timestamp);
    return b.toString();
  }
  
  @Pure
  public long getTimestamp() {
    return this.timestamp;
  }
}
