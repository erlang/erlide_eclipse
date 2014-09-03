package org.erlide.engine.model.builder;

import org.eclipse.xtend.lib.annotations.Data;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;

@Data
@SuppressWarnings("all")
public class ProblemData0 {
  private final String tag;
  
  private final String message;
  
  private final int arity;
  
  public ProblemData0(final String tag, final String message, final int arity) {
    super();
    this.tag = tag;
    this.message = message;
    this.arity = arity;
  }
  
  @Override
  @Pure
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((this.tag== null) ? 0 : this.tag.hashCode());
    result = prime * result + ((this.message== null) ? 0 : this.message.hashCode());
    result = prime * result + this.arity;
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
    ProblemData0 other = (ProblemData0) obj;
    if (this.tag == null) {
      if (other.tag != null)
        return false;
    } else if (!this.tag.equals(other.tag))
      return false;
    if (this.message == null) {
      if (other.message != null)
        return false;
    } else if (!this.message.equals(other.message))
      return false;
    if (other.arity != this.arity)
      return false;
    return true;
  }
  
  @Override
  @Pure
  public String toString() {
    ToStringBuilder b = new ToStringBuilder(this);
    b.add("tag", this.tag);
    b.add("message", this.message);
    b.add("arity", this.arity);
    return b.toString();
  }
  
  @Pure
  public String getTag() {
    return this.tag;
  }
  
  @Pure
  public String getMessage() {
    return this.message;
  }
  
  @Pure
  public int getArity() {
    return this.arity;
  }
}
