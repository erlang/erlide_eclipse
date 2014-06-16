package org.erlide.engine.model.builder;

import org.eclipse.xtend.lib.Data;
import org.eclipse.xtext.xbase.lib.util.ToStringHelper;

@Data
@SuppressWarnings("all")
public class ProblemData0 {
  private final String _tag;
  
  public String getTag() {
    return this._tag;
  }
  
  private final String _message;
  
  public String getMessage() {
    return this._message;
  }
  
  private final int _arity;
  
  public int getArity() {
    return this._arity;
  }
  
  public ProblemData0(final String tag, final String message, final int arity) {
    super();
    this._tag = tag;
    this._message = message;
    this._arity = arity;
  }
  
  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((this._tag== null) ? 0 : this._tag.hashCode());
    result = prime * result + ((this._message== null) ? 0 : this._message.hashCode());
    result = prime * result + this._arity;
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
    ProblemData0 other = (ProblemData0) obj;
    if (this._tag == null) {
      if (other._tag != null)
        return false;
    } else if (!this._tag.equals(other._tag))
      return false;
    if (this._message == null) {
      if (other._message != null)
        return false;
    } else if (!this._message.equals(other._message))
      return false;
    if (other._arity != this._arity)
      return false;
    return true;
  }
  
  @Override
  public String toString() {
    String result = new ToStringHelper().toString(this);
    return result;
  }
}
