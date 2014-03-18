package org.erlide.engine.model.builder;

import org.eclipse.xtend.lib.Data;
import org.eclipse.xtext.xbase.lib.util.ToStringHelper;

@Data
@SuppressWarnings("all")
public class ProblemData {
  private final String _category;
  
  public String getCategory() {
    return this._category;
  }
  
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
  
  public ProblemData(final String category, final String tag, final String message, final int arity) {
    super();
    this._category = category;
    this._tag = tag;
    this._message = message;
    this._arity = arity;
  }
  
  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((_category== null) ? 0 : _category.hashCode());
    result = prime * result + ((_tag== null) ? 0 : _tag.hashCode());
    result = prime * result + ((_message== null) ? 0 : _message.hashCode());
    result = prime * result + _arity;
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
    ProblemData other = (ProblemData) obj;
    if (_category == null) {
      if (other._category != null)
        return false;
    } else if (!_category.equals(other._category))
      return false;
    if (_tag == null) {
      if (other._tag != null)
        return false;
    } else if (!_tag.equals(other._tag))
      return false;
    if (_message == null) {
      if (other._message != null)
        return false;
    } else if (!_message.equals(other._message))
      return false;
    if (other._arity != _arity)
      return false;
    return true;
  }
  
  @Override
  public String toString() {
    String result = new ToStringHelper().toString(this);
    return result;
  }
}
