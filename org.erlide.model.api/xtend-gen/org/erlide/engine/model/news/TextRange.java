package org.erlide.engine.model.news;

import org.eclipse.xtend.lib.annotations.Data;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;

@Data
@SuppressWarnings("all")
public class TextRange {
  private final int offset;
  
  private final int length;
  
  public TextRange(final int offset, final int length) {
    super();
    this.offset = offset;
    this.length = length;
  }
  
  @Override
  @Pure
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + this.offset;
    result = prime * result + this.length;
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
    TextRange other = (TextRange) obj;
    if (other.offset != this.offset)
      return false;
    if (other.length != this.length)
      return false;
    return true;
  }
  
  @Override
  @Pure
  public String toString() {
    ToStringBuilder b = new ToStringBuilder(this);
    b.add("offset", this.offset);
    b.add("length", this.length);
    return b.toString();
  }
  
  @Pure
  public int getOffset() {
    return this.offset;
  }
  
  @Pure
  public int getLength() {
    return this.length;
  }
}
