package org.erlide.engine.services.codeassist;

import org.eclipse.xtend.lib.annotations.Data;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;

@Data
@SuppressWarnings("all")
public class CompletionData {
  private final String displayString;
  
  private final String replacementString;
  
  private final int replacementOffset;
  
  private final int replacementLength;
  
  private final int cursorPosition;
  
  public String getDisplayString() {
    String _xifexpression = null;
    if ((this.displayString == null)) {
      _xifexpression = this.replacementString;
    } else {
      _xifexpression = this.displayString;
    }
    return _xifexpression;
  }
  
  public CompletionData(final String displayString, final String replacementString, final int replacementOffset, final int replacementLength, final int cursorPosition) {
    super();
    this.displayString = displayString;
    this.replacementString = replacementString;
    this.replacementOffset = replacementOffset;
    this.replacementLength = replacementLength;
    this.cursorPosition = cursorPosition;
  }
  
  @Override
  @Pure
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((this.displayString== null) ? 0 : this.displayString.hashCode());
    result = prime * result + ((this.replacementString== null) ? 0 : this.replacementString.hashCode());
    result = prime * result + this.replacementOffset;
    result = prime * result + this.replacementLength;
    result = prime * result + this.cursorPosition;
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
    CompletionData other = (CompletionData) obj;
    if (this.displayString == null) {
      if (other.displayString != null)
        return false;
    } else if (!this.displayString.equals(other.displayString))
      return false;
    if (this.replacementString == null) {
      if (other.replacementString != null)
        return false;
    } else if (!this.replacementString.equals(other.replacementString))
      return false;
    if (other.replacementOffset != this.replacementOffset)
      return false;
    if (other.replacementLength != this.replacementLength)
      return false;
    if (other.cursorPosition != this.cursorPosition)
      return false;
    return true;
  }
  
  @Override
  @Pure
  public String toString() {
    ToStringBuilder b = new ToStringBuilder(this);
    b.add("displayString", this.displayString);
    b.add("replacementString", this.replacementString);
    b.add("replacementOffset", this.replacementOffset);
    b.add("replacementLength", this.replacementLength);
    b.add("cursorPosition", this.cursorPosition);
    return b.toString();
  }
  
  @Pure
  public String getReplacementString() {
    return this.replacementString;
  }
  
  @Pure
  public int getReplacementOffset() {
    return this.replacementOffset;
  }
  
  @Pure
  public int getReplacementLength() {
    return this.replacementLength;
  }
  
  @Pure
  public int getCursorPosition() {
    return this.cursorPosition;
  }
}
