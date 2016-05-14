package org.erlide.engine.services.codeassist;

import java.util.List;
import org.eclipse.xtend.lib.annotations.Data;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;
import org.erlide.engine.services.codeassist.CompletionData;
import org.erlide.engine.services.codeassist.Location;

@Data
@SuppressWarnings("all")
public class FunctionCompletionData extends CompletionData {
  private final List<Location> offsetsAndLengths;
  
  private final String additionalProposalInfo;
  
  public FunctionCompletionData(final List<Location> offsetsAndLengths, final String displayString, final String replacementString, final int replacementOffset, final int replacementLength, final int cursorPosition, final String additionalProposalInfo) {
    super(displayString, replacementString, replacementOffset, replacementLength, cursorPosition);
    this.offsetsAndLengths = offsetsAndLengths;
    this.additionalProposalInfo = additionalProposalInfo;
  }
  
  @Override
  @Pure
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + ((this.offsetsAndLengths== null) ? 0 : this.offsetsAndLengths.hashCode());
    result = prime * result + ((this.additionalProposalInfo== null) ? 0 : this.additionalProposalInfo.hashCode());
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
    if (!super.equals(obj))
      return false;
    FunctionCompletionData other = (FunctionCompletionData) obj;
    if (this.offsetsAndLengths == null) {
      if (other.offsetsAndLengths != null)
        return false;
    } else if (!this.offsetsAndLengths.equals(other.offsetsAndLengths))
      return false;
    if (this.additionalProposalInfo == null) {
      if (other.additionalProposalInfo != null)
        return false;
    } else if (!this.additionalProposalInfo.equals(other.additionalProposalInfo))
      return false;
    return true;
  }
  
  @Override
  @Pure
  public String toString() {
    String result = new ToStringBuilder(this)
    	.addAllFields()
    	.toString();
    return result;
  }
  
  @Pure
  public List<Location> getOffsetsAndLengths() {
    return this.offsetsAndLengths;
  }
  
  @Pure
  public String getAdditionalProposalInfo() {
    return this.additionalProposalInfo;
  }
}
