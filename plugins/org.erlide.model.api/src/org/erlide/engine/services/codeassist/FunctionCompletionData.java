package org.erlide.engine.services.codeassist;

import java.util.List;
import java.util.Objects;

import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;

@SuppressWarnings("all")
public class FunctionCompletionData extends CompletionData {
    private final List<Location> offsetsAndLengths;

    private final String additionalProposalInfo;

    public FunctionCompletionData(final List<Location> offsetsAndLengths,
            final String displayString, final String replacementString,
            final int replacementOffset, final int replacementLength,
            final int cursorPosition, final String additionalProposalInfo) {
        super(displayString, replacementString, replacementOffset, replacementLength,
                cursorPosition);
        this.offsetsAndLengths = offsetsAndLengths;
        this.additionalProposalInfo = additionalProposalInfo;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result
                + (offsetsAndLengths == null ? 0 : offsetsAndLengths.hashCode());
        return prime * result + (additionalProposalInfo == null ? 0
                : additionalProposalInfo.hashCode());
    }

    @Override
    public boolean equals(final Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        if (!super.equals(obj)) {
            return false;
        }
        final FunctionCompletionData other = (FunctionCompletionData) obj;
        if (!Objects.equals(offsetsAndLengths, other.offsetsAndLengths)) {
            return false;
        }
        if (!Objects.equals(additionalProposalInfo, other.additionalProposalInfo)) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return new ToStringBuilder(this).addAllFields().toString();
    }

    public List<Location> getOffsetsAndLengths() {
        return offsetsAndLengths;
    }

    public String getAdditionalProposalInfo() {
        return additionalProposalInfo;
    }
}
