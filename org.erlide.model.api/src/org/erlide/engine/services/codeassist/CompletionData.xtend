package org.erlide.engine.services.codeassist

import java.util.List
import org.eclipse.xtend.lib.annotations.Data

@Data
class CompletionData {
    String displayString
    String replacementString
    int replacementOffset
    int replacementLength
    int cursorPosition

    def String getDisplayString() {
        if(displayString === null) replacementString else displayString
    }

}

@Data
class Location {
    int offset
    int length
}

@Data
class FunctionCompletionData extends CompletionData {

    List<Location> offsetsAndLengths
    String additionalProposalInfo

    new(List<Location> offsetsAndLengths, String displayString, String replacementString, int replacementOffset,
        int replacementLength, int cursorPosition,
        String additionalProposalInfo) {
            super(displayString, replacementString, replacementOffset, replacementLength, cursorPosition)
            this.offsetsAndLengths = offsetsAndLengths
            this.additionalProposalInfo = additionalProposalInfo
        }
    }
