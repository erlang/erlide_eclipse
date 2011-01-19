/**
 *
 */
package org.erlide.core.erlang.internal;

import org.erlide.core.erlang.IErlComment;
import org.erlide.core.erlang.IParent;
import org.erlide.core.erlang.ISourceRange;

/**
 * @author jakob
 * 
 */
public class ErlComment extends SourceRefElement implements IErlComment {

    private final boolean fIsEdoc;
    private final boolean fIsHeader;

    public ErlComment(final IParent parent, final String name,
            final boolean isEdoc, final boolean isHeader) {
        super(parent, name);
        fIsEdoc = isEdoc;
        fIsHeader = isHeader;
    }

    public boolean isEdoc() {
        return fIsEdoc;
    }

    public boolean isHeader() {
        return fIsHeader;
    }

    /**
     * @see org.erlide.core.erlang.IErlElement#getKind()
     */
    public Kind getKind() {
        return Kind.COMMENT;
    }

    public boolean isVisibleInOutline() {
        return false;
    }

    @Override
    public String toString() {
        String result = "<comment";
        if (isEdoc()) {
            result = result + ":edoc";
        }
        if (isHeader()) {
            result = result + ":header";
        }
        return result + ", line=" + (getLineStart() + 1) + ">";
    }

    public String getHoverHelp() {
        return null;
    }

    public ISourceRange getNameRange() {
        return null;
    }

    public void setNameRange(final int offset, final int length) {
    }
}
