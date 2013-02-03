/**
 *
 */
package org.erlide.core.internal.model.erlang;

import org.erlide.core.internal.model.root.ErlMember;
import org.erlide.core.model.ErlModelException;
import org.erlide.core.model.IParent;
import org.erlide.core.model.erlang.IErlComment;
import org.erlide.core.model.erlang.ISourceRange;

/**
 * @author jakob
 * 
 */
public class ErlComment extends ErlMember implements IErlComment {

    private final boolean fIsHeader;

    public ErlComment(final IParent parent, final String name,
            final boolean isHeader) {
        super(parent, name);
        fIsHeader = isHeader;
    }

    @Override
    public boolean isHeader() {
        return fIsHeader;
    }

    /**
     * @see org.erlide.core.model.root.IErlElement#getKind()
     */
    @Override
    public Kind getKind() {
        return Kind.COMMENT;
    }

    @Override
    public String toString() {
        String result = "<comment";
        if (isHeader()) {
            result = result + ":header";
        }
        return result + ", line=" + (getLineStart() + 1) + ">";
    }

    @Override
    public ISourceRange getNameRange() {
        return null;
    }

    @Override
    public void setNameRange(final int offset, final int length) {
    }

    @Override
    public String getSource() throws ErlModelException {
        return getName();
    }
}
