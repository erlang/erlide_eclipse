/**
 *
 */
package org.erlide.engine.internal.model.erlang;

import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.IParent;
import org.erlide.engine.model.erlang.IErlComment;
import org.erlide.engine.model.erlang.ISourceRange;
import org.erlide.engine.model.root.ErlElementKind;

/**
 * @author jakob
 *
 */
public class ErlComment extends ErlMember implements IErlComment {

    private final boolean fIsHeader;

    public ErlComment(final IParent parent, final String name, final boolean isHeader) {
        super(parent, name);
        fIsHeader = isHeader;
    }

    @Override
    public boolean isHeader() {
        return fIsHeader;
    }

    /**
     * @see org.erlide.engine.model.root.IErlElement#getKind()
     */
    @Override
    public ErlElementKind getKind() {
        return ErlElementKind.COMMENT;
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
